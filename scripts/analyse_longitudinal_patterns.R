
# ---
# Authors: Camille Minaudo
# Project: "SALBIA"
# date: "April 2026"

# ---

# As saltwater intrudes upstream, you'd expect:
#
# TSS proxy (red, SWIR) — saltwater is clearer than turbid freshwater in the
# Gambia, so a drop in red/SWIR moving downstream marks the fresh/saline
# transition

# CDOM proxy (blue/green ratio, blue/red) —  CDOM is freshwater-sourced; you'd
# expect a downstream increase in blue ratios where CDOM drops off in the saline
# wedge

# Algae/chlorophyll (green, NIR/red) — the halocline can drive productivity
# gradients; look for spatial peaks near the front

# Blue/red or blue/green ratio — probably the best single proxy; saltwater has
# higher blue reflectance relative to red than turbid fresh water

# ---


rm(list = ls()) # clear workspace
cat("/014") # clear console

# ---- packages ----
library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(grid)
library(egg)
require(dplyr)
library(sf)
library(data.table)
library(mgcv)

scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
path_repo_root <- dirname(scriptpath) # path to the cloned repo
datapath <- file.path(path_repo_root,"data/gee")
plotpath <- file.path(path_repo_root,"plots")



# ── load data ──────────────────────────────────────────────────────────
# Satellite harmonised reflectance — one row per date × seg_id
setwd(datapath)
dt_aligned <- fread("salbia_reflectance_aligned_normalized.csv")
dt_aligned[, date := as.Date(date)]



# ── settings ───────────────────────────────────────────────────────────
counts_repetitive <- table(sort(dt_aligned$date))
minimum_coverage  <- 300
well_covered      <- as.Date(names(counts_repetitive[counts_repetitive > minimum_coverage]))

dist_min   <- 50
dist_max   <- 300
min_pts    <- 30
min_r2_gam <- 0.3
k_gam      <- 20
tol        <- 0

# ── define metrics to analyse ──────────────────────────────────────────
bands       <- c("blue", "green", "red", "nir", "swir1", "swir2")
ratio_pairs <- CJ(b1 = bands, b2 = bands)[b1 != b2]
ratio_names <- paste0(ratio_pairs$b1, "_", ratio_pairs$b2)
all_metrics <- c(bands, ratio_names)

# Use the zscore versions for longitudinal pattern analysis —
# these are already aligned across sensors and normalised per date
zscore_metrics <- paste0(all_metrics, "_zscore")

# Keep only those that actually exist in dt_aligned
zscore_metrics <- zscore_metrics[zscore_metrics %in% names(dt_aligned)]
cat("Metrics to analyse:", length(zscore_metrics), "\n")


# ── pre-filter: restrict to search window once ─────────────────────────
dt_window <- dt_aligned[
  d_from_basse > dist_min &
    d_from_basse < dist_max &
    date %in% well_covered
]
cat("Rows in search window:", nrow(dt_window), "\n")


# ── helper: fit GAM and extract metrics for one variable ───────────────
fit_gam_profile <- function(data_d, metric, d, k = k_gam) {
  
  # Drop non-finite values for this metric
  col_data <- data_d[[metric]]
  ok       <- is.finite(col_data)
  
  if (sum(ok) < min_pts) return(list(peak = NULL, modeled = NULL))
  
  sub <- data_d[ok]
  
  mod <- tryCatch(
    gam(as.formula(paste0(metric, " ~ s(d_from_basse, k = ", k, ")")),
        data   = sub,
        method = "REML"),
    error = function(e) NULL
  )
  if (is.null(mod)) return(list(peak = NULL, modeled = NULL))
  
  dev_expl <- summary(mod)$dev.expl
  if (dev_expl < min_r2_gam) return(list(peak = NULL, modeled = NULL))
  
  k_check <- tryCatch(k.check(mod), error = function(e) NULL)
  k_ok    <- if (!is.null(k_check)) all(k_check[, "p-value"] > 0.05) else NA
  edf     <- sum(mod$edf)
  
  # Prediction grid
  newd <- data.frame(
    d_from_basse = seq(min(sub$d_from_basse),
                       max(sub$d_from_basse),
                       by = 0.5)
  )
  pred_se      <- predict(mod, newd, se.fit = TRUE)
  newd$pred    <- pred_se$fit
  newd$pred_se <- pred_se$se.fit
  newd$pred_lo <- newd$pred - 1.96 * newd$pred_se
  newd$pred_hi <- newd$pred + 1.96 * newd$pred_se
  newd$date    <- d
  newd$metric  <- metric
  newd$slope   <- c(NA, diff(newd$pred) / diff(newd$d_from_basse))
  
  # Peak
  peak_row  <- newd[which.max(newd$pred), ]
  peak_dist <- peak_row$d_from_basse
  peak_pred <- peak_row$pred
  
  edge_buffer  <- 5
  peak_at_edge <- peak_dist <= (dist_min + edge_buffer) |
    peak_dist >= (dist_max - edge_buffer)
  pred_range   <- max(newd$pred) - min(newd$pred)
  is_monotonic <- pred_range < 0.5
  
  # FWHM
  halfmax    <- (max(newd$pred) + min(newd$pred)) / 2
  above_half <- newd$d_from_basse[newd$pred >= halfmax]
  if (length(above_half) >= 2) {
    FWHM       <- diff(range(above_half))
    FWHM_valid <- TRUE
  } else {
    FWHM       <- NA_real_
    FWHM_valid <- FALSE
  }
  
  # Slope metrics
  slopes_before <- newd$slope[newd$d_from_basse < peak_dist & newd$slope > 0]
  slopes_after  <- newd$slope[newd$d_from_basse > peak_dist & newd$slope < 0]
  
  peak_row_out <- data.frame(
    date         = d,
    metric       = metric,
    n_pts        = sum(ok),
    n_sensors    = uniqueN(sub$sensor),
    dev_expl     = round(dev_expl, 3),
    edf          = round(edf, 1),
    k_adequate   = k_ok,
    peak_pred    = peak_pred,
    dist_peak    = peak_dist,
    peak_at_edge = peak_at_edge,
    is_monotonic = is_monotonic,
    FWHM         = FWHM,
    FWHM_valid   = FWHM_valid,
    avg_rise     = if (length(slopes_before) > 0) mean(slopes_before) else NA_real_,
    max_rise     = if (length(slopes_before) > 0) max(slopes_before)  else NA_real_,
    avg_decline  = if (length(slopes_after)  > 0) mean(slopes_after)  else NA_real_,
    max_decline  = if (length(slopes_after)  > 0) min(slopes_after)   else NA_real_
  )
  
  list(peak = peak_row_out, modeled = as.data.table(newd))
}


# ── main loop: dates × metrics ─────────────────────────────────────────
# Pre-split by date once to avoid repeated subsetting inside the loop
dt_by_date <- split(dt_window, dt_window$date)

n_dates   <- length(well_covered)
n_metrics <- length(zscore_metrics)
cat("Processing", n_dates, "dates ×", n_metrics, "metrics =",
    n_dates * n_metrics, "GAMs\n")

peak_list    <- vector("list", n_dates * n_metrics)
modeled_list <- vector("list", n_dates * n_metrics)
idx <- 0L

for (i in seq_along(well_covered)) {
  d      <- as.Date(well_covered[i])
  d_char <- as.character(d)
  
  if (!d_char %in% names(dt_by_date)) next
  data_d <- dt_by_date[[d_char]]
  
  if (i %% 50 == 0)
    cat("Processing date", i, "/", n_dates, ":", as.character(d), "\n")
  
  for (m in zscore_metrics) {
    idx <- idx + 1L
    res <- fit_gam_profile(data_d, m, d)
    peak_list[[idx]]    <- res$peak
    modeled_list[[idx]] <- res$modeled
  }
}

# ── assemble ───────────────────────────────────────────────────────────
df_peaks      <- rbindlist(peak_list,    fill = TRUE)
modeled_all   <- rbindlist(modeled_list, fill = TRUE)

cat("Total GAM fits retained:", nrow(df_peaks), "\n")


# ── quality summary per metric ─────────────────────────────────────────
quality_summary <- df_peaks[, .(
  n_dates_fit    = .N,
  n_edge         = sum(peak_at_edge,  na.rm = TRUE),
  n_monotonic    = sum(is_monotonic,  na.rm = TRUE),
  n_k_inadequate = sum(!k_adequate,   na.rm = TRUE),
  median_dev     = round(median(dev_expl, na.rm = TRUE), 3),
  median_edf     = round(median(edf,      na.rm = TRUE), 1)
), by = metric][order(-median_dev)]

print(quality_summary)


# ── dist_peak summary: which metrics show consistent front position? ───
peak_consistency <- df_peaks[
  peak_at_edge == FALSE & is_monotonic == FALSE,
  .(
    n           = .N,
    mean_dist   = round(mean(dist_peak,   na.rm = TRUE), 1),
    sd_dist     = round(sd(dist_peak,     na.rm = TRUE), 1),
    median_FWHM = round(median(FWHM,      na.rm = TRUE), 1)
  ),
  by = metric
][order(sd_dist)]

cat("\nMetrics with most consistent front position (low sd_dist):\n")
print(peak_consistency)


# ── edf distribution plot ──────────────────────────────────────────────
ggplot(df_peaks, aes(edf)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = k_gam - 1, linetype = "dashed", color = "tomato") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "GAM effective df per metric",
       x = "edf", y = "count") +
  theme_bw(base_size = 7)


# ── export ─────────────────────────────────────────────────────────────
fwrite(df_peaks,    file.path(datapath, "longitudinal_peaks_allmetrics.csv"))
fwrite(modeled_all, file.path(datapath, "longitudinal_gam_profiles_allmetrics.csv"))
cat("Saved peak metrics and GAM profiles for all metrics\n")




setwd(datapath)
df_peaks <- fread("longitudinal_peaks_allmetrics.csv")
df_peaks[, date := as.Date(date)]



# Pre-compute derived columns ONCE on the full dataframe
df_peaks <- df_peaks |>
  mutate(
    doy    = lubridate::yday(date),
    month  = lubridate::month(date, label = TRUE),
    year   = lubridate::year(date),
    decade = as.factor(paste0(floor(year / 10) * 10, "s"))
  )

df_peaks_split <- split(df_peaks, df_peaks$metric)

metrics <- unique(df_peaks$metric)


df_peaks_green <- df_peaks_split[["green_zscore"]]

ggplot(df_peaks_green, aes(doy, dist_peak))+geom_point()+theme_article()

ggplot(df_peaks_green[df_peaks_green$dist_peak>100 & df_peaks_green$dist_peak < 250 & df_peaks_green$year>215,], aes(date, dist_peak))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_article()+facet_wrap(month~.)








# Filter to reliable fits and candidate metrics
front_proxies <- c("green_zscore", "blue_zscore", "red_zscore", 
                   "red_blue_zscore", "red_green_zscore", "green_blue_zscore")
df_hov <- df_peaks[
  peak_at_edge == FALSE & metric %in% front_proxies
]

ggplot(df_hov, aes(x = date, y = dist_peak)) +
  geom_point(aes(colour = peak_pred), size = 0.4, alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.15, 
              colour = "white", linewidth = 0.7, se = FALSE) +
  scale_colour_viridis_c(option = "A", name = "Peak\nzscore") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  facet_wrap(~ metric, ncol = 2) +
  labs(
    title    = "GAM peak location over time — saline front proxy",
    subtitle = "Distance of spectral peak from Basse; filtered to non-edge peaks",
    x        = NULL,
    y        = "Distance from Basse [km]"
  ) +
  theme_article() +
  theme(panel.spacing = unit(1, "lines"))


ggplot(df_hov, aes(x = as.numeric(month), y = dist_peak)) +
  geom_jitter(aes(colour = decade), width = 0.2, size = 0.3, alpha = 0.2) +
  geom_smooth(aes(group = decade, colour = decade),
              method = "loess", span = 0.5, linewidth = 0.9, se = FALSE) +
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  facet_wrap(~ metric, ncol = 2) +
  labs(
    title = "Seasonal cycle of front position by decade",
    x     = NULL,
    y     = "Distance from Basse [km]",
    colour = "Decade"
  ) +
  theme_article()




# Wider FWHM = more diffuse/gradual front — interesting secondary signal
ggplot(df_hov, aes(x = date, y = FWHM)) +
  geom_point(aes(colour = month), size = 0.4, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.2,
              colour = "white", linewidth = 0.7, se = FALSE) +
  scale_colour_viridis_d(option = "D") +
  facet_wrap(~ metric, ncol = 2) +
  labs(
    title = "Peak width (FWHM) over time — front sharpness proxy",
    y     = "FWHM [km]", x = NULL
  ) +
  theme_article()


# Correlate dist_peak across metrics for the same date
# If metrics are coherent proxies, their dist_peak should correlate
df_wide <- dcast(
  df_hov[, .(date, metric, dist_peak)],
  date ~ metric, value.var = "dist_peak"
)

# Quick pairs plot
library(GGally)
ggpairs(df_wide[, -1], 
        upper = list(continuous = wrap("cor", size = 2.5)),
        lower = list(continuous = wrap("points", alpha = 0.1, size = 0.3)),
        diag  = list(continuous = wrap("densityDiag"))) +
  theme_article()









# ── 1. Compute seasonal climatology and anomaly ──────────────────────────────

df_anom <- df_peaks[peak_at_edge == FALSE & metric %in% front_proxies] |>
  
  # Monthly climatology per metric (mean + sd for normalisation)
  _[, `:=`(
    clim_mean = mean(dist_peak, na.rm = TRUE),
    clim_sd   = sd(dist_peak,   na.rm = TRUE)
  ), by = .(metric, month)] |>
  
  # Standardised anomaly (z-score flavour) and raw anomaly
  _[, `:=`(
    anom_raw  = dist_peak - clim_mean,
    anom_std  = (dist_peak - clim_mean) / clim_sd
  )] |>
  
  # Annual mean anomaly for trend line
  _[, anom_annual := mean(anom_raw, na.rm = TRUE), by = .(metric, year)]


# ── 2. Hovmöller-style anomaly plot ─────────────────────────────────────────

p_anom <- ggplot(df_anom, aes(x = date, y = anom_raw)) +
  
  # Zero reference
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
  
  # Raw anomaly points
  geom_point(aes(colour = anom_raw), size = 0.3, alpha = 0.4) +
  
  # Smooth interannual trend
  geom_smooth(
    method    = "loess",
    formula   = y ~ x,
    span      = 0.3,          # wider span = more interannual, less monthly noise
    colour    = "white",
    fill      = "grey30",
    linewidth = 0.9,
    alpha     = 0.3,
    se        = TRUE
  ) +
  
  scale_colour_gradient2(
    low      = "#2166ac",   # downstream (saltwater intrusion retreating)
    mid      = "grey90",
    high     = "#d6604d",   # upstream (saltwater pushing further in)
    midpoint = 0,
    name     = "Anomaly\n[km]"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  
  facet_wrap(~ metric, ncol = 2) +
  labs(
    title    = "Interannual anomaly of front position",
    subtitle = "Deseasonalised: monthly climatology subtracted per metric | positive = front further upstream",
    x        = NULL,
    y        = "Distance anomaly from climatology [km]"
  ) +
  theme_article() +
  theme(panel.spacing = unit(1, "lines"))


# ── 3. Annual mean anomaly — cleaner trend signal ────────────────────────────

df_annual <- df_anom[, .(
  anom_annual = mean(anom_raw, na.rm = TRUE),
  anom_sd     = sd(anom_raw,   na.rm = TRUE),
  n           = .N
), by = .(metric, year)][
  n >= 3  # drop years with very few observations
]

p_annual <- ggplot(df_annual, aes(x = year, y = anom_annual)) +
  
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
  
  # Uncertainty ribbon (±1 sd)
  geom_ribbon(
    aes(ymin = anom_annual - anom_sd,
        ymax = anom_annual + anom_sd),
    fill  = "grey40",
    alpha = 0.25
  ) +
  
  # Annual means
  geom_point(aes(colour = anom_annual, size = n), alpha = 0.8) +
  
  # Trend line
  geom_smooth(
    method    = "loess",
    formula   = y ~ x,
    span      = 0.5,
    colour    = "white",
    linewidth = 0.9,
    se        = FALSE
  ) +
  
  scale_colour_gradient2(
    low      = "#2166ac",
    mid      = "grey90",
    high     = "#d6604d",
    midpoint = 0,
    name     = "Anomaly\n[km]",
    guide    = "none"   # already encoded in y-axis
  ) +
  scale_size_continuous(range = c(1, 4), name = "n obs") +
  scale_x_continuous(breaks = seq(1984, 2026, 5)) +
  
  facet_wrap(~ metric, ncol = 2) +
  labs(
    title    = "Annual mean front position anomaly",
    subtitle = "±1 SD ribbon | point size = number of observations | positive = front further upstream",
    x        = NULL,
    y        = "Annual mean anomaly [km]"
  ) +
  theme_article() +
  theme(panel.spacing = unit(1, "lines"))


# ── 4. Save both ─────────────────────────────────────────────────────────────

# ggsave(file.path(plotpath, "front_anomaly_timeseries.jpg"),
#        p_anom,   width = 12, height = 10)
# 
# ggsave(file.path(plotpath, "front_anomaly_annual.jpg"),
#        p_annual, width = 12, height = 10)













modeled_all <- fread("longitudinal_gam_profiles_allmetrics.csv")
modeled_all[, date := as.Date(date)]

modeled_all$decade <- as.factor(paste(floor(modeled_all$year/10)*10, "s",sep = ""))


# my metric
mymetric = "green_zscore"
myname =   "Green\nzscore"

# ── 1. Filter to green_blue and aggregate onto a 2D grid ────────────────────

df_grid <- modeled_all[
  metric == mymetric
][,
  dist_bin := round(d_from_basse / 5) * 5
][,
  `:=`(
    month_grp  = data.table::month(date),   # explicit namespace avoids clash
    decade_grp = as.integer(format(date, "%Y")) %/% 10 * 10
  )
][,
  .(
    mean_pred = mean(pred, na.rm = TRUE),
    n         = .N
  ),
  by = .(decade_grp, month_grp, dist_bin)
][
  n >= 3
]


df_grid$dist_ocean <- -(df_grid$dist_bin-414.63) # Basse is 414.63 km away from the ocean (defined as seg_id = 831)


# ── 2. Heatmaps ───────────────────────────────────────────────────────────────
# Aggregate across decades for the non-faceted plot
df_grid_all <- df_grid[,
                       .(mean_pred = mean(mean_pred, na.rm = TRUE),
                         n         = sum(n)),
                       by = .(dist_ocean, month_grp)
][n >= 3]

p_heatmap <- ggplot(df_grid_all, aes(x = dist_ocean, y = month_grp)) +
  
  geom_tile(aes(fill = mean_pred), colour = NA) +
  
  # Contour: inherit only z, not fill
  geom_contour(
    aes(z = mean_pred),        # no fill here
    colour    = "white",
    linewidth = 0.35,
    alpha     = 0.9,
    breaks    = c(-1, -0.5, 0, 0.5, 1)
  ) +
  
  scale_fill_gradient2(
    low      = "#1d4e89",
    mid      = "#f7f7f7",
    high     = "#386641",
    midpoint = 0,
    limits   = c(-1.5, 1.5),
    oob      = scales::squish,
    name     = myname,
    na.value = "grey15"
  ) +
  
  scale_x_continuous(
    breaks    = seq(50, 300, 50),
    expand    = c(0, 0),
    transform = "reverse"
  ) +
  
  scale_y_continuous(
    breaks = 1:12,
    labels = month.abb,
    trans  = "reverse"         # Jan at top
  ) +
  
  labs(
    title    = "Green band — GAM-modelled longitudinal profiles",
    subtitle = "Monthly × distance profiles",
    x        = "Distance from the ocean [km]",
    y        = NULL,
    caption  = "Mean of GAM-predicted zscores; 5 km distance bins"
  ) +
  
  theme_article() +
  theme(
    strip.text        = element_text(face = "bold", size = 10),
    axis.text.x       = element_text(size = 7),
    axis.text.y       = element_text(size = 7),
    legend.key.height = unit(2.5, "lines"),
    plot.caption      = element_text(colour = "grey50", size = 7)
  )



p_heatmap_decade <- p_heatmap + df_grid +   # swap data back to full df_grid
  geom_text(aes(label = ifelse(n < 10, "·", ""))) +
  facet_wrap(~decade_grp, ncol = 2)


ggsave(
  file.path(plotpath, paste0("heatmap_gam_",mymetric,".jpg")),
  p_heatmap,
  width  = 8,
  height = 4,
  dpi    = 300
)


ggsave(
  file.path(plotpath, paste0("heatmap_gam_decade_",mymetric,".jpg")),
  p_heatmap_decade,
  width  = 12,
  height = 10,
  dpi    = 300
)
















plot_monthly_patterns <- function(modelled, metric) {
  
  ggplot(modelled, aes(d_from_basse, pred)) +
    
    # Individual lines — muted, thin, in the background
    geom_line(
      aes(group = date, colour = decade),
      linewidth = 0.15,
      alpha     = 0.5
    ) +
    
    # Confidence ribbon — subtle fill matching the overall tone
    geom_smooth(
      method    = "loess",
      formula   = y ~ x,
      colour    = "#000080",        # clean line pops over the ribbon
      fill      = "#2d2d2d",      # dark neutral ribbon (adjust to your theme)
      linewidth = 0.9,
      alpha     = 0.35,
      se        = F
    ) +
    
    scale_colour_viridis_d(
      option    = "C",
      begin     = 0.1,
      end       = 0.9,
      direction = 1,
      guide     = guide_legend(
        override.aes = list(alpha = 1, linewidth = 0.8)  # legend lines fully visible
      )
    ) +
    
    labs(
      title    = paste0("Modelled ", metric, " longitudinal profile"),
      subtitle = "Monthly patterns with LOESS trend",
      x        = "Distance from Basse [km]",
      y        = metric,
      colour   = "Decade"
    ) +
    
    ylim(c(-3,3))+
    
    theme_article() +
    theme(
      strip.text       = element_text(face = "bold", size = 9),
      panel.spacing    = unit(0.8, "lines"),   # breathe between facets
      legend.key.width = unit(1.5, "lines")    # longer swatch in legend
    ) +
    
    facet_wrap(~ month, nrow = 3)  # 3 rows × 4 cols is tidy for 12 months
}


# Pre-compute derived columns ONCE on the full dataframe
modeled_all <- modeled_all |>
  mutate(
    month  = lubridate::month(date, label = TRUE),
    year   = lubridate::year(date),
    decade = as.factor(paste0(floor(year / 10) * 10, "s"))
  )

modeled_split <- split(modeled_all, modeled_all$metric)


library(parallel)
library(pbapply)  # optional: adds a progress bar

metrics <- unique(df_peaks$metric)

pblapply(metrics, function(metric) {
  mymod <- modeled_split[[metric]]
  
  p <- plot_monthly_patterns(modelled = mymod, metric = metric)
  
  ggsave(
    filename = file.path(plotpath, paste0("longitudinal_patterns_", metric, ".jpg")),
    plot     = p,
    width    = 8,
    height   = 5
  )
  cat("Saved:", metric, "\n")
}, cl = max(1, detectCores() - 1))




