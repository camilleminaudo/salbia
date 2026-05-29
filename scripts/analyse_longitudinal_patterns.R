
# ---
# Authors: Camille Minaudo
# Project: "SALBIA"
# date: "April 2026"

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























































# ── coverage filter ────────────────────────────────────────────────────
counts_repetitive <- table(sort(dt_aligned$date))
minimum_coverage  <- 300
well_covered      <- as.Date(names(counts_repetitive[counts_repetitive > minimum_coverage]))

# search window for the saline front
dist_min   <- 50
dist_max   <- 300
min_pts    <- 30
min_r2_gam <- 0.3   # minimum GAM deviance explained to trust the fit
k_gam      <- 20
tol        <- 0     # date tolerance in days

# ── main loop ──────────────────────────────────────────────────────────
peak_list    <- vector("list", length(well_covered))
data_list    <- vector("list", length(well_covered))
modeled_list <- vector("list", length(well_covered))

for (i in seq_along(well_covered)) {
  d <- as.Date(well_covered[i])
  
  data_d <- dt_aligned[date >= d - tol/2 & date <= d + tol/2]
  data_d <- data_d[blue > 0 & is.finite(rb_zscore_date)]
  
  data_d_restrict <- data_d[d_from_basse > dist_min & d_from_basse < dist_max]
  
  # ── basic validity checks ──────────────────────────────────────────
  n_pts     <- nrow(data_d_restrict)
  n_sensors <- uniqueN(data_d_restrict$sensor)
  
  if (n_pts < min_pts) {
    message("Skipping ", d, ": only ", n_pts, " points in search window")
    next
  }
  
  # ── fit GAM with convergence guard ────────────────────────────────
  mod <- tryCatch(
    gam(rb_zscore_date ~ s(d_from_basse, k = k_gam),
        data    = data_d_restrict,
        method  = "REML"),
    error = function(e) {
      message("GAM failed on ", d, ": ", e$message)
      NULL
    }
  )
  if (is.null(mod)) next
  
  # GAM fit quality
  dev_expl <- summary(mod)$dev.expl   # deviance explained (analogous to R²)
  k_check  <- k.check(mod)            # check if k is large enough
  k_ok     <- all(k_check[, "p-value"] > 0.05)
  edf <- sum(mod$edf)   # total effective df used
  
  if (dev_expl < min_r2_gam) {
    message("Skipping ", d, ": GAM deviance explained = ",
            round(dev_expl, 3), " < ", min_r2_gam)
    next
  }
  
  # ── predict over fine grid ─────────────────────────────────────────
  newd <- data.frame(
    d_from_basse = seq(min(data_d_restrict$d_from_basse),
                       max(data_d_restrict$d_from_basse),
                       by = 0.5)   # 500 m resolution
  )
  pred_se      <- predict(mod, newd, se.fit = TRUE)
  newd$pred    <- pred_se$fit
  newd$pred_se <- pred_se$se.fit
  newd$pred_lo <- newd$pred - 1.96 * newd$pred_se
  newd$pred_hi <- newd$pred + 1.96 * newd$pred_se
  newd$date    <- d
  
  # ── slope along profile ────────────────────────────────────────────
  newd$slope <- c(NA, diff(newd$pred) / diff(newd$d_from_basse))
  
  # ── peak detection ─────────────────────────────────────────────────
  peak_row  <- newd[which.max(newd$pred), ]
  peak_dist <- peak_row$d_from_basse
  peak_pred <- peak_row$pred
  
  # Flag if peak is at the edge of the search window (unreliable)
  edge_buffer <- 5   # km
  peak_at_edge <- peak_dist <= (dist_min + edge_buffer) |
    peak_dist >= (dist_max - edge_buffer)
  
  # Flag if profile is monotonic (no real peak — front outside window)
  pred_range   <- max(newd$pred) - min(newd$pred)
  is_monotonic <- pred_range < 0.5   # less than 0.5 z-score units of variation
  
  # ── FWHM — guard against profiles that never cross halfmax ─────────
  halfmax      <- (max(newd$pred) + min(newd$pred)) / 2
  above_half   <- newd$d_from_basse[newd$pred >= halfmax]
  
  if (length(above_half) >= 2) {
    FWHM       <- diff(range(above_half))
    FWHM_valid <- TRUE
  } else {
    FWHM       <- NA_real_
    FWHM_valid <- FALSE
  }
  
  # ── slope metrics ──────────────────────────────────────────────────
  # Upstream of peak: rising slope (fresh → saline transition)
  # Downstream of peak: declining slope (saline → marine transition)
  slopes_before <- newd$slope[newd$d_from_basse < peak_dist & newd$slope > 0]
  slopes_after  <- newd$slope[newd$d_from_basse > peak_dist & newd$slope < 0]
  
  avg_rise    <- if (length(slopes_before) > 0) mean(slopes_before) else NA_real_
  avg_decline <- if (length(slopes_after)  > 0) mean(slopes_after)  else NA_real_
  
  # Max slope on each side — steepness of the front
  max_rise    <- if (length(slopes_before) > 0) max(slopes_before)  else NA_real_
  max_decline <- if (length(slopes_after)  > 0) min(slopes_after)   else NA_real_
  
  # ── store results ──────────────────────────────────────────────────
  peak_list[[i]] <- data.frame(
    date          = d,
    n_pts         = n_pts,
    n_sensors     = n_sensors,
    dev_expl      = round(dev_expl, 3),
    edf           = round(edf, 1),
    k_used        = k_gam,
    k_adequate    = k_ok,
    peak_pred     = peak_pred,
    dist_peak     = peak_dist,
    peak_at_edge  = peak_at_edge,
    is_monotonic  = is_monotonic,
    FWHM          = FWHM,
    FWHM_valid    = FWHM_valid,
    avg_rise      = avg_rise,
    max_rise      = max_rise,
    avg_decline   = avg_decline,
    max_decline   = max_decline
  )
  
  data_list[[i]]    <- data_d
  modeled_list[[i]] <- newd
}


# ── assemble results ───────────────────────────────────────────────────
df_Rpeak       <- rbindlist(peak_list,    fill = TRUE)
data_d.all     <- rbindlist(data_list,    fill = TRUE)
modeled.red2blue <- rbindlist(modeled_list, fill = TRUE)

# ── quality flags summary ──────────────────────────────────────────────
cat("Total dates processed:       ", nrow(df_Rpeak), "\n")
cat("Dates with edge peak:        ", sum(df_Rpeak$peak_at_edge,  na.rm=TRUE), "\n")
cat("Dates flagged as monotonic:  ", sum(df_Rpeak$is_monotonic,  na.rm=TRUE), "\n")
cat("Dates with invalid FWHM:     ", sum(!df_Rpeak$FWHM_valid,   na.rm=TRUE), "\n")
cat("Dates with k inadequate:     ", sum(!df_Rpeak$k_adequate,   na.rm=TRUE), "\n")


hist(df_Rpeak$edf, breaks = 30,
     main = "Distribution of GAM effective df across dates",
     xlab = "edf")








df_Rpeak$doy <-  as.numeric(strftime(df_Rpeak$date, format = "%j"))
df_Rpeak$month <- month(df_Rpeak$date)
df_Rpeak$year <- year(df_Rpeak$date)
df_Rpeak$decade <-  as.factor(paste(floor(df_Rpeak$year/10)*10, "s",sep = ""))

df_Rpeak$season <- "dry"
df_Rpeak$season[which(df_Rpeak$month>5 & df_Rpeak$month < 11)] <- "wet"


ggplot(df_Rpeak, aes(doy, dist_peak))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "loess", se = F)+
  xlab("Day of the year")+
  ylab("Peak position as distance from Basse [km]")+
  geom_point(aes(colour = decade))+theme_article()+ylim(c(0,NA))+geom_hline(yintercept = c(100, 200))


ggplot(df_Rpeak, aes(doy, FWHM))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "loess", se = F)+
  geom_point(aes(colour = decade))+theme_article()+ylim(c(0,NA))+geom_hline(yintercept = c(100, 200))


ggplot(df_Rpeak, aes(doy, avg_decline))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "loess", se = F)+
  geom_point(aes(colour = decade))+theme_article()+
  ylim(c(-.2,NA))


ggplot(df_Rpeak, aes(year, dist_peak))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "lm", se = F)+
  geom_point()+theme_article()+facet_wrap(month~.)




data_d.all$doy <-  as.numeric(strftime(data_d.all$date, format = "%j"))
data_d.all$month <- month(data_d.all$date)
data_d.all$year <- year(data_d.all$date)
data_d.all$decade <-  as.factor(paste(floor(data_d.all$year/10)*10, "s",sep = ""))


data_d.all[[paste0("red_blue","_flag")]] <- qualityCheck(y = data_d.all[["red_blue"]])
data_d.all_qaqced <- data_d.all[data_d.all$red_blue_flag ==0,]


ggplot(data_d.all_qaqced[data_d.all_qaqced$sensor!="SENTINEL_2",], 
       aes(d_from_basse, red_blue))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = sensor), linewidth=0.2, alpha=0.2)+
  # geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("Distance from Basse [km]")+
  # ylab("Red / Blue reflectance")+
  theme_article()+facet_wrap(month~.) #+  ggtitle("Red to Blue ratio")

unique(data_d.all$date[which(data_d.all$red_blue>2)])




modeled.red2blue$doy <-  as.numeric(strftime(modeled.red2blue$date, format = "%j"))
modeled.red2blue$month <- month(modeled.red2blue$date)
modeled.red2blue$year <- year(modeled.red2blue$date)
modeled.red2blue$decade <-  as.factor(paste(floor(modeled.red2blue$year/10)*10, "s",sep = ""))

summary(modeled.red2blue$pred)



ggplot(modeled.red2blue, aes(d_from_basse, pred))+
  # geom_abline(slope = 0, intercept = 0)+
  # geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  # geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("Distance from Basse [km]")+
  ylab("Red / Blue z-score reflectance")+
  theme_article()+facet_wrap(month~.)+
  ylim(c(-5,5))+
  ggtitle("Modelled Red to Blue ratio")

