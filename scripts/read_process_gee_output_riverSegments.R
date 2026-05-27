# ---
# Authors: Camille Minaudo
# Project: "SALBIA"
# date: "April 2024"

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

# some basic QaQc
qualityCheck <- function(y){
  # outlier detection based on inter-quartile range
  Q_0.25 <- as.numeric(quantile(y, probs = c(0.25), na.rm = T))
  Q_0.75 <- as.numeric(quantile(y, probs = c(0.75), na.rm = T))
  IQR <- Q_0.75-Q_0.25
  
  delta <- 3
  flag_outlier_low <- y < Q_0.25 - delta * IQR
  flag_outlier_upp <- y > Q_0.75 + delta * IQR
  
  flag_negative <- y < 0
  
  flags <- 0*y
  flags[flag_outlier_low] <- 1
  flags[flag_outlier_upp] <- 1
  flags[flag_negative] <- 2
  
  return(flags)
}

myPalette <- c("#66c2a5", "#fc8d62", "#483D8B")


scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
path_repo_root <- dirname(scriptpath) # path to the cloned repo
datapath <- file.path(path_repo_root,"data/gee")
plotpath <- file.path(path_repo_root,"plots")


shp_path <- "C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/GIS/basse2ocean_buffer200m.shp"
river <- st_read(shp_path)
cat("Loaded", nrow(river), "segments\n")


setwd(datapath)

dataS2 <- read.csv("data/gee/S2_Gambia_river_segmts.csv")
dataS2$date <- as.Date(dataS2$date)
subdataS2 <- dataS2[,c("date",
                    "seg_id",
                    "img_id",
                    "blue_count",
                    "blue_mean",
                    "green_mean",
                    "red_mean",
                    "nir_mean",
                    "swir1_mean",
                    "swir2_mean",
                    "blue_stdDev",
                    "green_stdDev",
                    "red_stdDev",
                    "nir_stdDev",
                    "swir1_stdDev",
                    "swir2_stdDev",
                    "satellite")]
ind_match <- match(subdataS2$seg_id, river$seg_id)
subdataS2$d_from_ocean <- (river$from_ocean[ind_match] + river$from_oce_1[ind_match])/2
subdataS2$d_from_basse <- abs(subdataS2$d_from_ocean-max(subdataS2$d_from_ocean))


dataLS <- read.csv("data/gee/Landsat_Gambia_river_segmts.csv")
dataLS$date <- as.Date(dataLS$date)
subdataLS <- dataLS[,c("date",
                       "seg_id",
                       "img_id",
                       "blue_count",
                       "blue_mean",
                       "green_mean",
                       "red_mean",
                       "nir_mean",
                       "swir1_mean",
                       "swir2_mean",
                       "blue_stdDev",
                       "green_stdDev",
                       "red_stdDev",
                       "nir_stdDev",
                       "swir1_stdDev",
                       "swir2_stdDev",
                       "satellite")]
ind_match <- match(subdataLS$seg_id, river$seg_id)
subdataLS$d_from_ocean <- (river$from_ocean[ind_match] + river$from_oce_1[ind_match])/2
subdataLS$d_from_basse <- abs(subdataLS$d_from_ocean-max(subdataLS$d_from_ocean))



# -------------------- QA QC ------------------------------------


for (band in c("blue_mean",
               "green_mean",
               "red_mean",
               "nir_mean",
               "swir1_mean",
               "swir2_mean")){
  message(band)
  subdataS2[[paste0(band,"_flag")]] <- qualityCheck(y = subdataS2[[band]])
  subdataLS[[paste0(band,"_flag")]] <- qualityCheck(y = subdataLS[[band]])
}

subdataS2$flag_all <- subdataS2$blue_mean_flag+ subdataS2$green_mean_flag+subdataS2$nir_mean_flag+subdataS2$red_mean_flag+subdataS2$swir1_mean_flag+subdataS2$swir2_mean_flag
subdataLS$flag_all <- subdataLS$blue_mean_flag+ subdataLS$green_mean_flag+subdataLS$nir_mean_flag+subdataLS$red_mean_flag+subdataLS$swir1_mean_flag+subdataLS$swir2_mean_flag


# Merge LS and S2 data into a single df
data_raw <- rbind(subdataS2, subdataLS)


# -------------------- PLOT Profiles for selected dates ------------------------------------

dmin = "2024-07-31"
dmax = "2024-08-06"

ggplot(data_raw[data_raw$date >= dmin & data_raw$date <= dmax,], 
       aes(seg_id, red_mean, colour = satellite))+
  geom_point()+
  xlab("Segment ID: Basse = 0, Ocean = 800")+
  theme_article()+facet_wrap(date~.)




# -------------------- PLOT BASIC INFO ABOUT DATABASE ------------------------------------

subdataS2$zone <- "upper"
subdataS2$zone[subdataS2$seg_id>200] <- "central"
subdataS2$zone[subdataS2$seg_id>=700] <- "lower"
subdataS2$zone <- factor(subdataS2$zone, levels = c("upper","central","lower"))



subdataS2$date <- as.Date(subdataS2$date)
subdataS2$date_dec <- decimal_date(subdataS2$date)
subdataS2$year <- year(subdataS2$date)
subdataS2$decade <-  as.factor(paste(floor(subdataS2$year/10)*10, "s",sep = ""))
subdataS2$month <- month(subdataS2$date)
subdataS2$doy <-  as.numeric(strftime(subdataS2$date, format = "%j"))



p_year <- ggplot(subdataS2, aes(year))+
  geom_histogram(stat="count", aes(fill = zone))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "top")

p_sats <- ggplot(subdataS2, aes(year))+
  geom_histogram(stat="count", aes(fill = satellite))+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  theme_article()+theme(legend.position = "top")



ggplot(subdataS2, aes(blue_count))+
  geom_density()+
  theme_article()+theme(legend.position = "top")



p_month <- ggplot(subdataS2, aes(month))+
  geom_histogram(stat="count", aes(fill = zone))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "none")+scale_x_continuous(breaks = seq(1,12), labels = seq(1,12))


p_distance <- ggplot(subdataS2, aes(d_from_basse))+
  geom_histogram(stat="count", aes(fill = zone))+
  xlab("Distance from Basse [km]")+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "none")






# -------------------- HARMONIZE SENSORS ------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)

# ── 1. FILTER AND SETUP ────────────────────────────────────────────────
# Convert to data.table once at the start — all operations below are in-place
dt <- as.data.table(data_raw)
dt <- dt[flag_all == 0]

# Normalise sensor label once, upfront, to avoid repeating str_detect later
dt[, sensor := fcase(
  str_detect(satellite, "^Sentinel-2"), "SENTINEL_2",
  default = satellite
)]


# ── 2. FIND DATE×SEGMENT PAIRS COVERED BY MORE THAN ONE SENSOR ────────
# This replaces the slow for-loop + rbind pattern
dt[, pair_key := paste0(date, "-", seg_id)]

pair_counts <- dt[, .(n_sensors = uniqueN(sensor)), by = pair_key]
valid_pairs <- pair_counts[n_sensors > 1, pair_key]

dt_overlap <- dt[pair_key %in% valid_pairs]
cat("Rows with multi-sensor overlap:", nrow(dt_overlap), "\n")


# ── 3. WIDE FORMAT FOR CROSS-SENSOR COMPARISON ────────────────────────
# Average within sensor per date×segment×band before pivoting
bands <- c("blue_mean","green_mean","red_mean","nir_mean","swir1_mean","swir2_mean")

dt_long_overlap <- melt(
  dt_overlap,
  id.vars       = c("date", "d_from_basse", "sensor"),
  measure.vars  = bands,
  variable.name = "band",
  value.name    = "value"
)

# Clean band names: remove "_mean" suffix
dt_long_overlap[, band := str_remove(band, "_mean")]

# Average in case of duplicates within same sensor×date×segment×band
dt_long_overlap <- dt_long_overlap[,
                                   .(value = mean(value, na.rm = TRUE)),
                                   by = .(date, d_from_basse, sensor, band)
]

# Split into reference (S2) and others
dt_ref <- dt_long_overlap[sensor == "SENTINEL_2",
                          .(date, d_from_basse, band, ref_value = value)
]

dt_compare <- dt_long_overlap[sensor != "SENTINEL_2"] |>
  merge(dt_ref, by = c("date", "d_from_basse", "band"), all.x = TRUE)

cat("Rows in cross-sensor comparison table:", nrow(dt_compare), "\n")


# ── 4. DIAGNOSTIC PLOT ─────────────────────────────────────────────────
ggplot(dt_compare,
       aes(ref_value, value)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_grid(band ~ sensor, scales = "free") +
  labs(x = "Sentinel-2", y = "Other sensor") +
  theme_bw()


# ── 5. FIT CROSS-CALIBRATION MODELS ───────────────────────────────────
# One linear model per band × sensor: value ~ ref_value (S2)
lm_table <- dt_compare %>%
  group_by(band, sensor) %>%
  group_modify(~ {
    mod        <- lm(value ~ ref_value, data = .x)
    tidy_mod   <- tidy(mod)
    glance_mod <- glance(mod)
    tibble(
      intercept = tidy_mod$estimate[tidy_mod$term == "(Intercept)"],
      slope     = tidy_mod$estimate[tidy_mod$term == "ref_value"],
      p_value   = tidy_mod$p.value[tidy_mod$term == "ref_value"],
      r_squared = glance_mod$r.squared,
      n         = glance_mod$nobs
    )
  }) %>%
  ungroup()

print(lm_table)

# Summary statistics
dt_compare %>%
  group_by(sensor, band) %>%
  summarise(
    r    = cor(ref_value, value, use = "complete.obs"),
    bias = mean(value - ref_value, na.rm = TRUE),
    rmse = sqrt(mean((value - ref_value)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>% print()


# ── 6. APPLY HARMONISATION TO FULL DATASET ────────────────────────────
# Landsat 5 has no contemporaneous S2 overlap so it borrows L7's model.
# L7, L8, L9 each get their own model fitted above.
sensor_model_map <- data.table(
  sensor           = c("LANDSAT_5","LANDSAT_7","LANDSAT_8","LANDSAT_9","SENTINEL_2"),
  sensor_for_model = c("LANDSAT_7","LANDSAT_7","LANDSAT_8","LANDSAT_9","SENTINEL_2")
)

# Melt full dataset to long format — do this once
dt_long_full <- melt(
  dt,
  id.vars      = c("date","d_from_basse","seg_id","img_id","sensor","satellite"),
  measure.vars = c("blue_mean","green_mean","red_mean",
                   "nir_mean","swir1_mean","swir2_mean"),
  variable.name = "band",
  value.name    = "value"
)
dt_long_full[, band := str_remove(band, "_mean")]

# Join sensor model map
dt_long_full <- merge(dt_long_full, sensor_model_map, by = "sensor", all.x = TRUE)

# Join calibration coefficients
lm_dt <- as.data.table(lm_table)
dt_long_full <- merge(
  dt_long_full,
  lm_dt[, .(sensor_for_model = sensor, band, intercept, slope)],
  by = c("sensor_for_model", "band"),
  all.x = TRUE
)

# Apply correction — S2 keeps its original value, others get corrected
dt_long_full[, value_harmonized := fcase(
  sensor == "SENTINEL_2", value,
  !is.na(slope),          intercept + slope * value,
  default = NA_real_
)]


# ── 7. DIAGNOSTIC PLOT POST-HARMONISATION ─────────────────────────────
ggplot(dt_long_full[band == "blue"],
       aes(value, value_harmonized, color = sensor)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(title = "Blue band: raw vs harmonised")


# ── 8. PIVOT BACK TO WIDE AND EXPORT ──────────────────────────────────
dt_final <- dcast(
  dt_long_full,
  date + d_from_basse + seg_id + img_id + sensor ~ band,
  value.var = "value_harmonized"
)

setwd(datapath)
fwrite(dt_final, "salbia_reflectance_harmonized.csv")
cat("Saved", nrow(dt_final), "rows to salbia_reflectance_harmonized.csv\n")






# -------------------- ALIGN and NORMALIZE red/blue ------------------------------------


dt_final[, rb_ratio := fifelse(blue > 0.001, red / blue, NA_real_)]


# ── helper: fit lm on overlap and return corrected full profile ─────────
align_to_reference <- function(df_ref, df_other, min_overlap = 3, min_r2 = 0.6) {
  
  overlap <- merge(
    df_ref[,   .(seg_id, ref_value   = rb_ratio)],
    df_other[, .(seg_id, other_value = rb_ratio)],
    by = "seg_id"
  )
  overlap <- overlap[is.finite(ref_value) & is.finite(other_value)]
  
  # Not enough overlap to fit a meaningful model
  if (nrow(overlap) < min_overlap) {
    message("  Skipping: only ", nrow(overlap), " valid overlapping segments",
            " — keeping reference sensor only")
    df_other[, rb_aligned := NA_real_]
    return(df_other)
  }
  
  mod <- lm(other_value ~ ref_value, data = overlap)
  a   <- coef(mod)[1]
  b   <- coef(mod)[2]
  r2  <- summary(mod)$r.squared
  message("  n_overlap=", nrow(overlap),
          "  intercept=", round(a, 4),
          "  slope=",     round(b, 4),
          "  R²=",        round(r2, 3))
  
  # Poor model fit — discard the other sensor for this date
  if (r2 < min_r2) {
    message("  R² below threshold (", round(r2, 3), " < ", min_r2, ")",
            " — discarding ", df_other$sensor[1], " for this date")
    df_other[, rb_aligned := NA_real_]
    return(df_other)
  }
  
  # Good fit — apply correction
  df_other[, rb_aligned := fifelse(
    is.finite(rb_ratio),
    (rb_ratio - a) / b,
    NA_real_
  )]
  return(df_other)
}


# ── main loop: one date at a time ──────────────────────────────────────
dates <- unique(dt_final$date)
result_list <- vector("list", length(dates))

for (i in seq_along(dates)) {
  d <- dates[i]
  dt_date <- dt_final[date == d]
  
  sensor_counts <- dt_date[, .(n = .N), by = sensor][order(-n)]
  
  if (nrow(sensor_counts) == 1) {
    dt_date[, rb_aligned := rb_ratio]
    result_list[[i]] <- dt_date
    next
  }
  
  ref_sensor <- sensor_counts$sensor[1]
  message("Date: ", d, " | Reference sensor: ", ref_sensor,
          " (", sensor_counts$n[1], " obs)")
  
  df_ref <- dt_date[sensor == ref_sensor]
  df_ref[, rb_aligned := rb_ratio]
  
  other_sensors <- sensor_counts$sensor[-1]
  aligned_others <- lapply(other_sensors, function(s) {
    message(" Aligning: ", s)
    align_to_reference(df_ref, dt_date[sensor == s])
  })
  
  # Combine and drop sensors that failed the R² threshold
  # (their rb_aligned is all NA — remove them entirely rather than
  # keeping empty rows that would distort the z-score)
  all_parts <- rbindlist(c(list(df_ref), aligned_others),
                         use.names = TRUE, fill = TRUE)
  result_list[[i]] <- all_parts[!is.na(rb_aligned)]
}

# After building dt_aligned, summarise how many date×sensor pairs
# survived vs were discarded
dt_aligned[, .(
  n_dates       = uniqueN(date),
  n_obs         = .N,
  n_na_aligned  = sum(is.na(rb_aligned))
), by = sensor]

dt_aligned <- rbindlist(result_list, use.names = TRUE, fill = TRUE)


# ── z-score on the aligned profiles ────────────────────────────────────
# Now z-score within date × sensor, but on rb_aligned instead of rb_ratio
# Since sensors are now on the same scale, you could also z-score
# within date only (collapsing across sensors) — try both
dt_aligned[, rb_zscore_sensor := {
  x <- rb_aligned
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}, by = .(date, sensor)]

# Z-score across all sensors for a given date (more aggressive alignment)
dt_aligned[, rb_zscore_date := {
  x <- rb_aligned
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}, by = date]


# ── diagnostic: check alignment quality ────────────────────────────────
# Plot raw vs aligned for dates with multi-sensor coverage
sample_dates <- dt_aligned[, .(n_sensors = uniqueN(sensor)),
                           by = date][n_sensors > 1, date] |> head(6)

dt_plot <- melt(
  dt_aligned[date %in% sample_dates],
  id.vars      = c("date", "sensor", "d_from_basse"),
  measure.vars = c("rb_ratio", "rb_aligned", "rb_zscore_date"),
  variable.name = "method",
  value.name    = "value"
)

ggplot(dt_plot, aes(d_from_basse, value, color = sensor)) +
  geom_line(alpha = 0.8, linewidth = 0.6) +
  facet_grid(method ~ date, scales = "free_y") +
  labs(
    x     = "Distance from upstream (km)",
    y     = "Red/Blue ratio",
    color = "Sensor"
  ) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 7))



setwd(datapath)
fwrite(dt_aligned, "salbia_reflectance_aligned_normalized.csv")
cat("Saved", nrow(dt_aligned), "rows to salbia_reflectance_aligned_normalized.csv\n")






# -------------------- LONGITUDINAL PATTERNS ------------------------------------


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

hist(df_Rpeak$edf, breaks = 30,
     main = "Distribution of GAM effective df across dates",
     xlab = "edf")

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



ggplot(df_Rpeak, aes(date, avg_decline))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "loess", se = F)+
  geom_point(aes(colour = decade))+theme_article()


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

modeled.red2blue[[paste0("flag")]] <- qualityCheck(y = modeled.red2blue[["pred"]])
modeled.red2blue_qaqced <- modeled.red2blue[modeled.red2blue$flag ==0,]
summary(modeled.red2blue_qaqced$pred)


ggplot(modeled.red2blue_qaqced, aes(d_from_basse, pred))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("Distance from Basse [km]")+
  ylab("Red / Blue reflectance")+
  theme_article()+facet_wrap(month~.)+
  # ylim(c(0,2))+
  ggtitle("Red to Blue ratio")


