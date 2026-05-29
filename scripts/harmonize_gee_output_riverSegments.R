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


scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
path_repo_root <- dirname(scriptpath) # path to the cloned repo
datapath <- file.path(path_repo_root,"data/gee")
plotpath <- file.path(path_repo_root,"plots")


shp_path <- "C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/GIS/basse2ocean_buffer200m.shp"
river <- st_read(shp_path)
cat("Loaded", nrow(river), "segments\n")


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

setwd(datapath)
dt_final <- fread("salbia_reflectance_harmonized.csv")
dt_final[, date := as.Date(date)]



library(data.table)

# ── 1. COMPUTE ALL METRICS UPFRONT ─────────────────────────────────────
bands <- c("blue", "green", "red", "nir", "swir1", "swir2")

ratio_pairs <- CJ(b1 = bands, b2 = bands)[b1 != b2]
ratio_names <- paste0(ratio_pairs$b1, "_", ratio_pairs$b2)

for (i in seq_len(nrow(ratio_pairs))) {
  b1 <- ratio_pairs$b1[i]
  b2 <- ratio_pairs$b2[i]
  cn <- ratio_names[i]
  dt_final[, (cn) := fifelse(get(b2) > 0.001, get(b1) / get(b2), NA_real_)]
}

all_metrics <- c(bands, ratio_names)
cat("Total metrics to align:", length(all_metrics), "\n")


# ── 2. PER-BAND COEFFICIENT FUNCTION ───────────────────────────────────
# For one date × sensor pair, fits one lm per metric on overlapping
# segments. Returns a named list of coefficients (a, b, r2) per metric,
# or NULL for metrics that fail the r2 threshold.
# Overlap is always defined spatially (same seg_id seen by both sensors).

get_perband_coeffs <- function(df_ref, df_other,
                               metrics,
                               min_overlap = 3,
                               min_r2      = 0.6) {
  
  # Merge on seg_id once — bring all metrics from both sensors
  ref_cols   <- c("seg_id", metrics)
  other_cols <- c("seg_id", metrics)
  
  # Only keep finite rows to avoid lm complaints
  overlap <- merge(
    df_ref[,   .SD, .SDcols = ref_cols],
    df_other[, .SD, .SDcols = other_cols],
    by     = "seg_id",
    suffixes = c("_ref", "_other")
  )
  
  if (nrow(overlap) < min_overlap) {
    message("  Skipping: only ", nrow(overlap), " overlapping segments")
    return(NULL)
  }
  
  # Fit one lm per metric — vectorised with lapply, no nested loop
  coeffs <- lapply(metrics, function(m) {
    ref_col   <- paste0(m, "_ref")
    other_col <- paste0(m, "_other")
    
    x <- overlap[[ref_col]]
    y <- overlap[[other_col]]
    ok <- is.finite(x) & is.finite(y)
    
    if (sum(ok) < min_overlap) return(NULL)
    
    mod <- tryCatch(
      lm(y[ok] ~ x[ok]),
      error = function(e) NULL
    )
    if (is.null(mod)) return(NULL)
    
    a  <- coef(mod)[1]
    b  <- coef(mod)[2]
    r2 <- summary(mod)$r.squared
    
    if (r2 < min_r2) return(NULL)
    
    list(a = a, b = b, r2 = r2, n = sum(ok))
  })
  
  names(coeffs) <- metrics
  
  # Drop metrics that failed
  coeffs <- Filter(Negate(is.null), coeffs)
  
  if (length(coeffs) == 0) return(NULL)
  
  coeffs
}


# ── 3. MAIN LOOP ───────────────────────────────────────────────────────
dates       <- unique(dt_final$date)
result_list <- vector("list", length(dates))
coeff_rows  <- vector("list", length(dates))

for (i in seq_along(dates)) {
  d       <- dates[i]
  dt_date <- dt_final[date == d]
  
  sensor_counts <- dt_date[, .(n = .N), by = sensor][order(-n)]
  ref_sensor    <- sensor_counts$sensor[1]
  
  message("Date: ", d, " | Reference: ", ref_sensor,
          " (", sensor_counts$n[1], " obs)")
  
  df_ref <- copy(dt_date[sensor == ref_sensor])
  
  # Reference sensor: aligned = original for all metrics
  for (m in all_metrics) {
    df_ref[, paste0(m, "_aligned") := get(m)]
  }
  
  if (nrow(sensor_counts) == 1) {
    result_list[[i]] <- df_ref
    next
  }
  
  other_sensors  <- sensor_counts$sensor[-1]
  aligned_others <- lapply(other_sensors, function(s) {
    
    message(" Aligning: ", s)
    df_other <- copy(dt_date[sensor == s])
    
    # Get per-band coefficients
    coeffs <- get_perband_coeffs(df_ref, df_other, all_metrics)
    
    if (is.null(coeffs)) {
      message("  All metrics failed — dropping ", s, " for this date")
      return(NULL)
    }
    
    # Apply per-band correction: aligned = (raw - a) / b
    for (m in all_metrics) {
      aligned_col <- paste0(m, "_aligned")
      if (m %in% names(coeffs)) {
        a <- coeffs[[m]]$a
        b <- coeffs[[m]]$b
        df_other[, (aligned_col) := fifelse(
          is.finite(get(m)) & b != 0,
          (get(m) - a) / b,
          NA_real_
        )]
      } else {
        # This metric failed r2 threshold — set to NA for this sensor×date
        df_other[, (aligned_col) := NA_real_]
      }
    }
    
    # Log coefficients for audit
    coeff_rows[[i]] <<- rbind(
      coeff_rows[[i]],
      rbindlist(lapply(names(coeffs), function(m) {
        data.table(
          date    = d,
          ref     = ref_sensor,
          other   = s,
          metric  = m,
          a       = coeffs[[m]]$a,
          b       = coeffs[[m]]$b,
          r2      = coeffs[[m]]$r2,
          n       = coeffs[[m]]$n
        )
      }))
    )
    
    df_other
  })
  
  aligned_others <- Filter(Negate(is.null), aligned_others)
  
  result_list[[i]] <- rbindlist(
    c(list(df_ref), aligned_others),
    use.names = TRUE,
    fill      = TRUE
  )
}

dt_aligned <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
dt_coeffs  <- rbindlist(coeff_rows,  use.names = TRUE, fill = TRUE)

cat("Aligned rows:", nrow(dt_aligned), "\n")
cat("Coefficient records:", nrow(dt_coeffs), "\n")


# ── 4. Z-SCORE ALL ALIGNED METRICS ─────────────────────────────────────
aligned_cols <- paste0(all_metrics, "_aligned")
aligned_cols <- aligned_cols[aligned_cols %in% names(dt_aligned)]
zscore_cols  <- gsub("_aligned", "_zscore", aligned_cols)

dt_aligned[,
           (zscore_cols) := lapply(.SD, function(x) {
             m <- mean(x, na.rm = TRUE)
             s <- sd(x,   na.rm = TRUE)
             if (is.finite(s) && s > 0) (x - m) / s else rep(NA_real_, length(x))
           }),
           by      = date,
           .SDcols = aligned_cols
]

cat("Z-score columns added:", length(zscore_cols), "\n")


# ── 5. COEFFICIENT AUDIT ───────────────────────────────────────────────
if (nrow(dt_coeffs) > 0) {
  
  # Summary per sensor × metric
  audit <- dt_coeffs[, .(
    n_dates    = uniqueN(date),
    median_r2  = round(median(r2),        3),
    median_a   = round(median(a),         4),
    median_b   = round(median(b),         4),
    pct_good_b = round(mean(abs(b-1)<0.2)*100, 1)  # % slopes within 20% of 1
  ), by = .(other, metric)]
  
  print(audit[order(other, metric)])
  
  # Flag suspicious: slope far from 1 or large intercept
  suspicious <- dt_coeffs[abs(b - 1) > 0.5 | abs(a) > 0.1]
  if (nrow(suspicious) > 0) {
    cat("\nSuspicious corrections:", nrow(suspicious), "\n")
    print(suspicious[order(-abs(b - 1))][1:min(20, .N)])
  }
  
  # Plot coefficient distributions per metric and sensor
  ggplot(dt_coeffs[metric %in% bands],   # bands only for readability
         aes(b, fill = other)) +
    geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    facet_wrap(~ metric, scales = "free") +
    scale_fill_brewer(palette = "Set1", name = "Sensor") +
    labs(title = "Distribution of alignment slopes per band",
         x = "Slope (b)", y = "Count") +
    theme_bw()
}


# ── 6. EXPORT ──────────────────────────────────────────────────────────
cols_to_keep <- c(
  "date", "seg_id", "sensor", "satellite",
  "dist_start_km", "dist_end_km", "d_from_basse",
  bands,           # original bands
  aligned_cols,    # per-band aligned versions
  zscore_cols      # z-scored versions
)
cols_to_keep <- cols_to_keep[cols_to_keep %in% names(dt_aligned)]

fwrite(
  dt_aligned[, .SD, .SDcols = cols_to_keep],
  file.path(datapath, "salbia_reflectance_aligned_normalized.csv")
)
cat("Saved", nrow(dt_aligned), "rows,", length(cols_to_keep), "columns\n")

fwrite(dt_coeffs,
       file.path(datapath, "salbia_alignment_coefficients_perband.csv"))
cat("Per-band coefficients saved:", nrow(dt_coeffs), "records\n")







