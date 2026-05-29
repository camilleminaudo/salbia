# ---
# Authors: Camille Minaudo
# Project: "SALBIA"
# date: "May 2026"

# ---

rm(list = ls()) # clear workspace
cat("/014") # clear console


library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(mgcv)

# ── paths ──────────────────────────────────────────────────────────────
scriptpath      <- dirname(rstudioapi::getSourceEditorContext()$path)
path_repo_root  <- dirname(scriptpath)
datapath        <- file.path(path_repo_root, "data/gee")
plotpath        <- file.path(path_repo_root, "plots")
gispath        <- file.path(path_repo_root, "data/GIS")

# ── load data ──────────────────────────────────────────────────────────
# Satellite harmonised reflectance — one row per date × seg_id
dataLS <- fread(file.path(datapath, "salbia_reflectance_aligned_normalized.csv"))
dataLS[, date := as.Date(date)]
dataLS[, rb_ratio := fifelse(blue > 0.001, red / blue, NA_real_)]
dataLS[, nirswir2_ratio := fifelse(swir2 > 0.001, nir / swir2, NA_real_)]
dataLS[, swir2nir_ratio := fifelse(nir > 0.001, swir2 / nir, NA_real_)]

# River segments shapefile — polygon or line, one row per seg_id
# Must have seg_id, dist_start_km, dist_end_km columns

# ── load river segments as polygons ────────────────────────────────────
river_segs <- st_read(file.path("C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/GIS/basse2ocean_buffer200m.shp")) |>
  st_transform(32628)

# Confirm seg_id, dist_start_km, dist_end_km are present
names(river_segs) <- c("fid","seg_id",
                       "seg_length",
                       "dist_start_km", "dist_end_km", 
                       "from_ocean_start", "from_oce_end", "geometry"  )



# ── load in situ data ────────────────────────────────────
insitu <- read.csv(
  file.path(path_repo_root, "data/in_situ/2026_Final_MasterData.csv"),
  skip    = 1,
  dec     = ".",  
  na.strings = c("", "NA", "N/A", "nd", "ND", "-", " ", "#VALUE!")  # catch all common NA codes
)
insitu$date <- as.Date(insitu$date, format = "%d/%m/%Y")

v <- "NO3"
bad <- insitu[[v]][is.na(suppressWarnings(as.numeric(insitu[[v]])))]
bad <- bad[!is.na(bad)]
unique(bad)


# After loading, find all character columns that look like
# they contain numbers with comma decimals
char_cols <- names(insitu)[sapply(insitu, is.character)]
char_cols


for (v in c("depth","depth_max","PSI","SRP","NO2","NH4","TSS","Cl","Br","NO3")) {
  sample_vals <- insitu[[v]][!is.na(insitu[[v]])]
  # Check if replacing comma with dot makes them numeric
  converted <- suppressWarnings(
    as.numeric(gsub(",", ".", sample_vals))
  )
  pct_numeric <- mean(!is.na(converted))
  if (pct_numeric > 0.8) {   # >80% of values parse as numeric after fix
    cat("Converting", v, "— comma decimal detected (",
        round(pct_numeric * 100), "% numeric after fix)\n")
    insitu[[v]] <- as.numeric(gsub(",", ".", insitu[[v]]))
  }
}

# Verify
str(insitu)


# ── build in-situ sf in UTM 28N ────────────────────────────────────────
insitu_sf <- st_as_sf(insitu,
                      coords = c("longitude", "latitude"),
                      crs    = 4326) |>
  st_transform(32628)

# write shp file
# st_write(insitu_sf, 
#          paste0(dirname(gispath), "/", "salbia_in_situ.gpkg"))



# ── SPATIAL MATCH: assign each in-situ point to its nearest segment ────

# st_nearest_feature works on polygons or lines —
# returns the index of the nearest segment for each in-situ point



# Step 1: direct intersection — point falls inside a segment polygon
insitu_joined <- st_join(
  insitu_sf,           # points, already in UTM 32628
  river_segs |> select(seg_id, dist_start_km, dist_end_km),
  join      = st_within,   # strict: point must be inside the polygon
  left      = TRUE          # keep points that fall outside (NA seg_id)
)

# How many matched directly vs fell outside?
n_inside  <- sum(!is.na(insitu_joined$seg_id))
n_outside <- sum( is.na(insitu_joined$seg_id))
cat("Points inside a segment polygon: ", n_inside,  "\n")
cat("Points outside all segments:     ", n_outside, "\n")

# Step 2: for points that fell outside, assign nearest segment
# (catches points outside the 200m buffer)
if (n_outside > 0) {
  outside_idx  <- which(is.na(insitu_joined$seg_id))
  outside_pts  <- insitu_joined[outside_idx, ]
  nearest_idx  <- st_nearest_feature(outside_pts, river_segs)
  dist_to_nearest <- st_distance(outside_pts,
                                 river_segs[nearest_idx, ],
                                 by_element = TRUE) |> as.numeric()
  
  insitu_joined$seg_id[outside_idx]        <- river_segs$seg_id[nearest_idx]
  insitu_joined$dist_start_km[outside_idx] <- river_segs$dist_start_km[nearest_idx]
  insitu_joined$dist_end_km[outside_idx]   <- river_segs$dist_end_km[nearest_idx]
  insitu_joined$dist_to_seg_m              <- NA_real_
  insitu_joined$dist_to_seg_m[outside_idx] <- dist_to_nearest
  
  cat("Outside points: nearest segment distance summary (m):\n")
  print(summary(dist_to_nearest))
}

# Flag which points were inside vs snapped to nearest
insitu_joined <- insitu_joined |>
  mutate(match_type = if_else(is.na(dist_to_seg_m), "within", "nearest"))

ggplot(insitu_joined, aes(dist_to_seg_m))+geom_histogram()+theme_bw()


# Points matched by "nearest" but very far from any segment
# are likely outside the river entirely — filter them
max_snap_dist <- 1000   # meters; adjust based on summary above
insitu_safe <- insitu_joined |>
  filter(match_type == "within" | dist_to_seg_m < max_snap_dist)

cat("In-situ points retained after spatial filter:",
    nrow(insitu_safe), "/", nrow(insitu_joined), "\n")


# ── diagnostic map ─────────────────────────────────────────────────────
ggplot() +
  geom_sf(data = river_segs, fill = "lightblue", color = "steelblue",
          linewidth = 0.2) +
  geom_sf(data = insitu_safe,
          aes(color = match_type, shape = match_type),
          size = 1.8) +
  scale_color_manual(values = c("within" = "darkgreen", "nearest" = "tomato")) +
  labs(title = "In-situ points matched to river segments",
       color = "Match type", shape = "Match type") +
  theme_bw()


# ── TEMPORAL MATCH: join satellite obs on same seg_id within ±dt days ──

dt_days_max <- 2   # tighten to 0 for same-day only, loosen to 3 if needed

insitu_dt <- as.data.table(st_drop_geometry(insitu_safe))
insitu_dt[, obs_id := .I]

matchups <- dataLS[insitu_dt,
                   on             = .(seg_id = seg_id),
                   allow.cartesian = TRUE,
                   nomatch        = 0
][abs(as.numeric(date - i.date)) <= dt_days_max]

setnames(matchups, "date",   "sat_date")
setnames(matchups, "i.date", "insitu_date")
matchups[, dt_days := as.numeric(sat_date - insitu_date)]

best_matchups <- matchups[
  order(obs_id, abs(dt_days))
][, .SD[1], by = obs_id]

cat("Unique in-situ observations matched:", nrow(best_matchups), "\n")



# ── BEST MATCHUP: when multiple satellite dates match, keep closest ─────
# Priority: (1) smallest |dt_days|, (2) smallest dist_to_seg_m
best_matchups <- matchups[
  order(obs_id, abs(dt_days), dist_to_seg_m)
][, .SD[1], by = obs_id]

cat("Date distribution of matchups:\n")
print(table(year(best_matchups$insitu_date)))


# ── DIAGNOSTICS ────────────────────────────────────────────────────────

# 1. Temporal offset distribution
ggplot(best_matchups, aes(dt_days)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Temporal offset between satellite and in-situ",
       x = "Days (satellite − in situ)", y = "Count") +
  theme_bw()

# 2. Sensor coverage of matchups
ggplot(best_matchups, aes(sensor)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Matchups by sensor", x = NULL, y = "Count") +
  theme_bw()

# 3. Core scatter: optical bands vs salinity
ggplot(best_matchups,
       aes(Salinity, nir, color = abs(dt_days))) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "|Δt| days") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Red/Blue ratio vs Salinity",
       x = "Salinity (PSU)", y = "Red/Blue ratio") +
  theme_bw()#+facet_wrap(month(sat_date)~.)

ggplot(best_matchups,
       aes(nir/swir2, Salinity, color = abs(dt_days))) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "|Δt| days") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "Salinity vs NIR / SWIR2 ratio",
       y = "Salinity (PSU)", x = "NIR / SWIR2 ratio") +
  theme_bw()

ggplot(best_matchups,
       aes(TSS, green, color = abs(dt_days))) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "|Δt| days") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  # labs(title = "Green band vs Salinity",
  #      x = "Salinity (PSU)", y = "Green reflectance") +
  theme_bw()



# 4. Facet by variable if you have multiple in-situ params
insitu_vars <- c("Salinity", "TSS", "Chla")   # adjust to your column names
insitu_vars <- insitu_vars[insitu_vars %in% names(best_matchups)]

best_matchups_long <- melt(
  best_matchups,
  id.vars     = c("obs_id", "seg_id", "dist_start_km", "rb_ratio",
                  "sensor", "dt_days", "dist_to_seg_m"),
  measure.vars = insitu_vars,
  variable.name = "variable",
  value.name    = "insitu_value"
)

ggplot(best_matchups_long[!is.na(insitu_value)],
       aes(insitu_value, rb_ratio, color = sensor)) +
  geom_point(alpha = 0.6, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "In-situ value", y = "Red/Blue ratio") +
  theme_bw()

# 5. Longitudinal view: where are the matchups along the river?
ggplot(best_matchups, aes(dist_start_km, Salinity,
                          color = as.factor(month(insitu_date)))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Month") +
  labs(title = "Salinity matchups along river profile",
       x = "Distance from upstream (km)", y = "Salinity (PSU)") +
  theme_bw()


# ── EXPORT 
fwrite(best_matchups,
       file.path(datapath, "salbia_satellite_insitu_matchups.csv"))
cat("Saved", nrow(best_matchups), "matchups.\n")



# ── PLOT SPATIAL PATTERNS ─────────────────────────────────────────────────────────────


library(patchwork)

# ── identify dates with multiple in-situ salinity observations ─────────
dates_multi <- best_matchups[
  !is.na(Salinity),
  .(n_sal = .N),
  by = insitu_date
][n_sal > 5, insitu_date]

cat("Dates with >5 salinity observation:", length(dates_multi), "\n")


# ── full satellite longitudinal profiles for matchup dates ─────────────
# Pull from the full aligned dataset, not from best_matchups
rb_full <- dataLS[
  date %in% dates_multi,   # filter to matchup dates
  .(rb_ratio = mean(rb_ratio, na.rm = TRUE),
    sensor   = first(sensor)),
  by = .(date, d_from_basse, seg_id)
]
setnames(rb_full, "date", "insitu_date")
setnames(rb_full, "d_from_basse", "dist_start_km")


# ── plot function for a single date ────────────────────────────────────
plot_date <- function(d) {
  dat_sal <- best_matchups[insitu_date == d & !is.na(Salinity)]
  dat_rb  <- rb_full[insitu_date == d & !is.na(rb_ratio)]
  
  # shared x range across both panels
  x_range <- range(c(dat_sal$dist_start_km, dat_rb$dist_start_km),
                   na.rm = TRUE)
  
  p_sal <- ggplot(dat_sal, aes(dist_start_km, Salinity)) +
    geom_point(aes(color = as.factor(abs(dt_days))),
               size = 2.5, alpha = 0.8) +
    geom_line(alpha = 0.4) +
    scale_color_brewer(palette = "OrRd", name = "|Δt| days") +
    coord_cartesian(xlim = x_range) +
    labs(
      title = format(d, "%d %b %Y"),
      x     = NULL,
      y     = "Salinity (PSU)"
    ) +
    theme_bw() +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title   = element_text(size = 10))
  
  p_rb <- ggplot(dat_rb, aes(dist_start_km, rb_ratio)) +
    geom_point(aes(color = sensor), size = 1, alpha = 0.5) +
    geom_line(aes(color = sensor, group = sensor), alpha = 0.6) +
    annotate("rect",
             xmin  = min(dat_sal$dist_start_km, na.rm = TRUE),
             xmax  = max(dat_sal$dist_start_km, na.rm = TRUE),
             ymin  = -Inf, ymax = Inf,
             alpha = 0.08, fill = "steelblue") +
    scale_color_brewer(palette = "Set1", name = "Sensor") +
    coord_cartesian(xlim = x_range) +
    labs(
      x = "Distance from upstream (km)",
      y = "Red / Blue"
    ) +
    theme_bw()
  
  p_sal / p_rb +
    plot_layout(heights = c(1.5, 1), guides = "collect")
}

# ── generate and save one plot per date ────────────────────────────────
dir.create(plotpath, showWarnings = FALSE)

for (d in sort(dates_multi)) {
  d <- as.Date(d)
  p <- plot_date(d)
  
  ggsave(
    filename = file.path(plotpath,
                         paste0("matchup_", format(d, "%Y%m%d"), ".pdf")),
    plot   = p,
    width  = 8,
    height = 5
  )
}
cat("Saved", length(dates_multi), "plots to", plotpath, "\n")


# ── all dates on one page with facet_wrap ────────────────────

# ── shared x range across all dates ────────────────────────────────────
x_range_all <- c(0,415)

# ── in-situ observation range per date (for the shaded band) ───────────
sal_all <- best_matchups[insitu_date %in% dates_multi & !is.na(Salinity)]
sal_ranges <- sal_all[, .(xmin = min(dist_start_km, na.rm = TRUE),
                          xmax = max(dist_start_km, na.rm = TRUE)),
                      by = insitu_date]

p_sal_all <- ggplot(sal_all,
                    aes(dist_start_km, Salinity)) +
  geom_point(size = 1.2, alpha = 0.99) +
  geom_line(aes(group = insitu_date), alpha = 0.3) +
  # scale_color_brewer(palette = "OrRd", name = "|Δt| days") +
  coord_cartesian(xlim = x_range_all) +
  facet_grid(~ insitu_date, labeller = label_both) +
  labs(x = NULL, y = "Salinity (PSU)") +
  theme_bw(base_size = 8) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p_rb_all <- ggplot(rb_full,
                   aes(dist_start_km, rb_ratio, color = sensor)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_line(aes(group = interaction(insitu_date, sensor)), alpha = 0.3) +
  # shaded band showing in-situ sampling range per facet
  geom_rect(data = sal_ranges,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            alpha = 0.08, fill = "steelblue") +
  scale_color_brewer(palette = "Set1", name = "Sensor") +
  coord_cartesian(xlim = x_range_all) +
  facet_grid(~ insitu_date, labeller = label_both) +
  labs(x = "Distance from upstream (km)", y = "Red / Blue") +
  theme_bw(base_size = 8)

p_overview <- p_sal_all / p_rb_all +
  plot_layout(heights = c(1.5, 1), guides = "collect")

ggsave(
  filename = file.path(plotpath, "matchup_all_dates_overview.pdf"),
  plot     = p_overview,
  width    = 14,
  height   = 10
)
cat("Overview plot saved.\n")









# ────────────────────────────────────────────────────────────────────────────
# ── EXPLORING RELATIONSHIPS BETWEEN WQ AND OPTICAL INDICATORS ───────────────────────────────────────

library(corrplot)

# ── 1. COMPUTE ALL BAND FEATURES ───────────────────────────────────────
# Work on best_matchups which already has both sat and in-situ columns

bm <- copy(best_matchups)

# Single band ratios — all meaningful pairwise combinations
# using bands common to both Landsat and S2
bands <- c("blue", "green", "red", "nir", "swir1", "swir2")

# All pairwise ratios b1/b2 where b1 != b2
ratio_pairs <- CJ(b1 = bands, b2 = bands)[b1 != b2]

for (i in seq_len(nrow(ratio_pairs))) {
  b1   <- ratio_pairs$b1[i]
  b2   <- ratio_pairs$b2[i]
  cname <- paste0(b1, "_", b2)
  bm[, (cname) := fifelse(
    get(b2) > 0.001, get(b1) / get(b2), NA_real_
  )]
}

# Named indices with physical meaning
bm[, NDVI   := (nir - red)   / (nir + red)]
bm[, NDWI   := (green - nir) / (green + nir)]   # Gao 1996
bm[, MNDWI  := (green - swir1) / (green + swir1)]  # Xu 2006
bm[, NDTI   := (red - green) / (red + green)]   # turbidity index
bm[, EVI    := 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)]
bm[, SABI   := (nir - red) / (blue + green)]    # surface algal bloom
bm[, FLH    := green - 1.005 * (blue + (red - blue) * (560 - 490) / (665 - 490))]  # fluorescence proxy

# Log-transformed single bands (often linearises bio-optical relationships)
for (b in bands) {
  cname <- paste0("log_", b)
  bm[, (cname) := log(get(b))]
}

# Collect all candidate feature names
ratio_names <- paste0(ratio_pairs$b1, "_", ratio_pairs$b2)
index_names <- c("NDVI","NDWI","MNDWI","NDTI","EVI","SABI","FLH")
logband_names <- paste0("log_", bands)
all_features <- c(bands, ratio_names, index_names, logband_names)

cat("Total candidate features:", length(all_features), "\n")


# ── 2. SCREENING: correlation of every feature with each in-situ var ───
insitu_vars <- c("Salinity", "TSS", "Chla", "DOC")
insitu_vars <- insitu_vars[insitu_vars %in% names(bm)]

# Compute Pearson and Spearman r for every feature × in-situ variable
screen <- rbindlist(lapply(insitu_vars, function(yvar) {
  rbindlist(lapply(all_features, function(feat) {
    x <- bm[[feat]]
    y <- bm[[yvar]]
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 10) return(NULL)
    data.table(
      insitu_var = yvar,
      feature    = feat,
      n          = sum(ok),
      r_pearson  = cor(x[ok], y[ok], method = "pearson"),
      r_spearman = cor(x[ok], y[ok], method = "spearman")
    )
  }))
}))

screen[, abs_r_pearson  := abs(r_pearson)]
screen[, abs_r_spearman := abs(r_spearman)]
screen[order(-abs_r_spearman)]

# Top 10 features per in-situ variable by Spearman |r|
top_features <- screen[,
                       .SD[order(-abs_r_spearman)][1:min(10, .N)],
                       by = insitu_var
]
print(top_features[, .(insitu_var, feature, n,
                       r_pearson  = round(r_pearson,  3),
                       r_spearman = round(r_spearman, 3))])


# ── 3. CORRELATION HEATMAP ─────────────────────────────────────────────
# Visual overview of all feature × in-situ correlations

screen_wide <- dcast(
  screen,
  feature ~ insitu_var,
  value.var = "r_spearman"
)

# Sort rows by average |r| across all in-situ vars
screen_wide[, mean_abs_r := rowMeans(abs(.SD), na.rm = TRUE),
            .SDcols = insitu_vars]
screen_wide <- screen_wide[order(-mean_abs_r)]

# Plot heatmap — top 30 features only for readability
top30 <- screen_wide$feature[1:min(30, nrow(screen_wide))]
mat   <- as.matrix(screen_wide[feature %in% top30,
                               .SD, .SDcols = insitu_vars],
                   rownames = screen_wide[feature %in% top30, feature])

corrplot(
  mat,
  is.corr    = FALSE,
  method     = "color",
  col        = colorRampPalette(c("tomato","white","steelblue"))(200),
  cl.lim     = c(-1, 1),
  addCoef.col = "black",
  number.cex  = 0.6,
  tl.cex      = 0.7,
  tl.col      = "black",
  title       = "Spearman r: satellite features vs in-situ variables",
  mar         = c(0, 0, 2, 0)
)


# ── 4. SCATTER PLOTS FOR TOP FEATURES ─────────────────────────────────
# For each in-situ variable, plot top N features

plot_top_scatter <- function(yvar, n_top = 6) {
  top_feats <- screen[insitu_var == yvar][order(-abs_r_spearman)]$feature[1:n_top]
  
  plot_list <- lapply(top_feats, function(feat) {
    dat <- bm[is.finite(get(feat)) & is.finite(get(yvar))]
    r   <- screen[insitu_var == yvar & feature == feat, r_spearman]
    
    ggplot(dat, aes(x = get(feat), y = get(yvar))) +
      geom_point(aes(color = sensor), alpha = 0.6, size = 1.5) +
      geom_smooth(method = "lm",  se = TRUE,
                  color = "black",    linetype = "solid",  linewidth = 0.8) +
      geom_smooth(method = "gam", se = FALSE,
                  color = "tomato",   linetype = "dashed", linewidth = 0.8,
                  method.args = list(method = "REML")) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title    = feat,
        subtitle = paste0("Spearman r = ", round(r, 3)),
        x        = feat,
        y        = yvar
      ) +
      theme_bw(base_size = 9) +
      theme(legend.position = "none",
            plot.title    = element_text(size = 9, face = "bold"),
            plot.subtitle = element_text(size = 8))
  })
  
  wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title   = paste("Top", n_top, "features for", yvar),
      caption = "Black = linear fit | Red dashed = GAM"
    )
}

for (yvar in insitu_vars) {
  p <- plot_top_scatter(yvar, n_top = 6)
  print(p)
  ggsave(
    filename = file.path(plotpath,
                         paste0("feature_screen_", yvar, ".pdf")),
    plot  = p,
    width = 12,
    height = 8
  )
}


# ── 5. SENSOR-STRATIFIED CORRELATIONS ─────────────────────────────────
# Check whether relationships are consistent across sensors
# or driven by one sensor only

screen_sensor <- rbindlist(lapply(insitu_vars, function(yvar) {
  rbindlist(lapply(unique(bm$sensor), function(s) {
    rbindlist(lapply(top_features[insitu_var == yvar, feature], function(feat) {
      sub <- bm[sensor == s]
      x   <- sub[[feat]]
      y   <- sub[[yvar]]
      ok  <- is.finite(x) & is.finite(y)
      if (sum(ok) < 5) return(NULL)
      data.table(
        insitu_var = yvar,
        sensor     = s,
        feature    = feat,
        n          = sum(ok),
        r_spearman = cor(x[ok], y[ok], method = "spearman")
      )
    }))
  }))
}))

# Plot: is the correlation consistent across sensors?
ggplot(screen_sensor,
       aes(sensor, r_spearman, fill = sensor)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(insitu_var ~ feature, scales = "free_x") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Spearman r by sensor — top features",
    x     = NULL,
    y     = "Spearman r"
  ) +
  theme_bw(base_size = 7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(size = 6))

ggsave(file.path(plotpath, "feature_screen_by_sensor.pdf"),
       width = 16, height = 8)


# ── 6. EXPORT SCREENING TABLE ──────────────────────────────────────────
fwrite(screen[order(insitu_var, -abs_r_spearman)],
       file.path(datapath, "feature_screening_correlations.csv"))
cat("Screening table saved.\n")










# ────────────────────────────────────────────────────────────────────────────
# ── TESTING SEVERAL MODELS FOR TOP 6 CANDIDATES ───────────────────────────────────────

# ── model fitting infrastructure ───────────────────────────────────────

# Each model is defined by a name, a formula string for the plot label,
# and a function that takes (x, y) vectors and returns a fitted object
# with a predict method and AIC/R² extractable via broom

fit_models <- function(x, y) {
  
  ok  <- is.finite(x) & is.finite(y) & x > 0 & y > 0
  x   <- x[ok]
  y   <- y[ok]
  df  <- data.frame(x = x, y = y,
                    log_x = log(x), log_y = log(y),
                    x2 = x^2)
  
  models <- list(
    
    linear = lm(y ~ x, data = df),
    
    inverse = lm(y ~ I(1/x), data = df),
    
    poly2 = lm(y ~ x + x2, data = df),
    
    power = lm(log_y ~ log_x, data = df),   # log(y) ~ log(x)
    
    exponential = lm(log_y ~ x, data = df), # log(y) ~ x
    
    log_linear = lm(y ~ log_x, data = df),  # y ~ log(x)
    
    gam = gam(y ~ s(x, k = 10), data = df, method = "REML")
  )
  
  models
}

# Extract summary metrics from a fitted model
# For log-transformed models, R² is computed on the original scale
extract_metrics <- function(models, x, y) {
  
  ok <- is.finite(x) & is.finite(y) & x > 0 & y > 0
  x  <- x[ok]
  y  <- y[ok]
  df <- data.frame(x = x, y = y,
                   log_x = log(x), log_y = log(y),
                   x2 = x^2)
  
  rbindlist(lapply(names(models), function(mname) {
    
    mod <- models[[mname]]
    
    # Predictions on original y scale
    y_hat <- switch(mname,
                    linear      = predict(mod, df),
                    inverse     = predict(mod, df),
                    poly2       = predict(mod, df),
                    power       = exp(predict(mod, df)),
                    exponential = exp(predict(mod, df)),
                    log_linear  = predict(mod, df),
                    gam         = predict(mod, df)
    )
    
    ss_res <- sum((y - y_hat)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2_orig <- 1 - ss_res / ss_tot
    
    aic_val <- tryCatch(AIC(mod), error = function(e) NA_real_)
    
    data.table(
      model    = mname,
      n        = length(y),
      r2_orig  = round(r2_orig, 4),
      AIC      = round(aic_val, 2),
      RMSE     = round(sqrt(mean((y - y_hat)^2)), 4)
    )
  }))
}

# Generate prediction ribbon for a model over a fine x grid
predict_model <- function(mod, mname, x_seq) {
  df_new <- data.frame(
    x     = x_seq,
    log_x = log(x_seq),
    x2    = x_seq^2
  )
  y_hat <- switch(mname,
                  linear      = predict(mod, df_new),
                  inverse     = predict(mod, df_new),
                  poly2       = predict(mod, df_new),
                  power       = exp(predict(mod, df_new)),
                  exponential = exp(predict(mod, df_new)),
                  log_linear  = predict(mod, df_new),
                  gam         = as.numeric(predict(mod, df_new))
  )
  data.frame(x = x_seq, y_hat = y_hat, model = mname)
}


# ── model colours and labels ───────────────────────────────────────────
model_colors <- c(
  linear      = "#333333",
  inverse     = "#E41A1C",
  poly2       = "#377EB8",
  power       = "#4DAF4A",
  exponential = "#984EA3",
  log_linear  = "#FF7F00",
  gam         = "#A65628"
)
model_labels <- c(
  linear      = "Linear: y = a + bx",
  inverse     = "Inverse: y = a/x + b",
  poly2       = "Poly2: y = a + bx + cx²",
  power       = "Power: y = ax^b",
  exponential = "Exponential: y = ae^(bx)",
  log_linear  = "Log-linear: y = a + b·log(x)",
  gam         = "GAM"
)


# ── main comparison function for one feature × one in-situ variable ────
compare_models <- function(feat, yvar, data, n_top_plot = 3) {
  
  x <- data[[feat]]
  y <- data[[yvar]]
  
  # fit all models
  models  <- fit_models(x, y)
  metrics <- extract_metrics(models, x, y)
  metrics[, feature    := feat]
  metrics[, insitu_var := yvar]
  metrics <- metrics[order(-r2_orig)]
  
  # prediction grid
  ok    <- is.finite(x) & is.finite(y) & x > 0 & y > 0
  x_seq <- seq(min(x[ok], na.rm = TRUE),
               max(x[ok], na.rm = TRUE),
               length.out = 200)
  
  preds <- rbindlist(lapply(names(models), function(mname) {
    tryCatch(
      predict_model(models[[mname]], mname, x_seq),
      error = function(e) NULL
    )
  }))
  
  # show all models but highlight top 3 by R²
  top_models    <- metrics$model[1:min(n_top_plot, nrow(metrics))]
  preds[, alpha := ifelse(model %in% top_models, 1.0, 0.35)]
  preds[, lwd   := ifelse(model %in% top_models, 1.2, 0.6)]
  
  # scatter + model curves
  df_plot <- data.frame(x = x[ok], y = y[ok])
  
  p_scatter <- ggplot(df_plot, aes(x, y)) +
    geom_point(alpha = 0.5, size = 1.5, color = "gray40") +
    geom_line(data = preds,
              aes(x, y_hat,
                  color = model,
                  alpha = alpha,
                  linewidth = lwd,
                  group = model)) +
    scale_color_manual(values = model_colors,
                       labels = model_labels,
                       name   = "Model") +
    scale_alpha_identity() +
    scale_linewidth_identity() +
    labs(
      title    = paste(feat, "→", yvar),
      subtitle = paste0("Best: ", metrics$model[1],
                        "  R²=", metrics$r2_orig[1],
                        "  RMSE=", metrics$RMSE[1]),
      x = feat,
      y = yvar
    ) +
    theme_bw(base_size = 9) +
    theme(legend.text = element_text(size = 7))
  
  # metrics table as a plot
  metrics_fmt <- metrics[, .(
    Model = model,
    R2    = r2_orig,
    RMSE  = RMSE,
    AIC   = AIC
  )]
  # highlight best R² row
  metrics_fmt[, best := R2 == max(R2)]
  
  p_table <- ggplot(metrics_fmt,
                    aes(y = reorder(Model, R2))) +
    geom_col(aes(x = R2, fill = best),
             show.legend = FALSE, width = 0.6) +
    geom_text(aes(x = R2,
                  label = paste0("R²=", R2,
                                 "  RMSE=", RMSE)),
              hjust = -0.05, size = 2.8) +
    scale_fill_manual(values = c("FALSE" = "steelblue",
                                 "TRUE"  = "tomato")) +
    scale_x_continuous(limits = c(0, 1.15),
                       breaks = seq(0, 1, 0.2)) +
    labs(x = "R² (original scale)", y = NULL,
         title = "Model comparison") +
    theme_bw(base_size = 9)
  
  list(
    plot    = p_scatter / p_table + plot_layout(heights = c(2, 1)),
    metrics = metrics
  )
}


# ── run for top 6 features per in-situ variable ────────────────────────
# Uses `top_features` and `screen` from the previous screening script

all_metrics <- NULL

for (yvar in insitu_vars) {
  
  top6 <- screen[insitu_var == yvar][order(-abs_r_spearman)]$feature[1:6]
  
  plots <- lapply(top6, function(feat) {
    tryCatch(
      compare_models(feat, yvar, bm),
      error = function(e) {
        message("Failed: ", feat, " ~ ", yvar, " — ", e$message)
        NULL
      }
    )
  })
  
  # collect metrics
  all_metrics <- rbindlist(
    c(list(all_metrics),
      lapply(plots, function(p) if (!is.null(p)) p$metrics)),
    fill = TRUE
  )
  
  # save individual plots
  for (i in seq_along(top6)) {
    if (is.null(plots[[i]])) next
    ggsave(
      filename = file.path(plotpath,
                           paste0("model_compare_", yvar, "_",
                                  top6[i], ".pdf")),
      plot  = plots[[i]]$plot,
      width = 8,
      height = 7
    )
  }
  
  # overview: all 6 features on one page
  plot_grid <- wrap_plots(
    lapply(plots, function(p) if (!is.null(p)) p$plot),
    ncol = 3
  ) +
    plot_annotation(
      title = paste("Model comparison —", yvar)
    )
  
  ggsave(
    filename = file.path(plotpath,
                         paste0("model_compare_", yvar, "_overview.pdf")),
    plot  = plot_grid,
    width = 18,
    height = 12
  )
}

# ── summary table: best model per feature × variable ──────────────────
best_per_pair <- all_metrics[
  !is.na(r2_orig),
  .SD[which.max(r2_orig)],
  by = .(feature, insitu_var)
][order(insitu_var, -r2_orig)]

print(best_per_pair[, .(insitu_var, feature, model,
                        r2_orig, RMSE, AIC)])

fwrite(all_metrics,
       file.path(datapath, "model_comparison_all.csv"))
fwrite(best_per_pair,
       file.path(datapath, "model_comparison_best.csv"))
cat("Model comparison tables saved.\n")