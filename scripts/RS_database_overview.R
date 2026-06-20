
# ---
# Authors: Camille Minaudo
# Project: "SALBIA"
# date: "April 2026"

# ---

# ---


rm(list = ls()) # clear workspace
cat("/014") # clear console

# ---- packages ----
library(tidyverse)
library(lubridate)
library(ggplot2)
require(dplyr)
library(sf)
library(data.table)

scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
path_repo_root <- dirname(scriptpath) # path to the cloned repo
datapath <- file.path(path_repo_root,"data/gee")
plotpath <- file.path(path_repo_root,"plots")



# ── load data ──────────────────────────────────────────────────────────
# Satellite harmonised reflectance — one row per date × seg_id
setwd(datapath)
dt_aligned <- fread("salbia_reflectance_aligned_normalized.csv")
dt_aligned[, date := as.Date(date)]




# ── load river segments as polygons ────────────────────────────────────
river_segs <- st_read(file.path("C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/GIS/basse2ocean_buffer200m.shp")) |>
  st_transform(32628)

# Confirm seg_id, dist_start_km, dist_end_km are present
names(river_segs) <- c("fid","seg_id",
                       "seg_length",
                       "dist_start_km", "dist_end_km", 
                       "from_ocean_start", "from_oce_end", "geometry"  )

head(river_segs)




library(patchwork)
library(scales)

# ── Palette & theme ───────────────────────────────────────────────────────────

sensor_pal <- c(
  LANDSAT_5   = "#2c6e8a",
  LANDSAT_7   = "#4fb3ce",
  LANDSAT_8   = "#f4a261",
  LANDSAT_9   = "#e76f51",
  SENTINEL_2  = "#386641"
)

theme_pub <- function() {
  theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      plot.title        = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle     = element_text(colour = "grey40", size = 9, margin = margin(b = 8)),
      axis.title        = element_text(size = 9, colour = "grey30"),
      axis.text         = element_text(size = 8, colour = "grey40"),
      panel.grid.major  = element_line(colour = "grey92", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.title      = element_text(size = 8, face = "bold"),
      legend.text       = element_text(size = 8),
      legend.key.size   = unit(0.8, "lines"),
      strip.text        = element_text(face = "bold", size = 9),
      plot.caption      = element_text(colour = "grey55", size = 7, margin = margin(t = 6)),
      plot.margin       = margin(8, 10, 8, 8)
    )
}

# ── Prep: one row per unique date × seg_id × sensor ──────────────────────────
# (avoids double-counting within the same acquisition)

obs <- dt_aligned[, .N, by = .(date, seg_id, sensor)]
obs[, `:=`(
  year  = year(date),
  month = month(date),
  doy   = yday(date)
)]

# ── Figure 1: Annual observation count by sensor ──────────────────────────────

annual <- obs[, .(n_obs = .N), by = .(year, sensor)]

p1 <- ggplot(annual, aes(x = year, y = n_obs, fill = sensor)) +
  geom_col(width = 0.8, colour = NA) +
  scale_fill_manual(values = sensor_pal, name = "Sensor") +
  scale_x_continuous(breaks = seq(1984, 2026, 4), expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(
    title    = "Annual satellite observations",
    subtitle = "Number of segment × date records per year",
    x        = NULL,
    y        = "Observations",
    caption  = "One observation = one segment on one acquisition date"
  ) +
  theme_pub()

# ── Figure 2: Monthly distribution (seasonality) ─────────────────────────────

monthly <- obs[, .(n_obs = .N), by = .(month, sensor)]
monthly[, month_lbl := factor(month.abb[month], levels = month.abb)]

p2 <- ggplot(monthly, aes(x = month_lbl, y = n_obs, fill = sensor)) +
  geom_col(width = 0.8, colour = NA) +
  scale_fill_manual(values = sensor_pal, name = "Sensor") +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(
    title    = "Seasonal distribution of observations",
    subtitle = "Aggregated across all years",
    x        = NULL,
    y        = "Observations"
  ) +
  theme_pub() +
  theme(legend.position = "none")   # shared with p1

# ── Figure 3: Longitudinal coverage — obs density along river ─────────────────

longit <- obs[, .(n_obs = .N), by = .(seg_id, sensor)]
# Join distance from ocean from dt_aligned
seg_dist <- unique(dt_aligned[, .(seg_id, d_from_basse)])
longit <- merge(longit, seg_dist, by = "seg_id", all.x = TRUE)
# Distance from ocean (assuming total river ~415 km from your seg table)
longit[, d_from_ocean := 415.6 - d_from_basse]

p3 <- ggplot(longit, aes(x = d_from_ocean, y = n_obs, colour = sensor)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_colour_manual(values = sensor_pal, name = "Sensor") +
  scale_x_continuous(
    breaks = seq(0, 400, 50),
    # trans  = "reverse",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Longitudinal coverage along the Gambia River",
    subtitle = "Total observations per 500 m segment",
    x        = "Distance from ocean [km]",
    y        = "Observations",
    caption  = "x-axis reversed: ocean at right, headwaters at left"
  ) +
  theme_pub() +
  theme(legend.position = "none")

# ── Figure 4: Spatial map — obs density per river segment ────────────────────

# Aggregate total obs per seg_id
seg_obs <- obs[, .(n_obs = .N), by = seg_id]

# Join to shapefile
river_map <- river_segs %>%
  left_join(as.data.frame(seg_obs), by = "seg_id") %>%
  st_transform(4326)   # WGS84 for plotting

# Bounding box for background
bbox <- st_bbox(river_map %>% st_buffer(10000))

library(ggspatial)

# Print all available tile types
rosm::osm.types()

# Best options for publication:
# "cartolight"     — minimal, grey, very clean
# "cartodark"      — dark background, segments pop
# "stamenterrain"  — terrain shading, good for river geography
# "osm"            — standard OSM, can be busy
# "hotstyle"       — simplified OSM variant

river_map_wide <- river_map %>%
  st_transform(32628) %>%          # back to UTM for metric buffering
  st_buffer(dist = 500) %>%        # add 500 m buffer on each side — adjust to taste
  st_transform(4326)               # back to WGS84 for plotting



p4 <- ggplot(river_map_wide) +
  
  # Background tiles — choose one:
  annotation_map_tile(
    type    = "hotstyle",   
    # type = "osm"            # more detail but busier
    # type = "stamenterrain"  # terrain, good for river context
    zoom    = 9,              # adjust: lower = less detail, faster
    quiet   = TRUE
  ) +
  
  geom_sf(aes(fill = n_obs), 
          colour    = NA,    # outline colour — try "grey20" for dark or "white" for light basemap
          linewidth = 0.3,        # increase to 0.5–0.8 if still too subtle
          alpha     = 0.85) +
  
  
  scale_fill_viridis_c(
    option    = "plasma",
    name      = "Obs.",
    labels    = comma,
    na.value  = "transparent",   # NAs invisible over basemap
    direction = -1
  ) +
  
  annotation_scale(
    location   = "bl",
    width_hint = 0.2,
    line_width = 0.5,
    text_cex   = 0.7
  ) +
  
  annotation_north_arrow(
    location = "bl",
    pad_y    = unit(0.5, "cm"),
    style    = north_arrow_minimal(text_size = 8)
  ) +
  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    crs  = 4326
  ) +
  
  labs(
    title    = "Spatial distribution of observations",
    subtitle = "Total satellite observations per 500 m river segment",
    x = NULL, y = NULL,
    caption  = "Basemap © CartoDB | WGS 84"
  ) +
  
  theme_pub() +
  theme(
    panel.grid.major  = element_line(colour = "grey88", linewidth = 0.3),
    axis.text         = element_text(size = 7),
    panel.background  = element_rect(fill = "aliceblue")  # ocean/background colour
  )

# ── Assemble & save ───────────────────────────────────────────────────────────

# Combined temporal panel (p1 + p2 sharing legend)
p_temporal <- (p1 + p2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "Gambia River — Remote Sensing Database Overview",
    theme   = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(
  file.path(plotpath, "RS_database_temporal.jpg"),
  p_temporal, width = 10, height = 4, dpi = 300
)

# Longitudinal + spatial panel
p_spatial <- p3 / p4 +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    title = "Gambia River — Spatial Coverage of Remote Sensing Database",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(
  file.path(plotpath, "RS_database_spatial.jpg"),
  p4, width = 9, height = 9, dpi = 300
)

# All four as a single conference poster panel
p_all <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Gambia River Remote Sensing Database",
    subtitle = "Landsat 5–9 and Sentinel-2 | 1984–2026 | 500 m segments",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(colour = "grey40", size = 10)
    )
  )

ggsave(
  file.path(plotpath, "RS_database_overview.jpg"),
  p_all, width = 12, height = 8, dpi = 300
)







