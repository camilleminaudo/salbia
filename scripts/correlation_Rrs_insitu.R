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
dataLS[, nirswir1_ratio := fifelse(swir1 > 0.001, nir / swir1, NA_real_)]
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
  file.path(path_repo_root, "data/in_situ/2026_Final_MasterData_v20260615.csv"),
  skip    = 1,
  dec     = ".",  
  na.strings = c("", "NA", "N/A", "nd", "ND", "-", " ", "#VALUE!")  # catch all common NA codes
)
insitu$date <- as.Date(insitu$date, format = "%d/%m/%Y")



# After loading, find all character columns that look like
# they contain numbers with comma decimals
char_cols <- names(insitu)[sapply(insitu, is.character)]
char_cols

v <- "NO3"
bad <- insitu[[v]][is.na(suppressWarnings(as.numeric(insitu[[v]])))]
bad <- bad[!is.na(bad)]
unique(bad)



for (v in c("depth","depth_max","PSI","SRP","NO2","NH4...41","Cl","Br","NO3")) {
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

# Clean station names
# t_to_s <- c(T01 = "S01", T02 = "S02", T05 = "S05", T06 = "S06",
#             T08 = "S08", T10 = "S10", T11 = "S11", T12 = "S12")
# insitu <- insitu %>%
#   mutate(site_id = recode(site_id, !!!t_to_s))

insitu %>% distinct(site_id, site_name) %>% arrange(site_id)

site_names_clean <- tribble(
  ~site_id, ~site_name,
  "S01", "Tendaba",
  "S02", "Bamba Tenda",
  "S03", "Kalagi",
  "S04", "Wali Kunda",
  "S05", "Kauur",
  "S06", "Kuntaur",
  "S07", "Bansang",
  "S08", "Jarreng Tenda",
  "S09", "Basse",
  "S10", "Kemoto",
  "S11", "Bonto Tenda",
  "S12", "Bureng",
  "S13", "Ocean"
)

insitu <- insitu %>%
  select(-site_name) %>%
  left_join(site_names_clean, by = "site_id")

insitu$site_name[is.na(insitu$site_name)] <- "Longitudinal"

# ── load in situ sensor data ────────────────────────────────────
sensors_salbia <- read.csv(
  file.path(path_repo_root, "data/in_situ/data_HOBOs_2025_clean.csv"),
  dec     = ".",  
  na.strings = c("", "NA", "N/A", "nd", "ND", "-", " ", "#VALUE!")  # catch all common NA codes
)
sensors_salbia$datetime <- ymd_hms(sensors_salbia$Date_GMT, tz = "UTC")

# take data from Kemoto out
sensors_salbia <- sensors_salbia[which(sensors_salbia$Station != "S10"),]



sensors_wgr <- read.csv(
  file.path(path_repo_root, "data/in_situ/data_WGR_salinity_clean.csv"),
  dec     = ".",  
  na.strings = c("", "NA", "N/A", "nd", "ND", "-", " ", "#VALUE!")  # catch all common NA codes
)
sensors_wgr$datetime <- ymd_hms(sensors_wgr$Date, tz = "UTC")
unique(sensors_wgr$Station_Name)


ggplot(sensors_wgr, aes(datetime, Salinity ))+geom_path()+theme_bw()+facet_wrap(Station_Name~.)



crosswalk <- tribble(
  ~source,   ~sensor_name,      ~site_id,
  "salbia",  "Tendaba",         "S01",
  "salbia",  "Bamba Tenda",     "S02",
  "salbia",  "Wali Kunda",      "S04",
  "salbia",  "Kaur Wharf",      "S05",
  "salbia",  "Kuntaur",         "S06",
  "salbia",  "Basse",           "S09",
  "salbia",  "Kemoto",          "S10",
  "wgr",     "Tendaba",         "S01",
  "wgr",     "Bansang",         "S07",
  "wgr",     "Basse",           "S09",
  "wgr",     "Kaur",            "S05",
  "wgr",     "Kuntaur",         "S06",
  "wgr",     "Ballingho",       NA_character_,
  "wgr",     "Banjul Harbour",  NA_character_,
  "wgr",     "Pakaliba",        NA_character_
)

site_coords <- insitu %>%
  filter(!is.na(site_id)) %>%
  distinct(site_id, latitude, longitude) %>%
  group_by(site_id) %>%
  summarise(latitude = first(latitude), longitude = first(longitude), .groups = "drop")

daily_salbia <- sensors_salbia %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(Station_Name, date) %>%
  summarise(Salinity = mean(Salinity_PSS, na.rm = TRUE), .groups = "drop") %>%
  left_join(crosswalk %>% filter(source == "salbia"), by = c("Station_Name" = "sensor_name"))

daily_wgr <- sensors_wgr %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(Station_Name, date) %>%
  summarise(Salinity = mean(Salinity, na.rm = TRUE), .groups = "drop") %>%
  left_join(crosswalk %>% filter(source == "wgr"), by = c("Station_Name" = "sensor_name"))

daily_salbia$source <- "sensor salbia"
daily_wgr$source <- "sensor wgr"
daily_sensors <- bind_rows(daily_salbia, daily_wgr) %>%
  filter(!is.na(site_id)) %>%
  left_join(site_coords, by = "site_id") %>%
  select(site_id, date, latitude, longitude, Salinity, source)


insitu <- insitu %>% mutate(date = as.Date(date))  # adjust if needed
insitu$source <- "grab samples"


# ── load in situ sensor data ────────────────────────────────────
long_sensors <- read.csv(
  file.path(path_repo_root, "data/in_situ/longitudinal_sensors_clean.csv")
  )
long_sensors$site_id <- paste0("longitudinal",long_sensors$X)
long_sensors$datetime <- ymd_hms(long_sensors$datetime_15min, tz = "UTC")
long_sensors$date <- as.Date(long_sensors$date)
long_sensors$source <- "longit_transects"


# ── combine all in situ data sources into one ────────────────────────────────────


combined <- bind_rows(insitu, daily_sensors, long_sensors)
length(unique(combined$date))
dim(combined)




ggplot(combined[combined$site_id %in% c("S01","S02","S04","S05","S06","S09","S010") & combined$date>"2023-01-01",], 
       aes(date, Salinity, colour = source))+
  geom_point()+theme_bw()+facet_wrap(site_id~., scales = "free_y")


# plot(table(combined$date))


# 
# 
# S01    597  Tendaba
# S02    537
# S04    254  Wali Kunda
# S05    404  Kauur
# S06    294  Kuntaur
# S07    174
# S09      0  Basse
# S10    676

# idsel <- 597
# datestart <- as.Date("2015-01-01")
# 
# RS_one_station <- dataLS[dataLS$date>datestart & dataLS$seg_id >= idsel-2 & dataLS$seg_id <= idsel+2,c(1:10)]
# 
# head(RS_one_station)
# summary(RS_one_station)
# unique(RS_one_station$sensor)
# 
# 
# 
# smoothedRS <- smooth.spline(x = decimal_date(RS_one_station$date), 
#                             y = RS_one_station$green, 
#                             spar = 0.2)
# 
# smoothedRS_df <- data.frame(date = smoothedRS$x,
#                             Rrs = smoothedRS$y)
# 
# 
# ggplot(RS_one_station, aes(decimal_date(date), green))+
#   geom_path(data = smoothedRS_df, aes(date, Rrs), linewidth = 2)+
#   geom_point(aes(colour = sensor))+theme_bw()
# 
# 
# 
# insitu_one_station <- insitu_daily_sf[insitu_daily_sf$seg_id==idsel,]
# clean_insitu <- data.frame(date = insitu_one_station$date,
#                            Salinity = insitu_one_station$Salinity)
# clean_insitu$decdate <- decimal_date(clean_insitu$date)
# clean_insitu <- clean_insitu[order(clean_insitu$decdate),]
# 
# 
# clean_insitu$Rrs <- approx(x = smoothedRS_df$date, smoothedRS_df$Rrs, xout = clean_insitu$decdate,
#                           method = "linear", )$y
# 
# ggplot(clean_insitu, aes(Rrs, Salinity, colour = year(date)))+geom_point()
# 
# ggplot(clean_insitu, aes(date, Salinity, colour = year(date)))+geom_point()
# ggplot(clean_insitu, aes(date, Rrs, colour = year(date)))+geom_point()



# ── build in-situ sf in UTM 28N ────────────────────────────────────────
insitu_sf <- st_as_sf(combined,
                      coords = c("longitude", "latitude"),
                      crs    = 4326) |>
  st_transform(32628)

# write shp file
st_write(insitu_sf,
         paste0(dirname(gispath), "/", "salbia_in_situ.gpkg"), append = F)



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


ggplot(insitu_safe, aes(seg_id, Salinity, colour = source))+geom_point()




# Identify numeric columns to average (excluding coordinates and IDs)
cols_to_avg <- insitu_safe %>%
  st_drop_geometry() %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("X", "River_branches", "seg_id", "dist_start_km", "dist_end_km", 
                   "dist_to_seg_m", "gap_before_min", "gap_after_min"))) %>%
  names()

# Daily average per seg_id
insitu_daily <- insitu_safe %>%
  st_drop_geometry() %>%
  mutate(
    # Use original date column; fall back to datetime for sensor rows
    date_grp = as.Date(coalesce(
      as.character(date),
      as.character(as.Date(datetime, tz = "UTC"))
    ))
  ) %>%
  group_by(seg_id, date_grp) %>%
  summarise(
    across(all_of(cols_to_avg), ~mean(.x, na.rm = TRUE)),
    site_id       = first(site_id),
    site_name     = first(site_name),
    dist_to_seg_m = first(dist_to_seg_m),
    source        = first(source),
    Season        = first(Season),
    dist_start_km = first(dist_start_km),
    dist_end_km   = first(dist_end_km),
    n_obs         = n(),
    .groups = "drop"
  ) %>%
  rename(date = date_grp)

# Compute centroid geometry per seg_id × date
# Using the same date logic as insitu_daily
geom_daily <- insitu_safe %>%
  mutate(
    date_grp = as.Date(coalesce(
      as.character(date),
      as.character(as.Date(datetime, tz = "UTC"))
    ))
  ) %>%
  group_by(seg_id, date_grp) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(geometry = st_centroid(geometry)) %>%
  rename(date = date_grp)

# Join averaged data back onto the geometry
insitu_daily_sf <- geom_daily %>%
  left_join(insitu_daily, by = c("seg_id", "date")) %>%
  st_as_sf(crs = st_crs(insitu_safe))


cat("Rows in insitu_daily:    ", nrow(insitu_daily), "\n")
cat("Rows in insitu_daily_sf: ", nrow(insitu_daily_sf), "\n")

# Check dates are now present
cat("NA dates in sf object:", sum(is.na(insitu_daily_sf$date)), "\n")

# Spot check
insitu_daily_sf[insitu_daily_sf$date == "2024-08-01", c("seg_id", "date", "Salinity", "site_name")]



# Check how many observations were collapsed per group
insitu_daily_sf %>% 
  st_drop_geometry() %>%
  count(n_obs) %>% 
  arrange(desc(n_obs))



# Check how many rows have valid dates now
cat("NA dates remaining:", sum(is.na(insitu_daily$date)), "\n")
cat("Total daily rows:", nrow(insitu_daily), "\n")




library(leaflet)

# Extract coordinates from geometry into columns
insitu_daily_sf <- st_transform(insitu_daily_sf, crs = 4326)
insitu_daily_sf <- insitu_daily_sf %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude  = st_coordinates(.)[, 2]
  )

pal <- colorNumeric("viridis", reverse = FALSE, domain = insitu_daily_sf$Salinity, na.color = "transparent")

leaflet(insitu_daily_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    color       = ~pal(Salinity),
    radius      = 5,
    stroke      = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>", site_name, "</b><br>",
      "<i>", date, "</i><br>",
      "Salinity: ",     round(Salinity, 2),     " PSS<br>",
      "Temperature: ",  round(Temperature, 2),  " °C<br>",
      "Conductivity: ", round(Conductivity, 2), " µS/cm<br>",
      "Source: ",       source, "<br>",
      "N obs averaged: ", n_obs
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal,
    values   = ~Salinity,
    title    = "Salinity (PSS)",
    opacity  = 0.8
  )



# Longitudinal view: where are the observations along the river?
ggplot(insitu_daily_sf, aes(dist_start_km, Salinity,
                            color = as.factor(month(date)))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Month") +
  labs(title = "Salinity observations along river profile",
       x = "Distance from upstream (km)", y = "Salinity (PSU)") +
  theme_bw()+facet_wrap(source~.)

ggplot(insitu_daily_sf, aes(dist_start_km, TSS,
                            color = as.factor(year(date)))) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Month") +
  labs(title = "TSS observations along river profile",
       x = "Distance from upstream (km)", y = "TSS [mg/L]") +
  theme_bw()


ggplot(insitu_daily_sf, aes(dist_start_km, DO,
                            color = year(date))) +
  geom_point(alpha = 0.7) +
  # scale_color_discrete(name = "Year") +
  labs(title = "DO observations along river profile",
       x = "Distance from upstream (km)", y = "DO [mg/L]") +
  theme_bw()













library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)
library(scales)
library(ggspatial)
library(lubridate)

# ── Prep ──────────────────────────────────────────────────────────────────────


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


insitu_plot <- insitu_daily_sf %>%
  filter(!is.na(Salinity)) %>%
  mutate(
    source = case_when(
      grepl("sensor|Sensor|continuous", source, ignore.case = TRUE) ~ "Continuous sensor",
      grepl("longit", source, ignore.case = TRUE) ~ "Longitudinal survey",
      grepl("grab|Grab|discrete", source, ignore.case = TRUE)       ~ "Grab sample",
      TRUE                                                           ~ source
    )
  )

source_pal <- c(
  "Continuous sensor"   = "#2c6e8a",   # deep teal blue
  "Longitudinal survey" = "#c77dff",   # soft violet — distinct from both others
  "Grab sample"         = "#e76f51"    # warm coral-orange
)

# ── Figure 1: Map of observation locations ────────────────────────────────────

# Summarise to one point per site (avoid overplotting)
site_summary <- insitu_plot %>%
  group_by(site_id, site_name, source) %>%
  summarise(
    n_obs       = n(),
    Sal_mean    = mean(Salinity, na.rm = TRUE),
    geometry    = st_union(geometry) %>% st_centroid(),
    .groups     = "drop"
  ) %>%
  st_as_sf()

bbox_insitu <- st_bbox(site_summary %>% st_buffer(10000))

p_map <- ggplot(site_summary) +
  annotation_map_tile(type = "hotstyle", zoom = 9, quiet = TRUE) +
  geom_sf(aes(colour = source, size = n_obs),
          alpha = 0.85, stroke = 0.5) +
  geom_sf_label(
    aes(label = site_name),
    size          = 2.5,
    nudge_y       = 0.06,
    label.padding = unit(0.12, "lines"),
    label.size    = 0,
    fill          = alpha("white", 0.7),
    colour        = "grey20"
  ) +
  scale_colour_manual(values = source_pal, name = "Source") +
  scale_size_continuous(
    name   = "N observations",
    range  = c(2, 8),
    breaks = c(10, 50, 200, 500)
  ) +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.7) +
  annotation_north_arrow(
    location = "bl", pad_y = unit(0.5, "cm"),
    style = north_arrow_minimal(text_size = 8)
  ) +
  coord_sf(
    xlim = c(bbox_insitu["xmin"], bbox_insitu["xmax"]),
    ylim = c(bbox_insitu["ymin"], bbox_insitu["ymax"])
  ) +
  labs(
    title    = "In situ salinity observation network",
    subtitle = "Gambia River — all stations with salinity data",
    x = NULL, y = NULL,
    caption  = "Basemap © CartoDB | Point size proportional to number of observations"
  ) +
  theme_pub() +
  theme(
    panel.grid.major = element_line(colour = "grey88", linewidth = 0.3),
    axis.text        = element_text(size = 7)
  )

# ── Figure 2: Longitudinal salinity profile ───────────────────────────────────



# Use dist_start_km as distance from Basse; distance from ocean = 415.6 - dist
longit_sal <- insitu_plot %>%
  st_drop_geometry() %>%
  filter(!is.na(Salinity), !is.na(dist_start_km)) %>%
  filter(month(date)>=5, month(date)<=9) %>%
  mutate(d_from_ocean = 415.6 - dist_start_km)

# Summary ribbon per source × distance bin (5 km)
longit_summary <- longit_sal %>%
  mutate(dist_bin = round(d_from_ocean / 5) * 5) %>%
  group_by(source, dist_bin) %>%
  summarise(
    Sal_median = median(Salinity, na.rm = TRUE),
    Sal_q25    = quantile(Salinity, 0.25, na.rm = TRUE),
    Sal_q75    = quantile(Salinity, 0.75, na.rm = TRUE),
    Sal_min    = quantile(Salinity, 0.05, na.rm = TRUE),
    Sal_max    = quantile(Salinity, 0.95, na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  ) %>%
  filter(n >= 3)

p_longit <- ggplot(longit_summary, aes(x = dist_bin, colour = source, fill = source)) +
  
  # 5–95% range
  geom_ribbon(aes(ymin = Sal_min, ymax = Sal_max), alpha = 0.12, colour = NA) +
  
  # IQR
  geom_ribbon(aes(ymin = Sal_q25, ymax = Sal_q75), alpha = 0.25, colour = NA) +
  
  # Median line
  geom_line(aes(y = Sal_median), linewidth = 0.9) +
  
  scale_colour_manual(values = source_pal, name = "Source") +
  scale_fill_manual(values   = source_pal, name = "Source") +
  scale_x_continuous(
    breaks    = seq(0, 400, 50),
    # trans     = "reverse",
    expand    = c(0.01, 0),
    sec.axis  = sec_axis(~415.6 - ., name = "Distance from Basse [km]",
                         breaks = seq(0, 400, 50))
  ) +
  scale_y_continuous(expand = c(0.02, 0)) +
  labs(
    title    = "Longitudinal salinity profile",
    subtitle = "Median ± IQR (shaded) and 5–95th percentile range",
    x        = "Distance from ocean [km]",
    y        = "Salinity [PSS]",
    caption  = "5 km distance bins | minimum 3 observations per bin"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

# ── Figure 3: Time series for fixed sensor stations ───────────────────────────

sensor_sites <- insitu_plot %>%
  st_drop_geometry() %>%
  filter(source == "Continuous sensor", !is.na(Salinity)) %>%
  group_by(site_id) %>%
  filter(n() >= 30) %>%       # only sites with enough data
  ungroup()

# Order sites from ocean to headwaters
site_order <- sensor_sites %>%
  group_by(site_id, site_name) %>%
  summarise(d = 415.6 - mean(dist_start_km, na.rm = TRUE), .groups = "drop") %>%
  arrange(d) %>%
  mutate(site_label = paste0(site_id, "\n(", round(d), " km from ocean)"))

sensor_sites <- sensor_sites %>%
  left_join(site_order %>% select(site_id, site_label, d), by = "site_id") %>%
  mutate(site_label = factor(site_label, levels = site_order$site_label))

p_ts <- ggplot(sensor_sites, aes(x = date, y = Salinity)) +
  
  geom_line(colour = source_pal["Continuous sensor"],
            linewidth = 0.4, alpha = 0.6) +
  geom_point(colour = source_pal["Continuous sensor"],
             size = 0.6, alpha = 0.5) +
  
  # Annual smoothed trend
  # geom_smooth(method = "loess", span = 0.15,
  #             colour    = "grey20",
  #             fill      = "grey70",
  #             linewidth = 0.8,
  #             alpha     = 0.3,
  #             se        = TRUE) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_log10()+
  # scale_y_continuous(expand = c(0.02, 0)) +
  
  facet_wrap(~ site_label, ncol = 3) +
  
  labs(
    title    = "Salinity time series — continuous sensor stations",
    subtitle = "Daily observations with LOESS trend (grey ribbon = 95% CI)",
    x        = NULL,
    y        = "Salinity [PSS]",
    caption  = "Sites ordered from ocean (top-left) to headwaters (bottom-right)"
  ) +
  theme_pub() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 7),
    strip.text   = element_text(size = 8)
  )

# ── Save ──────────────────────────────────────────────────────────────────────

ggsave(file.path(plotpath, "insitu_salinity_map.jpg"),
       p_map,   width = 9,  height = 6,  dpi = 300)

ggsave(file.path(plotpath, "insitu_salinity_longitudinal.jpg"),
       p_longit, width = 10, height = 4.5, dpi = 300)

ggsave(file.path(plotpath, "insitu_salinity_timeseries.jpg"),
       p_ts,    width = 11, height = ceiling(n_distinct(sensor_sites$site_id) / 2) * 2.5,
       dpi = 300)

# ── Combined panel (for poster/talk) ─────────────────────────────────────────

p_combined <- (p_map | p_longit) / p_ts +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title    = "Gambia River — In Situ Salinity Database",
    subtitle = paste0(
      format(min(insitu_plot$date, na.rm = TRUE), "%Y"), "–",
      format(max(insitu_plot$date, na.rm = TRUE), "%Y"),
      " | ", comma(nrow(insitu_plot)), " station-days | ",
      n_distinct(insitu_plot$site_id), " sites"
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(colour = "grey40", size = 10)
    )
  )

ggsave(file.path(plotpath, "insitu_salinity_overview.jpg"),
       p_combined, width = 14, height = 12, dpi = 300)








# ── TEMPORAL MATCH: join satellite obs on same seg_id within ±dt days ──

dt_days_max <- 0   # tighten to 0 for same-day only, loosen to 3 if needed

insitu_dt <- as.data.table(st_drop_geometry(insitu_daily_sf))
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
       aes(green, Salinity, color = sensor)) +
  geom_point(alpha = 0.7) +
  # scale_color_viridis_d(name = "satellite") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "Salinity vs Green band",
       x = "Green band reflectance (dl)",
       y = "Salinity (PSU)") +
  theme_bw()+facet_wrap(source~.)


best_matchups$site_id2 <- best_matchups$site_id
list_not_longit <- c("S01","S02","S03","S04","S05","S06","S07","S08","S09","S10")

best_matchups$site_id2[! best_matchups$site_id %in% list_not_longit] <- "longitudinal"

ggplot(best_matchups,
       aes(green, Salinity, color = source)) +
  geom_point(alpha = 0.7) +
  # scale_color_viridis_d(name = "satellite") +
  geom_smooth(aes(group = source, fill = source), method = "loess", se = TRUE, color = "black") +
  labs(title = "Salinity vs Green band",
       x = "Green band reflectance (dl)",
       y = "Salinity (PSU)") +
  theme_bw()+facet_wrap(site_id2~.)







# ── identify dates with multiple in-situ salinity observations ─────────
dates_multi <- best_matchups[
  !is.na(Salinity),
  .(n_sal = .N),
  by = insitu_date
][n_sal > 6, insitu_date]

cat("Dates with >5 salinity observation:", length(dates_multi), "\n")



# best_matchups <- best_matchups[which(best_matchups$source == "grab samples" | best_matchups$source == "longit_transects"),]

best_matchups <- best_matchups[best_matchups$insitu_date %in% dates_multi,]



ggplot(best_matchups,
       aes(green, (Salinity), color = source)) +
  geom_point(alpha = 0.7) +
  # scale_color_viridis_d(name = "satellite") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  # labs(title = "Red/Blue ratio vs Salinity",
  #      x = "Salinity (PSU)", y = "Red/Blue ratio") +
  theme_bw()



ggplot(best_matchups,
       aes(swir1/red, (Salinity), color = sensor)) +
  geom_point(alpha = 0.7) +
  # scale_color_viridis_d(name = "satellite") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  # labs(title = "Red/Blue ratio vs Salinity",
  #      x = "Salinity (PSU)", y = "Red/Blue ratio") +
  theme_bw()+facet_wrap(source~.)

ggplot(best_matchups,
       aes(swir1/nir, Salinity, color = source)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  # labs(title = "Salinity vs NIR / SWIR2 ratio",
  #      y = "Salinity (PSU)", x = "NIR / SWIR2 ratio") +
  theme_bw()

ggplot(best_matchups,
       aes(TSS, red/blue, color = abs(dt_days))) +
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
  id.vars     = c("obs_id", "seg_id", "dist_start_km", "green","nir","swir1",
                  "source", "dt_days", "dist_to_seg_m"),
  measure.vars = insitu_vars,
  variable.name = "variable",
  value.name    = "insitu_value"
)

ggplot(best_matchups_long[!is.na(insitu_value)],
       aes(insitu_value, nir/swir1, color = source)) +
  geom_point(alpha = 0.6, size = 1.2) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "In-situ value", y = "Green band") +
  theme_bw()



# ── EXPORT 
fwrite(best_matchups,
       file.path(datapath, "salbia_satellite_insitu_matchups.csv"))
cat("Saved", nrow(best_matchups), "matchups.\n")



# ── PLOT SPATIAL PATTERNS ─────────────────────────────────────────────────────────────


library(patchwork)


# ── full satellite longitudinal profiles for matchup dates ─────────────
# Pull from the full aligned dataset, not from best_matchups


RS_full <- dataLS[
  date %in% dates_multi,   # filter to matchup dates
  .(nirswir1_ratio  = mean(nirswir1_ratio , na.rm = TRUE),
    sensor   = first(sensor)),
  by = .(date, d_from_basse, seg_id)
]
setnames(RS_full, "date", "insitu_date")
setnames(RS_full, "d_from_basse", "dist_start_km")


# ── plot function for a single date ────────────────────────────────────
plot_date <- function(d) {
  dat_sal <- best_matchups[insitu_date == d & !is.na(Salinity)]
  dat_rb  <- RS_full[insitu_date == d & !is.na(nirswir1_ratio )]
  
  # shared x range across both panels
  x_range <- range(c(dat_sal$dist_start_km, dat_rb$dist_start_km),
                   na.rm = TRUE)
  
  p_sal <- ggplot(dat_sal, aes(417-dist_start_km, Salinity)) +
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
  
  p_rb <- ggplot(dat_rb, aes(417-dist_start_km, nirswir1_ratio )) +
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
      x = "Distance from ocean (km)",
      y = "nir / swir1"
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
                         paste0("matchups/matchup_", format(d, "%Y%m%d"), ".jpeg")),
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
                    aes(417-dist_start_km, Salinity, colour = source)) +
  geom_point(size = 1.2, alpha = 0.99) +
  geom_line(aes(group = insitu_date), alpha = 0.3) +
  # scale_color_brewer(palette = "OrRd", name = "|Δt| days") +
  coord_cartesian(xlim = x_range_all) +
  facet_grid(~ insitu_date, labeller = label_both) +
  labs(x = NULL, y = "Salinity (PSU)") +
  theme_bw(base_size = 8) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p_rb_all <- ggplot(RS_full,
                   aes(417-dist_start_km, nirswir1_ratio , color = sensor)) +
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
  labs(x = "Distance from ocean (km)", y = "nir / swir1") +
  theme_bw(base_size = 8)

p_overview <- p_sal_all / p_rb_all +
  plot_layout(heights = c(1.5, 1), guides = "collect")

ggsave(
  filename = file.path(plotpath, "matchups/matchup_all_dates_overview.jpeg"),
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
                         paste0("matchups/feature_screen_", yvar, ".jpeg")),
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

# ggsave(file.path(plotpath, "matchups/feature_screen_by_sensor.pdf"),
#        width = 16, height = 8)


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
    
    exponential = lm(log_y ~ x, data = df), # log(y) ~ x  (no offset/floor)
    
    log_linear = lm(y ~ log_x, data = df),  # y ~ log(x)
    
    gam = gam(y ~ s(x, k = 10), data = df, method = "REML")
  )
  
  # ── NEW: true nonlinear models fit with nls() ─────────────────────────
  # These are added because several candidate relationships (exponential
  # decay with a non-zero floor, hyperbolic/rational decay, and sigmoidal
  # decay) cannot be captured by log-linearization — they need genuine
  # nonlinear least squares with sensible starting values and, for
  # stability, lower/upper bounds via algorithm = "port".
  
  rng_y <- diff(range(y))
  rng_x <- diff(range(x))
  
  # 1. Exponential decay with floor:  y = a * exp(-b*x) + c
  models$exp_decay_offset <- tryCatch(
    nls(
      y ~ a * exp(-b * x) + c,
      data    = df,
      start   = list(a = rng_y, b = 1 / mean(x), c = min(y)),
      lower   = c(a = 0,     b = 0,     c = -abs(min(y))),
      upper   = c(a = Inf,   b = Inf,   c = max(y)),
      algorithm = "port",
      control = nls.control(maxiter = 200, warnOnly = TRUE)
    ),
    error = function(e) { message("exp_decay_offset failed: ", e$message); NULL }
  )
  
  # 2. Hyperbolic / rational decay:  y = a / (1 + b*x) + c
  models$hyperbolic <- tryCatch(
    nls(
      y ~ a / (1 + b * x) + c,
      data    = df,
      start   = list(a = rng_y, b = 1 / mean(x), c = min(y)),
      lower   = c(a = 0,     b = 0,     c = -abs(min(y))),
      upper   = c(a = Inf,   b = Inf,   c = max(y)),
      algorithm = "port",
      control = nls.control(maxiter = 200, warnOnly = TRUE)
    ),
    error = function(e) { message("hyperbolic failed: ", e$message); NULL }
  )
  
  # 3. Logistic (sigmoidal) decay, 4-parameter:
  #    y = a + (d - a) / (1 + exp(b*(x - c)))
  #    d = upper asymptote (low-x plateau), a = lower asymptote (high-x plateau)
  #    c = inflection point, b = steepness (b > 0 for a decreasing curve)
  models$logistic_decay <- tryCatch(
    nls(
      y ~ a + (d - a) / (1 + exp(b * (x - c))),
      data    = df,
      start   = list(a = min(y), d = max(y),
                     b = 1 / max(rng_x, .Machine$double.eps),
                     c = median(x)),
      lower   = c(a = -abs(min(y)), d = 0,   b = 0,    c = min(x)),
      upper   = c(a = max(y),       d = Inf, b = Inf,  c = max(x)),
      algorithm = "port",
      control = nls.control(maxiter = 200, warnOnly = TRUE)
    ),
    error = function(e) { message("logistic_decay failed: ", e$message); NULL }
  )
  
  # drop any nonlinear fits that failed outright
  models <- Filter(Negate(is.null), models)
  
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
                    linear            = predict(mod, df),
                    inverse           = predict(mod, df),
                    poly2             = predict(mod, df),
                    power             = exp(predict(mod, df)),
                    exponential       = exp(predict(mod, df)),
                    log_linear        = predict(mod, df),
                    gam               = predict(mod, df),
                    exp_decay_offset  = predict(mod, df),  # nls already on original scale
                    hyperbolic        = predict(mod, df),
                    logistic_decay    = predict(mod, df)
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
                  linear            = predict(mod, df_new),
                  inverse           = predict(mod, df_new),
                  poly2             = predict(mod, df_new),
                  power             = exp(predict(mod, df_new)),
                  exponential       = exp(predict(mod, df_new)),
                  log_linear        = predict(mod, df_new),
                  gam               = as.numeric(predict(mod, df_new)),
                  exp_decay_offset  = predict(mod, df_new),
                  hyperbolic        = predict(mod, df_new),
                  logistic_decay    = predict(mod, df_new)
  )
  data.frame(x = x_seq, y_hat = y_hat, model = mname)
}


# ── model colours and labels ───────────────────────────────────────────
model_colors <- c(
  linear            = "#333333",
  inverse           = "#E41A1C",
  poly2             = "#377EB8",
  power             = "#4DAF4A",
  exponential       = "#984EA3",
  log_linear        = "#FF7F00",
  gam               = "#A65628",
  exp_decay_offset  = "#F781BF",
  hyperbolic        = "#999999",
  logistic_decay    = "#66C2A5"
)
model_labels <- c(
  linear            = "Linear: y = a + bx",
  inverse           = "Inverse: y = a/x + b",
  poly2             = "Poly2: y = a + bx + cx²",
  power             = "Power: y = ax^b",
  exponential       = "Exponential: y = ae^(bx)",
  log_linear        = "Log-linear: y = a + b·log(x)",
  gam               = "GAM",
  exp_decay_offset  = "Exp decay w/ floor: y = ae^(-bx) + c",
  hyperbolic        = "Hyperbolic: y = a/(1+bx) + c",
  logistic_decay    = "Logistic decay: y = a + (d-a)/(1+e^(b(x-c)))"
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
  
  top6 <- unique(c(screen[insitu_var == yvar][order(-abs_r_spearman)]$feature[1:6], "green","nir_swir2","red_blue","green_blue"))

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
                           paste0("matchups/model_compare_", yvar, "_",
                                  top6[i], ".jpeg")),
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
                         paste0("matchups/model_compare_", yvar, "_overview.pdf")),
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



# ────────────────────────────────────────────────────────────────────────────
# ── SALINITY ~ GREEN : exp_decay_offset MODEL ONLY ────────────────────────
# ────────────────────────────────────────────────────────────────────────────
# y = a * exp(-b*x) + c
# Produces a single annotated plot with the fitted curve and goodness-of-fit
# metrics (R², RMSE, AIC, n) displayed directly on the panel.

plot_exp_decay_fit <- function(feat, yvar, data,
                               point_color = "gray40",
                               line_color  = "#F781BF") {
  
  x <- data[[feat]]
  y <- data[[yvar]]
  
  ok <- is.finite(x) & is.finite(y) & x > 0 & y > 0
  x  <- x[ok]
  y  <- y[ok]
  df <- data.frame(x = x, y = y)
  
  rng_y <- diff(range(y))
  
  # ── fit ──────────────────────────────────────────────────────────────
  mod <- nls(
    y ~ a * exp(-b * x) + c,
    data    = df,
    start   = list(a = rng_y, b = 1 / mean(x), c = min(y)),
    lower   = c(a = 0,   b = 0,   c = -abs(min(y))),
    upper   = c(a = Inf, b = Inf, c = max(y)),
    algorithm = "port",
    control = nls.control(maxiter = 200, warnOnly = TRUE)
  )
  
  co <- coef(mod)
  
  # ── goodness of fit ──────────────────────────────────────────────────
  y_hat   <- predict(mod, df)
  ss_res  <- sum((y - y_hat)^2)
  ss_tot  <- sum((y - mean(y))^2)
  r2      <- 1 - ss_res / ss_tot
  rmse    <- sqrt(mean((y - y_hat)^2))
  aic_val <- AIC(mod)
  n       <- length(y)
  
  # ── prediction curve over a fine grid ───────────────────────────────
  x_seq <- seq(min(x), max(x), length.out = 200)
  y_seq <- predict(mod, data.frame(x = x_seq))
  pred_df <- data.frame(x = x_seq, y_hat = y_seq)
  
  # ── label text for the panel ────────────────────────────────────────
  eq_label <- sprintf(
    "y = %.3f * e^(-%.3f*x) + %.3f\nR² = %.3f   RMSE = %.3f\nAIC = %.1f   n = %d",
    co["a"], co["b"], co["c"], r2, rmse, aic_val, n
  )
  
  # ── plot ─────────────────────────────────────────────────────────────
  p <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.55, size = 1.8, color = point_color) +
    geom_line(data = pred_df, aes(x, y_hat),
              color = line_color, linewidth = 1.1) +
    annotate(
      "label",
      x = max(x), y = max(y),
      label = eq_label,
      hjust = 1, vjust = 1,
      size = 3.2,
      label.size = 0.3,
      fill = "white", alpha = 0.85
    ) +
    labs(
      title = paste0(yvar, " = f(", feat, ")  —  exponential decay with floor"),
      x = feat,
      y = yvar
    ) +
    theme_bw(base_size = 11)
  
  list(plot = p, model = mod,
       metrics = data.table(
         model = "exp_decay_offset",
         n = n, r2 = round(r2, 4),
         RMSE = round(rmse, 4), AIC = round(aic_val, 2),
         a = round(co["a"], 4), b = round(co["b"], 4), c = round(co["c"], 4)
       ))
}

# ── run for Salinity ~ green ────────────────────────────────────────────
result <- plot_exp_decay_fit("green", "Salinity", bm)

print(result$plot)
print(result$metrics)

ggsave(
  filename = file.path(plotpath, "matchups/salinity_green_exp_decay_offset.jpeg"),
  plot = result$plot,
  width = 7, height = 5.5
)








# ────────────────────────────────────────────────────────────────────────────
# ── LINEAR MODEL PLOTS : TSS ~ swir1_blue  AND  DOC ~ swir2_green ─────────
# ────────────────────────────────────────────────────────────────────────────
# y = a + b*x
# Same style as the exp_decay_offset plot: scatter + fitted line +
# goodness-of-fit metrics (R², RMSE, AIC, n) annotated on the panel.

plot_linear_fit <- function(feat, yvar, data,
                            point_color = "gray40",
                            line_color  = "#333333",
                            label_pos   = c("topright", "topleft",
                                            "bottomright", "bottomleft")) {
  
  label_pos <- match.arg(label_pos)
  
  x <- data[[feat]]
  y <- data[[yvar]]
  
  ok <- is.finite(x) & is.finite(y)
  x  <- x[ok]
  y  <- y[ok]
  df <- data.frame(x = x, y = y)
  
  # ── fit ──────────────────────────────────────────────────────────────
  mod <- lm(y ~ x, data = df)
  co  <- coef(mod)
  
  # ── goodness of fit ──────────────────────────────────────────────────
  y_hat   <- predict(mod, df)
  ss_res  <- sum((y - y_hat)^2)
  ss_tot  <- sum((y - mean(y))^2)
  r2      <- 1 - ss_res / ss_tot
  rmse    <- sqrt(mean((y - y_hat)^2))
  aic_val <- AIC(mod)
  n       <- length(y)
  
  # ── prediction line over a fine grid ────────────────────────────────
  x_seq <- seq(min(x), max(x), length.out = 200)
  y_seq <- predict(mod, data.frame(x = x_seq))
  pred_df <- data.frame(x = x_seq, y_hat = y_seq)
  
  # ── label text for the panel ────────────────────────────────────────
  eq_label <- sprintf(
    "y = %.3f + %.3f*x\nR² = %.3f   RMSE = %.3f\nAIC = %.1f   n = %d",
    co["(Intercept)"], co["x"], r2, rmse, aic_val, n
  )
  
  # corner placement for the annotation box
  pos <- switch(label_pos,
                topright    = list(x = max(x), y = max(y), hjust = 1, vjust = 1),
                topleft     = list(x = min(x), y = max(y), hjust = 0, vjust = 1),
                bottomright = list(x = max(x), y = min(y), hjust = 1, vjust = 0),
                bottomleft  = list(x = min(x), y = min(y), hjust = 0, vjust = 0)
  )
  
  # ── plot ─────────────────────────────────────────────────────────────
  p <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.55, size = 1.8, color = point_color) +
    geom_line(data = pred_df, aes(x, y_hat),
              color = line_color, linewidth = 1.1) +
    annotate(
      "label",
      x = pos$x, y = pos$y,
      label = eq_label,
      hjust = pos$hjust, vjust = pos$vjust,
      size = 3.2,
      label.size = 0.3,
      fill = "white", alpha = 0.85
    ) +
    labs(
      title = paste0(yvar, " = f(", feat, ")  —  linear"),
      x = feat,
      y = yvar
    ) +
    theme_bw(base_size = 11)
  
  list(plot = p, model = mod,
       metrics = data.table(
         model = "linear",
         n = n, r2 = round(r2, 4),
         RMSE = round(rmse, 4), AIC = round(aic_val, 2),
         intercept = round(co["(Intercept)"], 4),
         slope = round(co["x"], 4)
       ))
}

# ── run for TSS ~ swir1_blue ────────────────────────────────────────────
result_tss <- plot_linear_fit("swir1_blue", "TSS", bm,
                              label_pos = "topright")

print(result_tss$plot)
print(result_tss$metrics)

ggsave(
  filename = file.path(plotpath, "matchups/tss_swir1_blue_linear.jpeg"),
  plot = result_tss$plot,
  width = 7, height = 5.5
)

# ── run for DOC ~ swir2_green ───────────────────────────────────────────
result_doc <- plot_linear_fit("swir2_green", "DOC", bm,
                              label_pos = "topright")

print(result_doc$plot)
print(result_doc$metrics)

ggsave(
  filename = file.path(plotpath, "matchups/doc_swir2_green_linear.jpeg"),
  plot = result_doc$plot,
  width = 7, height = 5.5
)




