


rm(list = ls()) # clear workspace
cat("/014") # clear console


library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(mgcv)
library(purrr)

library(wql)

# ── paths ──────────────────────────────────────────────────────────────
scriptpath      <- dirname(rstudioapi::getSourceEditorContext()$path)
path_repo_root  <- dirname(scriptpath)
datapath        <- file.path(path_repo_root, "data/gee")
plotpath        <- file.path(path_repo_root, "plots")
gispath        <- file.path(path_repo_root, "data/GIS")
insitu_path <- "C:/Projects/myGit/salbia/data/in_situ"

# ── load data ──────────────────────────────────────────────────────────
# Satellite harmonised reflectance — one row per date × seg_id
dataLS <- fread(file.path(datapath, "salbia_reflectance_aligned_normalized.csv"))
dataLS[, date := as.Date(date)]
dataLS[, rb_ratio := fifelse(blue > 0.001, red / blue, NA_real_)]
dataLS[, gb_ratio := fifelse(blue > 0.001, green / blue, NA_real_)]


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


# ── load in situ sensor data ────────────────────────────────────

long_2024_raw_low<- read.csv(file.path(insitu_path, "August2024_LowRange.csv"),
                             skip = 1,      # skip the Plot Title line
                             header = TRUE, # treat the next line as header
                             quote = "\"",  # proper quote handling
                             sep = ","
)

long_2024_raw_high<- read.csv(file.path(insitu_path, "August2024_HighRange.csv"),
                              skip = 1,      # skip the Plot Title line
                              header = TRUE, # treat the next line as header
                              quote = "\"",  # proper quote handling
                              sep = ","
)

long_2025_raw <- read.csv(file.path(insitu_path, "May2025_Boat.csv"),
                          skip = 1,      # skip the Plot Title line
                          header = TRUE, # treat the next line as header
                          quote = "\"",  # proper quote handling
                          sep = ","
)

long_2024_low <- long_2024_raw_low %>%
  select(2:4) %>% 
  rename(
    Date_GMT02 = 1,
    Conductivity_uS_cm = 2,
    Temperature_C = 3
  )


long_2024_high <- long_2024_raw_high %>%
  select(2:4) %>% 
  rename(
    Date_GMT02 = 1,
    Conductivity_uS_cm = 2,
    Temperature_C = 3
  )

long_2025 <- long_2025_raw %>%
  select(2:4) %>% 
  rename(
    Date_GMT02 = 1,
    Conductivity_uS_cm = 2,
    Temperature_C = 3
  )

# Calculating salinity based on Temp and Cond and using the ec2pss function
# Salinity in PSS

long_2024_low <- long_2024_low %>% mutate(
  # convert µS/cm → mS/cm
  EC_mS_cm = Conductivity_uS_cm / 1000,
  
  # calculate salinity
  Salinity_PSS = ec2pss(EC_mS_cm, Temperature_C, p = 0)
) %>%
  mutate(Date_GMT02 = as.POSIXct(Date_GMT02,
                                 format = "%m/%d/%y %I:%M:%S %p",
                                 tz = "Europe/Berlin")) %>%
  mutate(datetime_utc = Date_GMT02 - lubridate::hours(2))

long_2024_high <- long_2024_high %>% mutate(
  # convert µS/cm → mS/cm
  EC_mS_cm = Conductivity_uS_cm / 1000,
  
  # calculate salinity
  Salinity_PSS = ec2pss(EC_mS_cm, Temperature_C, p = 0)
) %>%
  mutate(Date_GMT02 = as.POSIXct(Date_GMT02,
                                 format = "%m/%d/%y %I:%M:%S %p",
                                 tz = "Europe/Berlin")) %>%
  mutate(datetime_utc = Date_GMT02 - lubridate::hours(2))



long_2025 <- long_2025 %>% mutate(
  # convert µS/cm → mS/cm
  EC_mS_cm = Conductivity_uS_cm / 1000,
  
  # calculate salinity
  Salinity_PSS = ec2pss(EC_mS_cm, Temperature_C, p = 0)
) %>%
  mutate(Date_GMT02 = as.POSIXct(Date_GMT02,
                                 format = "%m/%d/%y %I:%M:%S %p",
                                 tz = "Europe/Berlin")) %>%
  mutate(datetime_utc = Date_GMT02 - lubridate::hours(2))


ggplot(long_2024_high, aes(datetime_utc, Salinity_PSS))+geom_path()+theme_bw()




# ── load in situ grab sampling data ────────────────────────────────────
grabsamples <- read.csv(
  file.path(path_repo_root, "data/in_situ/2026_Final_MasterData_v20260615.csv"),
  skip    = 1,
  dec     = ".",  
  na.strings = c("", "NA", "N/A", "nd", "ND", "-", " ", "#VALUE!")  # catch all common NA codes
)
grabsamples <- grabsamples %>%
  mutate(
    datetime = if_else(
      is.na(hour),
      as.POSIXct(date, format = "%d/%m/%Y", tz = "UTC"),
      as.POSIXct(paste0(date, " ", hour), format = "%d/%m/%Y %H:%M", tz = "Europe/Berlin")
    )
  )





# ── 1. Combine and average sensor tables ──────────────────────────────────────

sensor_combined <- bind_rows(
  long_2024_low  %>% select(datetime_utc, Salinity_PSS, Temperature_C, Conductivity_uS_cm),
  long_2024_high %>% select(datetime_utc, Salinity_PSS, Temperature_C, Conductivity_uS_cm),
  long_2025      %>% select(datetime_utc, Salinity_PSS, Temperature_C, Conductivity_uS_cm)
) %>%
  filter(!is.na(datetime_utc))

# ── 2. 15-min averages ────────────────────────────────────────────────────────

sensor_15min <- sensor_combined %>%
  mutate(datetime_15min = floor_date(datetime_utc, "15 minutes")) %>%
  group_by(datetime_15min) %>%
  summarise(
    Salinity     = mean(Salinity_PSS,         na.rm = TRUE),
    Temperature  = mean(Temperature_C,        na.rm = TRUE),
    Conductivity = mean(Conductivity_uS_cm,   na.rm = TRUE),
    .groups = "drop"
  )

# ── 3. Clean grab samples ─────────────────────────────────────────────────────

grab_coords <- grabsamples %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(datetime)) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  arrange(datetime) %>%
  select(datetime, latitude, longitude, site_id, site_name)

# ── 4. Interpolation function ─────────────────────────────────────────────────
# For a sensor timestamp t:
#   - Find the nearest grab before (t_before) and after (t_after)
#   - Reject if the nearest of the two is more than 1 hour away
#   - Otherwise interpolate position linearly (constant speed assumption)

max_gap_sec <- 60 * 60   # 1 hour

interpolate_position <- function(t, grab) {
  
  diffs_sec <- as.numeric(difftime(t, grab$datetime, units = "secs"))
  
  before_idx <- which(diffs_sec >= 0)
  after_idx  <- which(diffs_sec <  0)
  
  # Need at least one point on each side to interpolate
  if (length(before_idx) == 0 || length(after_idx) == 0) {
    return(data.frame(
      latitude = NA_real_, longitude = NA_real_,
      site_id_before = NA_character_, site_name_before = NA_character_,
      site_id_after  = NA_character_, site_name_after  = NA_character_,
      gap_before_min = NA_real_,      gap_after_min    = NA_real_
    ))
  }
  
  i_before <- before_idx[which.min(diffs_sec[before_idx])]  # latest grab before t
  i_after  <- after_idx[which.max(diffs_sec[after_idx])]    # earliest grab after t
  
  gap_before_sec <- diffs_sec[i_before]        # always >= 0
  gap_after_sec  <- abs(diffs_sec[i_after])    # always >= 0
  
  nearest_gap <- min(gap_before_sec, gap_after_sec)
  
  # Reject if nearest grab is more than 1 hour away
  if (nearest_gap > max_gap_sec) {
    return(data.frame(
      latitude = NA_real_, longitude = NA_real_,
      site_id_before = NA_character_, site_name_before = NA_character_,
      site_id_after  = NA_character_, site_name_after  = NA_character_,
      gap_before_min = round(gap_before_sec / 60, 1),
      gap_after_min  = round(gap_after_sec  / 60, 1)
    ))
  }
  
  # Linear interpolation: weight by time elapsed between the two grabs
  total_sec <- gap_before_sec + gap_after_sec
  w <- gap_before_sec / total_sec   # 0 = at before point, 1 = at after point
  
  data.frame(
    latitude         = (1 - w) * grab$latitude[i_before]  + w * grab$latitude[i_after],
    longitude        = (1 - w) * grab$longitude[i_before] + w * grab$longitude[i_after],
    site_id_before   = grab$site_id[i_before],
    site_name_before = grab$site_name[i_before],
    site_id_after    = grab$site_id[i_after],
    site_name_after  = grab$site_name[i_after],
    gap_before_min   = round(gap_before_sec / 60, 1),
    gap_after_min    = round(gap_after_sec  / 60, 1)
  )
}

# ── 5. Apply to all 15-min rows ───────────────────────────────────────────────

coords_interp <- map_dfr(sensor_15min$datetime_15min, 
                         ~interpolate_position(.x, grab_coords))

sensor_with_coords <- bind_cols(sensor_15min, coords_interp) %>%
  filter(!is.na(latitude))

sensor_with_coords$date <- as.Date(sensor_with_coords$datetime_15min)


ggplot(sensor_with_coords, aes(datetime_15min, Salinity))+
  geom_path()+geom_point()+
  facet_wrap(date~.)+facet_wrap(date~., scales = "free")


sensor_with_coords[sensor_with_coords$date=="2024-08-01",]

# removing obs at the end of profile on 2024-08-01 althought they seem to indicate that salinity inside the mangroves is much lower than in the main river channel
ind_discard <- which(sensor_with_coords$date>="2024-08-01" & sensor_with_coords$site_name_after == "Bansang")
sensor_with_coords <- sensor_with_coords[-ind_discard,]





cat("Rows with interpolated position:", nrow(sensor_with_coords), "\n")
cat("Rows dropped (no grab within 1h):", nrow(sensor_15min) - nrow(sensor_with_coords), "\n")


pal <- colorNumeric("viridis", reverse = F, domain = sensor_with_coords$Salinity)

leaflet(sensor_with_coords) %>%
  addTiles() %>%
  addCircleMarkers(
    lng    = ~longitude,
    lat    = ~latitude,
    color  = ~pal(Salinity),
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>", site_name_before, " → ", site_name_after, "</b><br>",
      format(datetime_15min), "<br>",
      "Salinity: ",     round(Salinity, 2), "<br>",
      "Temperature: ",  round(Temperature, 2), " °C<br>",
      "Gap before: ",   gap_before_min, " min | Gap after: ", gap_after_min, " min"
    )
  )  %>%
  addLegend("bottomright", pal = pal, values = sensor_with_coords$Salinity,
            title = "Salinity (PSS)")





write.csv(sensor_with_coords, file = paste0(insitu_path,"/longitudinal_sensors_clean.csv"))














# ── looking now at patterns in the Rrs dataset ──────────────────────────────────────────────────────────────


# ── build in-situ sf in UTM 28N ────────────────────────────────────────
insitu_sf <- st_as_sf(sensor_with_coords,
                      coords = c("longitude", "latitude"),
                      crs    = 4326) |>
  st_transform(32628)


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

dt_days_max <- 0   # tighten to 0 for same-day only, loosen to 3 if needed

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
       aes(green, Salinity, color = as.factor(sat_date))) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d(name="sat date") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  # labs(title = "Red/Blue ratio vs Salinity",
  #      x = "Salinity (PSU)", y = "Red/Blue ratio") +
  theme_bw()#+facet_wrap(month(sat_date)~.)




unique(best_matchups$sat_date)


d = "2024-08-01"  # "2024-08-01"   "2025-05-25"

dataLS_that_d <- dataLS[dataLS$date == d,]

# range_seg <- range(dataLS_that_d$seg_id)

psat <- ggplot(dataLS_that_d, aes(seg_id, green))+
  geom_path()+geom_point()+facet_wrap(date~.)+xlim(c(0,800))
psat


psal <- ggplot(insitu_joined[insitu_joined$date == d,], aes(seg_id, Salinity))+
  geom_path()+geom_point()+facet_wrap(date~.)+geom_point()+xlim(c(0,800))+ylim(c(0,NA))

library(ggpubr)
ggarrange(psat, psal, ncol = 1, align = "v")










