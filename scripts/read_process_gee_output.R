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


myPalette <- c("#66c2a5", "#fc8d62", "#483D8B")


scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
path_repo_root <- dirname(scriptpath) # path to the cloned repo
datapath <- file.path(path_repo_root,"data/gee")
plotpath <- file.path(path_repo_root,"plots")

setwd(datapath)
dataLS <- read.csv("Rrs_gambia_LS5789.csv")
dataLS <- dataLS[,c("date",
                    "distance",
                    "id",
                    "img_id",
                    "ls_blue",
                    "ls_green",
                    "ls_red",
                    "ls_nir",
                    "ls_swir1",
                    "ls_swir2",
                    "satellite")]
names(dataLS) <- c("date",
                   "distance",
                   "id",
                   "img_id",
                   "blue",
                   "green",
                   "red",
                   "nir",
                   "swir1",
                   "swir2",
                   "satellite")

dataS2 <- read.csv("Rrs_gambia_S2.csv")
dataS2 <- dataS2[,c("date",
                    "distance",
                    "id",
                    "img_id",
                    "blue",
                    "green",
                    "red",
                    "nir",
                    "swir1",
                    "swir2",
                    "satellite")]



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


for (band in c("blue","green","nir" , "red","swir1","swir2")){
  message(band)
  dataLS[[band]] <- dataLS[[band]]*0.0000275-0.2 # scaling factors for all landsat products
  dataLS[[paste0(band,"_flag")]] <- qualityCheck(y = dataLS[[band]])
  
  dataS2[[paste0(band,"_flag")]] <- qualityCheck(y = dataS2[[band]])
}

# Merge LS and S2 data into a single df
data_raw <- rbind(dataLS, dataS2)

data_raw$flag_all <- data_raw$blue_flag+data_raw$green_flag+data_raw$nir_flag+data_raw$red_flag+data_raw$swir1_flag+data_raw$swir2_flag

ggplot(data_raw[data_raw$flag_all==0,], aes(blue, fill = satellite))+geom_density(alpha=.5)#+facet_wrap(satellite~.)

data_raw$date <- as.Date(data_raw$date)
data_raw$date_dec <- decimal_date(data_raw$date)
data_raw$year <- year(data_raw$date)
data_raw$decade <-  as.factor(paste(floor(data_raw$year/10)*10, "s",sep = ""))
data_raw$month <- month(data_raw$date)
data_raw$doy <-  as.numeric(strftime(data_raw$date, format = "%j"))

delta_d <- 0.02
table_distance <- data.frame(id = seq(min(data_raw$id), max(data_raw$id)),
                             distance = seq_len(length(seq(min(data_raw$id), max(data_raw$id))))*delta_d-delta_d)

data_raw$distance <- table_distance$distance[match(data_raw$id, table_distance$id)]



# -------------------- REMOVE DUPLUCATED OBSERVATIONS ------------------------------------

data <- data_raw[duplicated(paste0(data_raw$date, data_raw$id, data_raw$satellite)),]

data <- data[data$flag_all==0,]

data$red_blue <- data$red/data$blue
data$rgb_sum <- data$red + data$green + data$blue
data$distanceFromOcean <- data$distance-max(data$distance)

# ggplot(data[data$red_blue<1.45 & data$month<5,], aes(distance, red_blue))+
#   geom_point(aes(colour = zone))+
#   geom_smooth(method = "loess", aes(group = decade))+
#   scale_colour_manual(values=myPalette)+
#   scale_fill_manual(values=myPalette)+
#   theme_article()+facet_wrap(decade~.)


dmin = "2025-01-01"
dmax = "2025-02-01"

ggplot(data[data$date >= dmin & data$date <= dmax,], aes(distanceFromOcean, blue))+
  geom_point()+
  geom_smooth(method = "loess", 
              aes(fill = satellite, colour = satellite), alpha=0.5, se = F)+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")+
  xlab("Distance from ocean [degrees]")+
  theme_article()
# +
# facet_wrap(satellite~., ncol = 1)




# -------------------- PLOT BASIC INFO ABOUT DATABASE ------------------------------------

data$zone <- "upper"
data$zone[data$distance>=1.5] <- "central"
data$zone[data$distance>=2.5] <- "lower"
data$zone <- factor(data$zone, levels = c("upper","central","lower"))

ggplot(data[data$flag_all==0,], aes(nir, fill = zone))+geom_density(alpha=.5)+facet_wrap(satellite~.)



p_year <- ggplot(data, aes(year))+
  geom_histogram(stat="count", aes(fill = zone))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "top")

p_sats <- ggplot(data, aes(year))+
  geom_histogram(stat="count", aes(fill = satellite))+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  theme_article()+theme(legend.position = "top")

ggsave(filename = "satellites_coverage.jpeg",
       plot = p_sats, path = plotpath, width = 8, height = 5, dpi = 300, units = 'in')


p_month <- ggplot(data, aes(month))+
  geom_histogram(stat="count", aes(fill = zone))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "none")+scale_x_continuous(breaks = seq(1,12), labels = seq(1,12))


p_distance <- ggplot(data, aes(distance))+
  geom_histogram(stat="count", aes(fill = zone))+
  xlab("Distance from upstream point")+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "none")

plotGIS <- T
if (plotGIS){
  sheds <- sf::read_sf("C:/Projects/myGit/oligotrend/wp1-unify/data/GIS/FAO_AQUASTAT/major_hydrobasins.shp")
  
  mypts <- st_read(paste0(path_repo_root,"/data/GIS/points_along_mainstem_02.shp"))
  
  mypts$distance <- table_distance$distance[match(mypts$id, table_distance$id)]
  
  
  world <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
  
  
  mypts$zone <- "upper"
  mypts$zone[mypts$distance>=1.5] <- "central"
  mypts$zone[mypts$distance>=2.5] <- "lower"
  mypts$zone <- as.factor(mypts$zone)
  
  p_gis <- ggplot(data = world) +
    geom_sf(fill= "white") +
    geom_sf(data = sheds, fill = "#F5F5F5", colour = "grey70")+
    geom_sf(data = mypts, size=2, mapping = aes(colour = zone, fill = zone)) +
    scale_colour_manual(values=myPalette)+
    scale_fill_manual(values=myPalette)+
    coord_sf(xlim = c(-18, -14), ylim = c(12, 15), expand = FALSE)+
    theme_void()+theme(legend.position = "none")
  
  ggsave(filename = "spatial_coverage.jpeg",
         plot = p_gis, path = plotpath, width = 8, height = 5, dpi = 300, units = 'in')
  
  p_temporal_cover <- ggarrange(p_gis, p_distance, p_year, p_month, nrow = 2, ncol = 2)
  
  ggsave(filename = "temporal_coverage.jpeg",
         plot = p_temporal_cover, path = plotpath, width = 15, height = 10, dpi = 300, units = 'in')
} else {
  ggarrange(p_year, p_month, nrow = 1)
}



# -------------------- HARMONIZE SENSORS ------------------------------------

tab_repetitions <- as.data.frame(table(paste0(data$date,"-",data$id)))

which_repet <- which(tab_repetitions$Freq>1)

data_hrmnz <- NULL
for (k in which_repet){
  data_hrmnz <- rbind(data_hrmnz,
                      data[which(paste0(data$date,"-",data$id) == tab_repetitions$Var1[k]),]) 
}


# library(dplyr)
# library(tidyr)

df_wide <- data_hrmnz %>%
  pivot_wider(
    id_cols = c(date, distance),
    names_from = satellite,
    values_from = c(blue, green, red, nir, swir1, swir2),
    names_glue = "{.value}_{satellite}",
    values_fn = mean
  )

library(stringr)

# Convert wide table back to long tidy format
df_long <- df_wide %>%
  pivot_longer(
    cols = -c(date, distance),
    names_to = c("band", "sensor"),
    names_pattern = "([^_]+)_(.*)",
    values_to = "value"
  ) %>%
  mutate(
    sensor = case_when(
      str_detect(sensor, "^Sentinel-2") ~ "SENTINEL_2",
      TRUE ~ sensor
    )
  )


# Extract Sentinel2 reference values
ref <- df_long %>%
  filter(sensor == "SENTINEL_2") %>%
  rename(ref_value = value) %>%
  select(date, distance, band, ref_value)

# Join reference with all other sensors
df_compare <- df_long %>%
  filter(sensor != "SENTINEL_2") %>%
  left_join(ref,
            by = c("date", "distance", "band"))

head(df_compare)


ggplot(df_compare,
       aes(ref_value, value)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed") +
  facet_grid(band ~ sensor, scales = "free") +
  labs(
    x = "SENTINEL_2",
    y = "Other sensor"
  ) +
  theme_bw()

ggplot(df_compare,
       aes(ref_value, value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed") +
  facet_grid(band ~ sensor, scales = "free") +
  labs(
    x = "SENTINEL_2",
    y = "Other sensor"
  ) +
  theme_bw()



df_compare %>%
  group_by(sensor, band) %>%
  summarise(
    r = cor(ref_value, value, use = "complete.obs"),
    bias = mean(value - ref_value, na.rm = TRUE),
    rmse = sqrt(mean((value - ref_value)^2, na.rm = TRUE)),
    .groups = "drop"
  )

library(broom)

lm_table <- df_compare %>%
  group_by(band, sensor) %>%
  group_modify(~{
    
    mod <- lm(value ~ ref_value, data = .x)
    
    tidy_mod <- broom::tidy(mod)
    glance_mod <- broom::glance(mod)
    
    tibble(
      intercept = tidy_mod$estimate[tidy_mod$term == "(Intercept)"],
      slope     = tidy_mod$estimate[tidy_mod$term == "ref_value"],
      p_value   = tidy_mod$p.value[tidy_mod$term == "ref_value"],
      r_squared = glance_mod$r.squared,
      n         = glance_mod$nobs
    )
  }) %>%
  ungroup()
lm_table


# Now apply model coefficients to correct each observation using SENTINEL-2 as reference

sensor_map <- tibble::tibble(
  satellite = c("LANDSAT_5", "LANDSAT_7", "LANDSAT_8", "LANDSAT_9"),
  sensor_for_model = c("LANDSAT_7", "LANDSAT_7", "LANDSAT_8", "LANDSAT_9")
)

data2 <- data %>%
  left_join(sensor_map, by = "satellite")


data_long <- data2 %>%
  pivot_longer(
    cols = c(blue, green, red, nir, swir1, swir2),
    names_to = "band",
    values_to = "value"
  )
data_long$sensor = data_long$satellite

data_long <- data_long %>%
  mutate(
    sensor = case_when(
      str_detect(sensor, "^Sentinel-2") ~ "SENTINEL_2",
      TRUE ~ sensor
    )
  )


data_harmonized <- data_long %>%
  left_join(
    lm_table,
    by = c("band", "sensor_for_model" = "sensor")
  )

data_harmonized <- data_harmonized %>%
  mutate(
    value_harmonized = intercept + slope * value
  )

data_harmonized <- data_harmonized %>%
  mutate(
    value_harmonized = if_else(
      sensor == "SENTINEL_2",
      value,
      value_harmonized
    )
  )

data_final <- data_harmonized %>%
  select(date, distance, id, img_id, sensor, band, value_harmonized) %>%
  pivot_wider(
    names_from = band,
    values_from = value_harmonized
  )

ggplot(data_harmonized %>% filter(band == "blue"),
       aes(value, value_harmonized, color = sensor)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()



# save data_final
setwd(datapath)
write.csv(x = data_final, file = "salbia_LS_reflectance_harmonized.csv")


# ----------------------- SPECTRAL SHAPES ---------------------------------


v_wl <- c("blue","green","red","nir", "swir1","swir2")

data_spectral <- gather(data_final[,c("date", "distance","sensor",v_wl)], key = band, value = reflectance, -distance, -date, -sensor)
data_spectral$year <- year(data_spectral$date)

data_spectral$reflectance <- data_spectral$reflectance
data_spectral$decade <-  as.factor(paste(floor(data_spectral$year/10)*10, "s",sep = ""))
data_spectral$doy <-  as.numeric(strftime(data_spectral$date, format = "%j"))
data_spectral$month <-  as.numeric(month(data_spectral$date))


# ggplot(data_spectral, aes(distance, reflectance))+
#   geom_line(aes(group = date), alpha=0.2)+
#   # geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
#   scale_colour_viridis_d(option = "C")+
#   scale_fill_viridis_d(option = "C")+
#   theme_article()+facet_wrap(.~band)



# Season

data_spectral$season <- "wet" # June to October
data_spectral$season[data_spectral$month<6 | data_spectral$month>9] <- "dry"

data_spectral$band <- factor(data_spectral$band, levels = v_wl)


# upstream and downstream zones
upstream.limit = 0.8
downstream.limit = 3


data_upper <- data_spectral[data_spectral$distance < upstream.limit,]
data_upper$zone <- "upstream"
data_lower <- data_spectral[data_spectral$distance > downstream.limit,]
data_lower$zone <- "downstream"


data_extremes <- rbind(data_upper, data_lower)


ggplot(
  rbind(data_upper, data_lower), aes(band, reflectance, colour = zone))+
  # geom_line(aes(group = date), alpha=0.2)+
  # geom_point()+
  geom_boxplot()+
  # geom_smooth(method = "loess")+
  scale_colour_viridis_d(option = "C", end = 0.8)+
  # scale_fill_viridis_d(option = "C")+
  theme_article()+
  # ylim(c(0.5,2))+
  facet_grid(season~.)




# Order bands for plotting
band_levels <- c("blue", "green", "red", "nir", "swir1", "swir2")

spec_summary <- data_extremes %>%
  mutate(band = factor(band, levels = band_levels)) %>%
  group_by(zone, band) %>%
  summarise(
    median_ref = median(reflectance, na.rm = TRUE),
    q25 = quantile(reflectance, 0.25, na.rm = TRUE),
    q75 = quantile(reflectance, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(spec_summary,
       aes(x = band,
           y = median_ref,
           color = zone,
           group = zone)) +
  
  geom_ribbon(aes(ymin = q25,
                  ymax = q75,
                  fill = zone),
              alpha = 0.2,
              color = NA) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  labs(
    x = "Spectral band",
    y = "Reflectance",
    title = "Spectral signature of river zones"
  ) +
  
  theme_bw()


# Seasonal variations of spectral signatures

season_summary <- data_extremes %>%
  mutate(
    band = factor(band, levels = band_levels)
  ) %>%
  group_by(zone, season, band) %>%
  summarise(
    median_ref = median(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(season_summary,
       aes(band,
           median_ref,
           color = zone,
           group = zone)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  facet_wrap(~season) +
  
  labs(
    x = "Band",
    y = "Median reflectance",
    title = "Seasonal spectral signatures"
  ) +
  
  theme_bw()


# monthly median reflectance / LONG TERM TRENDS
library(lubridate)

time_summary <- data_extremes %>%
  mutate(
    month_date = floor_date(date, "month"),
    band = factor(band, levels = band_levels)
  ) %>%
  group_by(month_date, zone, band) %>%
  summarise(
    median_ref = median(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(time_summary,
       aes(month_date,
           median_ref,
           color = zone)) +
  
  geom_line(alpha = 0.5) +
  
  geom_smooth(se = FALSE,
              linewidth = 1.2) +
  
  facet_wrap(~band, scales = "free_y") +
  
  labs(
    x = "Date",
    y = "Reflectance",
    title = "Temporal evolution of spectral reflectance"
  ) +
  
  theme_bw()


# ----------------------- SPATIAL PATTERNS ---------------------------------

# We work with harmonized data across sensors



rb_data <- data_extremes %>%
  
  filter(band %in% c("red", "blue")) %>%
  
  group_by(date, distance, zone, band) %>%
  
  summarise(
    reflectance = median(reflectance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    names_from = band,
    values_from = reflectance
  ) %>%
  
  mutate(
    rb_ratio = red / blue
  ) %>%
  filter(is.finite(rb_ratio))


rb_data <- rb_data %>%
  mutate(
    date_num = as.numeric(date)
  )

rb_data$doy <- lubridate::yday(rb_data$date)
rb_data$year <- lubridate::year(rb_data$date)


library(mgcv)
# GAM model with seasonal features
gam_rb <- bam(
  rb_ratio ~
    
    # spatial structure
    s(distance, k = 10) +
    
    # seasonal cycle
    s(doy, bs = "cc", k = 10) +
    
    # long-term evolution
    s(year, k = 8) +
    
    # spatial-seasonal interaction
    ti(distance, doy,
       bs = c("tp", "cc"),
       k = c(20, 20)),
  
  data = rb_data,
  
  method = "fREML",
  
  discrete = TRUE,
  
  nthreads = 8
)




# interpolation
pred_grid <- expand.grid(
  distance = seq(min(rb_data$distance),
                 max(rb_data$distance),
                 length.out = 200),
  
  date_num = seq(min(rb_data$date_num),
                 max(rb_data$date_num),
                 length.out = 300)
)

pred_grid$date <- as.Date(pred_grid$date_num,
                          origin = "1970-01-01")
pred_grid$doy <- lubridate::yday(pred_grid$date)
pred_grid$year <- lubridate::year(pred_grid$date)


pred_grid$rb_pred <- predict(
  gam_rb,
  newdata = pred_grid
)


ggplot(pred_grid,
       aes(x = date,
           y = distance,
           fill = rb_pred)) +
  
  geom_raster(interpolate = TRUE) +
  
  scale_fill_viridis_c(
    option = "turbo",
    name = "Red / Blue"
  ) +
  
  labs(
    x = "Date",
    y = "Distance along river (km)",
    title = "Spatiotemporal dynamics of Red/Blue ratio"
  ) +
  
  theme_bw()












# repetitive data detection
v_date <- sort(data_final$date[which(data_final$distance>1 & data_final$distance<3)])
counts_repetitive <- table(v_date)
plot(counts_repetitive)

minimum_coverage = 30
well_covered <- as.Date(names(counts_repetitive[which(counts_repetitive>minimum_coverage)]))



tol <- 0 #days
lower_point <- 3

library(mgcv)


isF <- T
df_Rpeak <- NULL
for(d in sort(well_covered)){
  d = as.Date(d)
  data_d <- data_final[data_final$date >= d-tol/2 & data_final$date <= d+tol/2,]
  data_d <- data_d[data_d$blue>0,]
  
  
  # if(sum(data_d$distance>=lower_point)>3){
  
  
  data_d$red_blue <- data_d$red/data_d$blue
  lower_red_blue <- median(data_d$red_blue[data_d$distance>lower_point], na.rm = T)
  lower_nir <- median(data_d$nir[data_d$distance>lower_point], na.rm = T)
  data_d$nir_red <- data_d$nir/data_d$red # should be related to chl-a
  lower_nir_red <- median(data_d$nir_red[data_d$distance>lower_point], na.rm = T)
  
  data_d$dev_from_lower_nir <- data_d$nir - lower_nir
  data_d$dev_from_lower_red_blue <- data_d$red_blue - lower_red_blue
  data_d$dev_from_lower_nir_red <- data_d$nir_red - lower_nir_red
  
  
  # smooth_RB <- smooth.spline(x = data_d$distance, y=data_d$red_blue, spar = .6)
  # smooth_RB_sel <- data.frame(x = smooth_RB$x[smooth_RB$x>1 & smooth_RB$x < 3],
  #                             y = smooth_RB$y[smooth_RB$x>1 & smooth_RB$x < 3])
  
  
  
  # We consider that peak must be between distance = 1 and 2.5
  data_d_restrict <- data_d[which(data_d$distance>1 & data_d$distance<2.5),]
  
  # GAM model to identify peak
  mod <- gam(
    red_blue ~ s(distance, k = 20),
    data = data_d_restrict
  )
  newd <- data.frame(
    distance = seq(min(data_d_restrict$distance),
                   max(data_d_restrict$distance),
                   length.out = 1000)
  )
  
  newd$pred <- predict(mod, newd)
  peak <- newd %>%
    slice_max(pred, n = 1)
  
  
  halfmax <- max(newd$pred) / 2
  
  width <- range(newd$distance[newd$pred >= halfmax])
  
  FWHM <- diff(width) # Full Width Half Maximum (FWHM) to characterize spread of peak
  
  # newd$slope <- c(NA, diff(newd$pred) / diff(newd$distance))
  
  df_Rpeak <- rbind(df_Rpeak,
                    data.frame(metric = "red2blue",
                               date = as.Date(first(data_d$date)),
                               peak = peak$pred,
                               dist_peak = peak$distance,
                               FWHM = FWHM))
  
  
  ggplot(data_d) +
    geom_point(aes(distance, red_blue, colour = sensor), alpha = 0.5) +
    geom_line(data = newd,
              aes(distance, pred),
              linewidth = 1.2) +
    geom_vline(xintercept = peak$distance,
               linetype = "dashed") +
    theme_bw()+facet_wrap(date~.)
  
  # ggplot(data_d, aes(distance, dev_from_lower_red_blue))+
  #   # geom_point(aes(colour = sensor))+
  #   geom_point(data = smooth_RB_sel, aes(x, y-lower_red_blue))+
  #   geom_smooth(method = "loess")+
  #   theme_article()
  
  
  if(d==as.Date("2024-08-01")){
    plot_that_day <- ggplot(data_d, aes(distance, dev_from_lower_red_blue))+
      geom_point(aes(colour = date))+
      geom_smooth(method = "loess")+
      theme_article()
  }
  
  if(isF){
    isF <- F
    data_d.all <- data_d
  } else {
    data_d.all <- rbind(data_d.all, data_d)
  }
  # }
}

df_Rpeak$doy <-  as.numeric(strftime(df_Rpeak$date, format = "%j"))
df_Rpeak$month <- month(df_Rpeak$date)
df_Rpeak$year <- year(df_Rpeak$date)
df_Rpeak$decade <-  as.factor(paste(floor(df_Rpeak$year/10)*10, "s",sep = ""))

df_Rpeak$season <- "dry"
df_Rpeak$season[which(df_Rpeak$month>5 & df_Rpeak$month < 11)] <- "wet"


df_Rpeak$before2010 <- "yes"
df_Rpeak$before2010[df_Rpeak$year>=2010] <- "no"

ggplot(df_Rpeak, aes(doy, dist_peak))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "loess", se = F)+
  geom_point(aes(colour = decade))+theme_article()+ylim(c(0,NA))+geom_hline(yintercept = c(1.3, 1.95))

(1.95-1.3)*111


ggplot(df_Rpeak, aes(year, dist_peak))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  geom_smooth(method = "lm", se = F)+
  geom_point()+theme_article()+facet_wrap(month~.)



# plot_that_day




data_d.all$doy <-  as.numeric(strftime(data_d.all$date, format = "%j"))
data_d.all$month <- month(data_d.all$date)
data_d.all$year <- year(data_d.all$date)
data_d.all$decade <-  as.factor(paste(floor(data_d.all$year/10)*10, "s",sep = ""))

data_d.all$season <- "dry"
data_d.all$season[which(data_d.all$month>5 & data_d.all$month < 11)] <- "wet"

data_d.all$red_blue_flag <- qualityCheck(y = data_d.all$red_blue)
data_d.all$dev_from_lower_nir_flag <- qualityCheck(y = data_d.all$dev_from_lower_nir)
data_d.all$dev_from_lower_red_blue_flag <- qualityCheck(y = data_d.all$dev_from_lower_red_blue)
data_d.all$dev_from_lower_nir_red_flag <- qualityCheck(y = data_d.all$dev_from_lower_nir_red)

data_d.all$season <- "dry"
data_d.all$season[data_d.all$month>5 & data_d.all$month<11] <- "wet" # June to October

# p_deviation_nir <- ggplot(data_d.all[data_d.all$dev_from_lower_nir_flag==0,], aes(distance, dev_from_lower_nir))+
#   geom_abline(slope = 0, intercept = 0)+
#   geom_vline(xintercept = lower_point)+
#   geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
#   geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
#   scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
#   scale_fill_viridis_d(option = "C")+
#   xlab("distance from upstream [degrees]")+
#   ylab("deviation from lower zone")+
#   theme_article()+facet_wrap(month~.)+
#   ggtitle("Near Infra Red (NIR)")
#   # ylim(c(-1,0.5))+
# # p_deviation_nir
# 
# 
p_deviation_red_blue <- ggplot(data_d.all[data_d.all$red_blue_flag==0 & !is.na(data_d.all$dev_from_lower_red_blue),],
                               aes(distance, dev_from_lower_red_blue))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("distance from upstream [degrees]")+
  ylab("deviation from lower zone")+
  theme_article()+#facet_wrap(month~.)+
  ggtitle("Red to Blue ratio")+ylim(c(-1,0.5))
p_deviation_red_blue


# 
# 
# ggsave(filename = "nir_deviation.jpeg",
#        plot = p_deviation_nir, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')
# 
# 
# ggsave(filename = "red_blue_deviation.jpeg",
#        plot = p_deviation_red_blue, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')




p_deviation_nir_L5L7 <- ggplot(data_d.all[data_d.all$dev_from_lower_nir_flag==0 & (data_d.all$satellite=="LANDSAT_5" | data_d.all$satellite=="LANDSAT_7"),], aes(distance, dev_from_lower_nir))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("distance from upstream [degrees]")+
  ylab("deviation from lower zone")+
  theme_article()+facet_wrap(month~.)+
  ggtitle("Near Infra Red (NIR)")


ggsave(filename = "nir_deviation_L5L7.jpeg",
       plot = p_deviation_nir_L5L7, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')


ggplot(data_d.all, aes(distance, red_blue))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("distance from upstream [degrees]")+
  ylab("deviation from lower zone")+
  theme_article()+facet_wrap(month~.)+
  ggtitle("Red to Blue ratio")+ylim(c(0,2))

ggsave(filename = "red_blue_deviation.jpeg",
       plot = p_deviation_red_blue, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')




p_deviation_nir_red_L5L7 <- ggplot(data_d.all[data_d.all$satellite=="LANDSAT_5" | data_d.all$satellite=="LANDSAT_7",], aes(distance, nir_red))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("distance from upstream [degrees]")+
  ylab("deviation from lower zone")+
  theme_article()+facet_wrap(month~.)+
  ggtitle("NIR to Red ratio")+ylim(c(0,2))

ggsave(filename = "nir_red_deviation_L5L7.jpeg",
       plot = p_deviation_nir_red_L5L7, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')



