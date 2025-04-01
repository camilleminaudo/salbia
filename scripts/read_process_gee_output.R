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
data <- rbind(dataLS, dataS2)

data$flag_all <- data$blue_flag+data$green_flag+data$nir_flag+data$red_flag+data$swir1_flag+data$swir2_flag

ggplot(data[data$flag_all==0,], aes(blue, fill = satellite))+geom_density(alpha=.5)#+facet_wrap(satellite~.)

data$date <- as.Date(data$date)
data$date_dec <- decimal_date(data$date)
data$year <- year(data$date)
data$decade <-  as.factor(paste(floor(data$year/10)*10, "s",sep = ""))
data$month <- month(data$date)
data$doy <-  as.numeric(strftime(data$date, format = "%j"))

delta_d <- 0.02
table_distance <- data.frame(id = seq(min(data$id), max(data$id)),
                     distance = seq_len(length(seq(min(data$id), max(data$id))))*delta_d-delta_d)

data$distance <- table_distance$distance[match(data$id, table_distance$id)]


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

plotGIS <- F
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



# --------------------------------------------------------


data <- data[data$flag_all==0,]

data$red_blue <- data$red/data$blue


# ggplot(data[data$red_blue<1.45 & data$month<5,], aes(distance, red_blue))+
#   geom_point(aes(colour = zone))+
#   geom_smooth(method = "loess", aes(group = decade))+
#   scale_colour_manual(values=myPalette)+
#   scale_fill_manual(values=myPalette)+
#   theme_article()+facet_wrap(decade~.)




d = "2020-01-01"

ggplot(data[data$date >= d,], aes(distance, red))+
  # geom_point()+
  geom_smooth(method = "loess", 
              aes(fill = satellite, colour = satellite), alpha=0.5, se = F)+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")+
  theme_article()
# +
  # facet_wrap(satellite~., ncol = 1)



# --------------------------------------------------------


v_wl <- c("blue","green","red","nir", "swir1","swir2")

data_spectral <- gather(data[data$flag_all==0,c("date", "distance","satellite",v_wl)], key = band, value = reflectance, -distance, -date, -satellite)
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


data_upper <- data_spectral[data_spectral$distance < 0.2,]
data_upper$zone <- "upstream"

data_lower <- data_spectral[data_spectral$distance > 3,]
data_lower$zone <- "downstream"


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








# --------------------------------------------------------

# repetitive data detection
v_date <- sort((data$date))
counts_repetitive <- table(v_date)
plot(counts_repetitive)

minimum_coverage = 100
well_covered <- as.Date(names(counts_repetitive[which(counts_repetitive>minimum_coverage)]))


tol <- 0 #days
lower_point <- 3

isF <- T
for(d in sort(well_covered)){
  data_d <- data[data$date >= d-tol/2 & data$date <= d+tol/2,]
  data_d <- data_d[data_d$flag_all==0,]
  
  
  # ggplot(data_d, aes(distance, (red)/blue))+
  #   geom_point(aes(colour = satellite))+
  #   geom_smooth(method = "loess")+
  #   theme_article()
  
  if(sum(data_d$distance>=lower_point)>3){
    data_d$red_blue <- data_d$red/data_d$blue
    lower_red_blue <- median(data_d$red_blue[data_d$distance>lower_point], na.rm = T)
    lower_nir <- median(data_d$nir[data_d$distance>lower_point], na.rm = T)
    
    data_d$dev_from_lower_nir <- data_d$nir - lower_nir
    data_d$dev_from_lower_red_blue <- data_d$red_blue - lower_red_blue
    
    
    
    if(d==as.Date("2024-08-01")){
      plot_that_day <- ggplot(data_d, aes(distance, dev_from_lower_nir))+
        geom_line(aes(colour = date))+
        geom_smooth(method = "loess")+
        theme_article()
    }
    
    if(isF){
      isF <- F
      data_d.all <- data_d
    } else {
      data_d.all <- rbind(data_d.all, data_d)
    }
  }
}

plot_that_day


data_d.all$red_blue_flag <- qualityCheck(y = data_d.all$red_blue)
data_d.all$dev_from_lower_nir_flag <- qualityCheck(y = data_d.all$dev_from_lower_nir)
data_d.all$dev_from_lower_red_blue_flag <- qualityCheck(y = data_d.all$dev_from_lower_red_blue)

data_d.all$season <- "wet" # June to October
data_d.all$season[data_d.all$month<6 | data_d.all$month>9] <- "dry"

p_deviation_nir <- ggplot(data_d.all[data_d.all$dev_from_lower_nir_flag==0,], aes(distance, dev_from_lower_nir))+
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
  # ylim(c(-1,0.5))+
# p_deviation_nir


p_deviation_red_blue <- ggplot(data_d.all[data_d.all$dev_from_lower_red_blue_flag==0 & !is.na(data_d.all$dev_from_lower_red_blue),],
                               aes(distance, dev_from_lower_red_blue))+
  geom_abline(slope = 0, intercept = 0)+
  geom_vline(xintercept = lower_point)+
  geom_line(aes(group = date, colour = decade), linewidth=0.2, alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C", begin = 0.1, end = 0.9, direction = 1)+
  scale_fill_viridis_d(option = "C")+
  xlab("distance from upstream [degrees]")+
  ylab("deviation from lower zone")+
  theme_article()+facet_wrap(month~.)+
  ggtitle("Red to Blue ratio")
# ylim(c(-1,0.5))+
# p_deviation_red_blue


ggsave(filename = "nir_deviation.jpeg",
       plot = p_deviation_nir, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')


ggsave(filename = "red_blue_deviation.jpeg",
       plot = p_deviation_red_blue, path = plotpath, width = 12, height = 9, dpi = 300, units = 'in')





