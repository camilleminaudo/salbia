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
data <- read.csv("pts_Gambia_all.csv")
data$date <- as.Date(data$date)
data$date_dec <- decimal_date(data$date)
data$year <- year(data$date)
data$decade <-  as.factor(paste(floor(data$year/10)*10, "s",sep = ""))
data$month <- month(data$date)
data$doy <-  as.numeric(strftime(data$date, format = "%j"))


data$red_blue <- data$ls_red/data$ls_blue

data$zone <- "upper"
data$zone[data$distance>=0.8] <- "central"
data$zone[data$distance>=1.6] <- "lower"
data$zone <- as.factor(data$zone)

p_year <- ggplot(data, aes(year))+
  geom_histogram(stat="count", aes(fill = zone))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "top")
p_month <- ggplot(data, aes(month))+
  geom_histogram(stat="count", aes(fill = zone))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+theme(legend.position = "none")

plotGIS <- F
if (plotGIS){
  sheds <- sf::read_sf("C:/Projects/myGit/oligotrend/wp1-unify/data/GIS/FAO_AQUASTAT/major_hydrobasins.shp")
  
  mypts <- st_read(paste0(path_repo_root,"/data/GIS/points_along_mainstem.shp"))
  
  world <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
  
  
  mypts$zone <- "upper"
  mypts$zone[mypts$distance>=0.8] <- "central"
  mypts$zone[mypts$distance>=1.6] <- "lower"
  mypts$zone <- as.factor(mypts$zone)
  
  p_gis <- ggplot(data = world) +
    geom_sf(fill= "white") +
    geom_sf(data = sheds, fill = "#F5F5F5", colour = "grey70")+
    geom_sf(data = mypts, size=2, mapping = aes(colour = zone, fill = zone)) +
    scale_colour_manual(values=myPalette)+
    scale_fill_manual(values=myPalette)+
    coord_sf(xlim = c(-18, -14), ylim = c(12, 15), expand = FALSE)+
    theme_void()+theme(legend.position = "none")
  
  
  
  p_temporal_cover <- ggarrange(p_year, p_month, p_gis, nrow = 1)
  
  ggsave(filename = "temporal_coverage.jpeg",
         plot = p_temporal_cover, path = plotpath, width = 15, height = 6, dpi = 300, units = 'in')
} else {
  ggarrange(p_year, p_month, nrow = 1)
}




ggplot(data[data$ls_nir<18000 & data$month<5,], aes(distance, ls_red))+
  geom_point(aes(colour = zone))+
  geom_smooth(method = "loess", aes(group = decade))+
  scale_colour_manual(values=myPalette)+
  scale_fill_manual(values=myPalette)+
  theme_article()+facet_wrap(decade~.)



# ggplot(data[data$red_blue<1.45 & data$month<5,], aes(distance, red_blue))+
#   geom_point(aes(colour = zone))+
#   geom_smooth(method = "loess", aes(group = decade))+
#   scale_colour_manual(values=myPalette)+
#   scale_fill_manual(values=myPalette)+
#   theme_article()+facet_wrap(decade~.)




d = "1990-01-01"

ggplot(data[data$date <= d,], aes(doy, ls_nir))+
  # geom_point()+
  geom_smooth(method = "loess", aes(fill = as.factor(distance), colour = as.factor(distance)), alpha=0.5)+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")+
  theme_article()+
  facet_wrap(as.factor(round(distance))~.)+
  ylim(c(5000,20000))



v_wl <- c("ls_blue","ls_green","ls_nir" , "ls_red","ls_swir1","ls_swir2")

data_spectral <- gather(data[,c("date", "distance",v_wl)], key = band, value = reflectance, -distance, -date)
data_spectral$year <- year(data_spectral$date)

data_spectral$reflectance <- data_spectral$reflectance/10000
data_spectral$flag <- 0
data_spectral$flag[data_spectral$reflectance>1.5 | 
                     data_spectral$reflectance<0.5] <- 1


data_spectral$decade <-  as.factor(paste(floor(data_spectral$year/10)*10, "s",sep = ""))
data_spectral$doy <-  as.numeric(strftime(data_spectral$date, format = "%j"))


ggplot(data_spectral[data_spectral$flag==0,], aes(distance, reflectance))+
  geom_line(aes(group = date), alpha=0.2)+
  # geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")+
  theme_article()+facet_wrap(.~band)






data_upper <- data_spectral[data_spectral$distance < 0.2,]

p_upper <- ggplot(data_upper[data_upper$flag==0,], aes(doy, reflectance, colour = decade))+
  # geom_line(aes(group = date), alpha=0.2)+
  geom_point()+
  geom_smooth(method = "loess")+
  scale_colour_viridis_d(option = "C")+
  # scale_fill_viridis_d(option = "C")+
  theme_article()+facet_wrap(.~band)+ylim(c(0.5,2))+theme(legend.position = 'none')




data_lower <- data_spectral[data_spectral$distance > 2,]

p_lower <- ggplot(data_lower[data_lower$flag==0,], aes(doy, reflectance, colour = decade))+
  # geom_line(aes(group = date), alpha=0.2)+
  geom_point()+
  geom_smooth(method = "loess")+
  scale_colour_viridis_d(option = "C")+
  # scale_fill_viridis_d(option = "C")+
  theme_article()+facet_wrap(.~band)+ylim(c(0.5,2))

ggarrange(p_upper, p_lower, nrow = 1)







# --------------------------------------------------------

# some basic QaQc
qualityCheck <- function(y){
  # outlier detection based on inter-quartile range
  Q_0.25 <- as.numeric(quantile(y, probs = c(0.25), na.rm = T))
  Q_0.75 <- as.numeric(quantile(y, probs = c(0.75), na.rm = T))
  IQR <- Q_0.75-Q_0.25
  
  delta <- 3
  flag_outlier_low <- y < Q_0.25 - delta * IQR
  flag_outlier_upp <- y > Q_0.75 + delta * IQR

  flags <- 0*y
  flags[flag_outlier_low] <- 1
  flags[flag_outlier_upp] <- 1
  
  return(flags)
}


for (band in c("ls_blue","ls_green","ls_nir" , "ls_red","ls_swir1","ls_swir2")){
  message(band)
  data[[paste0(band,"_flag")]] <- qualityCheck(y = data[[band]])
}




isF <- T
for(d in sort(unique(data$date))){
  data_d <- data[data$date == d,]
  data_d <- data_d[data_d$ls_blue_flag==0 | data_d$ls_red_flag==0,]
  # ggplot(data_d, aes(distance, ls_red))+
  #   geom_point(aes(colour = zone))+
  #   geom_smooth(method = "loess")+
  #   scale_colour_manual(values=myPalette)+
  #   scale_fill_manual(values=myPalette)+
  #   theme_article()+facet_wrap(decade~.)
  
  if(sum(data_d$distance<=0.8)>3){
    data_d$red_blue <- data_d$ls_red/data_d$ls_blue
    
    red_blue_upper <- mean(data_d$red_blue[data_d$distance<=0.2], na.rm = T)
    
    data_d$dev_from_upper <- data_d$red_blue - red_blue_upper
    # data_d$red_dev_from_upper <- data_d$ls_red - red_upper
    
    if(isF){
      isF <- F
      data_d.all <- data_d
    } else {
      data_d.all <- rbind(data_d.all, data_d)
    }
  }
}

data_d.all$red_blue_flag <- qualityCheck(data_d.all$red_blue)

p_deviation <- ggplot(data_d.all[data_d.all$red_blue_flag==0,], aes(distance, dev_from_upper))+
  # geom_line(aes(group = date, colour = decade))+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")+
  xlab("distance from upstream [degrees]")+
  ylab("deviation from upper zone")+
  theme_article()#+
  # ylim(c(-1,0.5))+
  # facet_wrap(month~.)
p_deviation

ggsave(filename = "red_blue_deviation.jpeg",
       plot = p_deviation, path = plotpath, width = 10, height = 4, dpi = 300, units = 'in')





