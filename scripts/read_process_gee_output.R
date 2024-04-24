

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

scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
path_repo_root <- dirname(scriptpath) # path to the cloned repo
datapath <- file.path(path_repo_root,"data/gee")

setwd(datapath)
data <- read.csv("pts_Gambia_all.csv")
data$date <- as.Date(data$date)
data$date_dec <- decimal_date(data$date)
data$year <- year(data$date)
data$month <- month(data$date)
data$doy <-  as.numeric(strftime(data$date, format = "%j"))



ggplot(data[data$ls_nir<18000,], aes(distance, ls_nir, colour = month))+
  geom_point()+
  geom_smooth(method = "loess", aes(group = month))+
  scale_colour_viridis_c(option = "C")+
  theme_article()+facet_wrap(year~.)+ylim(c(5000,20000))




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



ggplot(data_spectral[data_spectral$flag==0 & data_spectral$doy>150 & data_spectral$doy<250,], aes(distance, reflectance))+
  # geom_line(aes(group = date), alpha=0.2)+
  geom_smooth(method = "loess", aes(colour = decade, fill = decade))+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")+
  theme_article()+facet_wrap(.~band, scales = "free_y")






