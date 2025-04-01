
# ---
# Authors: Camille Minaudo, Timothy Gbadegesin
# Project: "SALBIA"
# date: "July 2024"
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
library(readr)
library(data.table)
library(stringr)


# ---- load data ----

coordinates <- data.frame(lat = c(),
                          long = c(),
                          depth = c())

flow_direction <- "down2up" # down2up or up2dowm

setwd("C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/sampling/data")

data_transect_raw <- readLines("Qliner/Tendaba/T1/Tendaba1.txt")

# data_transect_raw <- as.data.frame(fread("Qliner/Tendaba/T1/Tendaba1.txt", skip = 65, fill = T, blank.lines.skip = T))


# ---- Clean and structure raw file ----
# select velocity field
ind_Vx <- grep(pattern = "Vx_", x = data_transect_raw)
data <- data_transect_raw[ind_Vx]

mydata <- NULL

for(l in seq(1,length(data))){
  myrow <- data[l]
  loc <- unlist(str_locate_all(string = myrow, pattern = " "))
  loc <- loc[seq(1,length(loc)/2)]
  chunk <- loc - seq_along(loc)
  table_chunk <- table(chunk)
  
  data_l <- unlist(str_split(myrow, pattern = " "))
  data_l <- data_l[-which(data_l=="")]
  data_l <- data_l[seq(1, length(data_l)-3)]
  
  ind_gap <- which(as.numeric(table_chunk)>10)
  ind_gap <- ind_gap[which(ind_gap<length(data_l))]
  
  data_f <- as.numeric(data_l)
  
  if(length(ind_gap)>0){
    for(i in ind_gap){
      n_gap <- round(as.numeric(table_chunk)[i]/9)
      
      data_f <- c(data_l[seq(1,ind_gap)],rep(x = "NA", n_gap), data_l[seq(ind_gap+1,length(data_l))])
    }
    data_f <- as.numeric(data_f)
  }
  
  mydata <- rbind(mydata, data_f)
}

mydata <- as.data.frame(mydata[,c(2,seq(3,dim(mydata)[2], 2))])


ggplot(mydata, aes(V1, V3))+geom_path()+scale_x_reverse()+coord_flip()


# get rid of MeanV and Std columns
# dim_data <- dim(data)
# data <- data[,seq(1,dim_data[2]-3)]


# ---- Compute distances ----


# ---- Interpolate velocity field ----


# ---- Calculate total discharge ----





