#### qaqc on taylor's sorted out camera trap photos batch 1 ######

library(readr)
library(dplyr)
library(ggplot2)
library(stringi)

photos <- read_csv("data/raw_data/2019_serdp_data/TaylorFindsCritters.csv")

summary(photos)
filter(photos, cow >1)

photos <- photos[,c(4,9,7,11,14,15,18,6,3,5,12,17,1,2,13,16,8,10,19)]

photos <- photos %>% 
  mutate(camera_number = stri_sub(file_name, 1, 3),
         status = "test")
unique(photos$camera_number)

photos$camera_number[photos$camera_number=="10-"] <- 010
photos$camera_number[photos$camera_number=="12-"] <- 012
photos$camera_number[photos$camera_number=="14-"] <- 014
photos$camera_number[photos$camera_number=="2-1"] <- 002
photos$camera_number[photos$camera_number=="21-"] <- 021
photos$camera_number[photos$camera_number=="22-"] <- 022
photos$camera_number[photos$camera_number=="25-"] <- 025
photos$camera_number[photos$camera_number=="27-"] <- 027
photos$camera_number[photos$camera_number=="28-"] <- 028
photos$camera_number[photos$camera_number=="33-"] <- 033
photos$camera_number[photos$camera_number=="34-"] <- 034
photos$camera_number[photos$camera_number=="37-"] <- 037
photos$camera_number[photos$camera_number=="4-1"] <- 004
photos$camera_number[photos$camera_number=="5-1"] <- 005
photos$camera_number[photos$camera_number=="6-1"] <- 006
photos$camera_number[photos$camera_number=="6-2"] <- 006
photos$camera_number[photos$camera_number=="7-1"] <- 007
photos$camera_number[photos$camera_number=="8-1"] <- 008
photos$camera_number[photos$camera_number=="9-1"] <- 009

