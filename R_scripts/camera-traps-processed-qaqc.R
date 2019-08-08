#### qaqc on taylor's sorted out camera trap photos batch 1 ######

library(readr)
library(dplyr)
library(ggplot2)
library(stringi)

photos <- read_csv("data/raw_data/2019_serdp_data/TaylorFindsCritters.csv")

summary(photos)
#filter(photos, cow >1)

#rearranging column names into something that made more sense
photos <- photos[,c(4,9,7,11,14,15,18,6,3,5,12,17,1,2,13,16,8,10,19)]

photos <- photos %>% 
  mutate(camera_number = stri_sub(file_name, 1, 3),
         sd_card = stri_sub(file_name, 1, 5),
         plot_id = "test",
         status = "test")


#correcting for consistent camera numbers

photos$camera_number[photos$camera_number=="10-"] <- "10"
photos$camera_number[photos$camera_number=="12-"] <- "12"
photos$camera_number[photos$camera_number=="14-"] <- "14"
photos$camera_number[photos$camera_number=="2-1"] <- "2"
photos$camera_number[photos$camera_number=="21-"] <- "21"
photos$camera_number[photos$camera_number=="22-"] <- "22"
photos$camera_number[photos$camera_number=="25-"] <- "25"
photos$camera_number[photos$camera_number=="27-"] <- "27"
photos$camera_number[photos$camera_number=="28-"] <- "28"
photos$camera_number[photos$camera_number=="33-"] <- "33"
photos$camera_number[photos$camera_number=="34-"] <- "34"
photos$camera_number[photos$camera_number=="37-"] <- "37"
photos$camera_number[photos$camera_number=="4-1"] <- "4"
photos$camera_number[photos$camera_number=="5-1"] <- "5"
photos$camera_number[photos$camera_number=="6-1"] <- "6"
photos$camera_number[photos$camera_number=="6-2"] <- "6"
photos$camera_number[photos$camera_number=="7-1"] <- "7"
photos$camera_number[photos$camera_number=="8-1"] <- "8"
photos$camera_number[photos$camera_number=="9-1"] <- "9"

#correcting for consistent sd card numbers because multiple sd cards on the same camera, when plot was moved/sd was full

photos$sd_card[photos$sd_card=="10-1-"] <- "10-1"
photos$sd_card[photos$sd_card=="12-1-"] <- "12-1"
photos$sd_card[photos$sd_card=="14-1-"] <- "14-1"
photos$sd_card[photos$sd_card=="2-1-1"] <- "2-1"
photos$sd_card[photos$sd_card=="21-1-"] <- "21-1"
photos$sd_card[photos$sd_card=="22-1-"] <- "22-1"

photos$sd_card[photos$sd_card=="25-1-"] <- "25-1"
photos$sd_card[photos$sd_card=="25-2-"] <- "25-2"

photos$sd_card[photos$sd_card=="27-1-"] <- "27-1"

photos$sd_card[photos$sd_card=="28-1-"] <- "28-1"
photos$sd_card[photos$sd_card=="28-2-"] <- "28-2"

photos$sd_card[photos$sd_card=="33-1-"] <- "33-1"
photos$sd_card[photos$sd_card=="33-2-"] <- "33-2"

photos$sd_card[photos$sd_card=="34-1-"] <- "34-1"
photos$sd_card[photos$sd_card=="37-1-"] <- "37-1"
photos$sd_card[photos$sd_card=="4-1-1"] <- "4-1"
photos$sd_card[photos$sd_card=="5-1-1"] <- "5-1"
photos$sd_card[photos$sd_card=="6-1-1"] <- "6-1"
photos$sd_card[photos$sd_card=="6-2-1"] <- "6-2"

photos$sd_card[photos$sd_card=="7-1-1"] <- "7-1"
photos$sd_card[photos$sd_card=="8-1-1"] <- "8-1"
photos$sd_card[photos$sd_card=="9-1-1"] <- "9-1"

unique(photos$camera_number)
n_distinct(photos$camera_number)
unique(photos$sd_card)
n_distinct(photos$sd_card)

#^read in camera trap report to pair status and plot id #####
camera_traps_report <- read_csv("data/raw_data/2019_serdp_data/camera-traps-info.csv")

unique(camera_traps_report$sd_card)

photos_combined <- left_join(photos, camera_traps_report, by = "sd_card")

View(photos_combined)
#it frickin worked

#reorganize column names and rename from join
photos_combined <- photos_combined[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,24,25,26,28,29,30,31)]

photos_combined <- photos_combined[,c(22,23,24,20,21,25,26,27,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]

colnames(photos_combined)[colnames(photos_combined)=="plot_id.y"] <- "plot_id"
colnames(photos_combined)[colnames(photos_combined)=="status.y"] <- "status"
colnames(photos_combined)[colnames(photos_combined)=="camera_number.x"] <- "camera_number"

##creating frames to merge into one for species counts by installation and invasion

photo_stats_deer <- photos_combined %>% 
  mutate(species = "deer") %>% 
  group_by(status, installation, species) %>% 
  summarise(count = sum(deer))

photo_stats_cow <- photos_combined %>% 
  mutate(species = "cow") %>% 
  filter(cow < 2) %>% 
  group_by(status, installation, species) %>% 
  summarise(count = sum(cow))

photo_stats_turkey <- photos_combined %>% 
  mutate(species = "turkey") %>% 
  group_by(status, installation, species) %>% 
  summarise(count = sum(turkey))

photo_stats_pig <- photos_combined %>% 
  mutate(species = "pig") %>% 
  group_by(status, installation, species) %>% 
  summarise(count = sum(pig))

species_counts <- rbind(photo_stats_deer, photo_stats_turkey, photo_stats_pig)

species_counts <- species_counts %>% 
  group_by(status, species) %>% 
  summarise(count = sum(count))

# questionable <- photos_combined %>% 
#   filter(is.na(status))
#View(questionable)
#fixed a missing entry on camera traps info sheet (25-2 sd card was not there)


#color/theme for figures#####
invasion_color <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_fill <- scale_color_manual(values = c("deepskyblue", "red"))

#all individuals across native/invaded only main 4 hosts

ggplot(species_counts, aes(species, count, color = status))+
  geom_point(position = "jitter") +
  # geom_bar(aes(total_pig)) +
  # geom_bar(aes(total_deer)) +
  # geom_bar(aes(total_turkey)) +
  invasion_color +
  theme_bw() +
  NULL

ggplot(species_counts, aes(species, count, color = status))+
  geom_boxplot() +
  # geom_bar(aes(total_pig)) +
  # geom_bar(aes(total_deer)) +
  # geom_bar(aes(total_turkey)) +
  invasion_color +
  theme_bw() +
  NULL

ggplot(photo_stats)

total_cow = sum(cow),
total_deer = sum(deer),
total_pig = sum(pig),
total_turkey = sum(turkey),
total_raccoon = sum(raccoon))
