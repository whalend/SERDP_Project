#### qaqc on taylor's sorted out camera trap photos batch 1 ######

library(readr)
library(dplyr)
library(ggplot2)
library(stringi)

photos <- read_csv("data/raw_data/2019_serdp_data/all_camera_photos.csv")
summary(photos)
#filter(photos, cow >1)

#rearranging column names into something that made more sense because they were all random out of whack
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

photos$camera_number[photos$camera_number=="T4-"] <- "113"
photos$camera_number[photos$camera_number=="23-"] <- "23"
photos$camera_number[photos$camera_number=="35-"] <- "35"
photos$camera_number[photos$camera_number=="38-"] <- "38"
photos$camera_number[photos$camera_number=="42-"] <- "42"
photos$camera_number[photos$camera_number=="9-2"] <- "9"
photos$camera_number[photos$camera_number=="T6-"] <- "114"

photos$camera_number[photos$camera_number=="15-"] <- "15"
photos$camera_number[photos$camera_number=="20-"] <- "20"

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

photos$sd_card[photos$sd_card=="T4-1-"] <- "113-1"
photos$sd_card[photos$sd_card=="21-2-"] <- "21-2"
photos$sd_card[photos$sd_card=="23-1-"] <- "23-1"
photos$sd_card[photos$sd_card=="33-3-"] <- "33-3"
photos$sd_card[photos$sd_card=="34-2-"] <- "34-2"
photos$sd_card[photos$sd_card=="35-1-"] <- "35-1"
photos$sd_card[photos$sd_card=="38-1-"] <- "38-1"
photos$sd_card[photos$sd_card=="42-1-"] <- "42-1"
photos$sd_card[photos$sd_card=="9-2-1"] <- "9-2"
photos$sd_card[photos$sd_card=="T6-1-"] <- "114-1"

photos$sd_card[photos$sd_card=="15-1-"] <- "15-1"
photos$sd_card[photos$sd_card=="20-1-"] <- "20-1"

sort(unique(photos$camera_number))
n_distinct(photos$camera_number)
sort(unique(photos$sd_card))
n_distinct(photos$sd_card)


hancock <- photos %>% 
  filter(sd_card=="112-1")
hancock_2 <- photos %>% 
  filter(sd_card=="14-1") 
hancock_all <- rbind(hancock, hancock_2)
hancock_all <- hancock_all %>% 
  filter(turkey > 0)

photos <- photos %>% 
  filter(sd_card!="112-1") %>% 
  filter(sd_card!="14-1") 

photos <- rbind(photos, hancock_all)
#unique(photos$installation)
#^read in camera trap report to pair status and plot id #####
camera_traps_report <- read_csv("data/raw_data/2019_serdp_data/camera-traps-info.csv")
camera_traps_report <- camera_traps_report[,c(1:9)]

sort(unique(camera_traps_report$sd_card))
sort(unique(camera_traps_report$camera_number))

photos_combined <- left_join(photos, camera_traps_report, by = "sd_card")

#View(photos_combined)
#it frickin worked

#reorganize column names and rename from join
#test2
photos_combined <- photos_combined[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28)]



photos_combined <- photos_combined[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,24,25,26,28,29,30,31)]

photos_combined <- photos_combined[,c(22,23,24,20,21,25,26,27,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]

colnames(photos_combined)[colnames(photos_combined)=="plot_id.y"] <- "plot_id"
colnames(photos_combined)[colnames(photos_combined)=="status.y"] <- "status"
colnames(photos_combined)[colnames(photos_combined)=="camera_number.x"] <- "camera_number"


##### MAIN DATA FRAME TO BE MANIPULATED (photos_combined) #####
# need to figure out time manipulations for ever 5 or so minutes to remove chance of repeat individuals as best possible
# "ctime" is uploaded date/time?, "mtime" is date/time photo was actually taken

write_csv(photos_combined, "data/processed_data/2019-camera-trap-photos-all.csv")

trapped_days <- read_csv("C:/Users/Steven/Desktop/serdp/testing camera trap stuff/CamearaDays.csv")
#^ read in camera trap days test from drew, true number of days from first photo taken to last photo taken. preferred over camera out/camera in because of failures due to mech/battery dead/sd full in a few days instead of the full range of deployment

photos_combined <- left_join(trapped_days, photos_combined, by = "camera_number")
# filter(photos_combined, !is.na(other))$other %>% unique(.)
## Done here by using the 'tidyr::gather' function after some other manipulations

# photos_combined_long <- photos_combined %>%
#       mutate(cattle_egret = case_when(
#             other == "cattle egrets" ~ 2,
#             other == "cattle egret" ~ 1,
#             TRUE ~ 0),
#       empty = 0
#       ) %>%
#       select(installation:sd_card, camera_number, camera_out, camera_in, mtime, empty:cattle_egret, -other) %>%
#       tidyr::gather(species, count, -installation:-mtime)
# unique(p1$count)
# 
# photos_long_stats <- photos_combined_long %>% 
#   group_by(installation, status, species, plot_id, camera_number, sd_card) %>% 
#   summarise(total_count = sum(count),
#             days_trapped = sum(unique(Days)))
# 
# trapped_days <- photos_long_stats %>% 
#   group_by(status, installation, plot_id, camera_number, sd_card) %>% 
#   summarise(total_days = sum(unique(days_trapped)))
# 
# trapped_days$total_days[trapped_days$sd_card=="6-1"] <- 20
# trapped_days$total_days[trapped_days$sd_card=="28-1"] <- 20
# trapped_days$total_days[trapped_days$sd_card=="6-2"] <- 21
# trapped_days$total_days[trapped_days$sd_card=="28-2"] <- 21
# 
# trapped_days$total_days[trapped_days$sd_card=="33-1"] <- 15
# trapped_days$total_days[trapped_days$sd_card=="33-2"] <- 21
# 
# trapped_days$total_days[trapped_days$sd_card=="103-1"] <- 15
# trapped_days$total_days[trapped_days$sd_card=="103-2"] <- 21
# 
# 
# trapped_days$total_days[trapped_days$sd_card=="25-1"] <- 15
# trapped_days$total_days[trapped_days$sd_card=="25-2"] <- 21


# days_by_photos <- left_join(trapped_days, photos_long_stats)
# days_cows <- days_by_photos %>% 
#   filter(species == "cow")
# days_deer <- days_by_photos %>% 
#   filter(species == "deer")
# days_pig <- days_by_photos %>% 
#   filter(species == "pig")
# days_turkey <- days_by_photos %>% 
#   filter(species == "turkey")
# days_cattle_egret <- days_by_photos %>% 
#   filter(species == "cattle_egret")
# 
# days_by_photos <- rbind(days_cows, days_deer, days_pig, days_turkey, days_cattle_egret)
# 
# days_by_photos <- days_by_photos %>%
#   group_by(status, species) %>% 
#   mutate(count_per_day = total_count/total_days) %>%
#   select(status, installation, plot_id, species, count_per_day)
# 
# days_by_photos <- days_by_photos %>% 
#   group_by(status, species) %>% 
#   summarise(species_per_day = sum(count_per_day))
  
  # ungroup(.) %>% 
  # group_by(status) %>% 
  # summarise(mean_days = mean(total_days))

photos_stats <- photos_combined %>%
  group_by(status) %>%
  summarise(total_cow = sum(cow),
            total_deer = sum(deer),
            total_turkey = sum(turkey),
            total_pig = sum(pig))

photo_stats_cow <- photos_combined %>%
  mutate(species = "cow") %>%
  #filter(cow < 2) %>%
  group_by(status, installation, species) %>%
  summarise(count = sum(cow))

photo_stats_deer <- photos_combined %>%
  mutate(species = "deer") %>%
  group_by(status, installation, species) %>%
  summarise(count = sum(deer))

photo_stats_turkey <- photos_combined %>%
  mutate(species = "turkey") %>%
  group_by(status, installation, species) %>%
  summarise(count = sum(turkey))

photo_stats_pig <- photos_combined %>%
  mutate(species = "pig") %>%
  group_by(status, installation, species) %>%
  summarise(count = sum(pig))

species_counts_installation <- rbind(photo_stats_cow, photo_stats_deer, photo_stats_turkey, photo_stats_pig)

#all individuals across native/invaded only main 4 hosts
species_counts_status <- species_counts_installation %>%
  group_by(status, species) %>%
  summarise(count = sum(count))

write_csv(species_counts_installation, "data/processed_data/2019_species_counts_installation.csv")
write_csv(species_counts_status, "data/processed_data/2019_species_counts_status.csv")

# questionable <- photos_combined %>%
#   filter(is.na(status))
#View(questionable)
#fixed a missing entry on camera traps info sheet (25-2 sd card was not there)


#color/theme for figures#####
invasion_color <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red", "deepskyblue"))

species_breakdown <- 
  ggplot() +
  geom_bar(data = species_counts_installation, aes(species, count, color = status, fill = status), stat = "identity", position = "dodge") +
  invasion_color +
  invasion_fill +
  theme_bw() +
  NULL


status_hosts <- ggplot(data = species_counts_status, mapping = aes(x = status, y = count, color= status)) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Photo count of 4 main hosts') +
  xlab("status") +
  invasion_color +
  invasion_fill +
  #theme(legend.position="none") +
  NULL

species <- ggplot(data = species_counts_installation, mapping = aes(x = species, y = count, color= status)) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Raw # of photos per installation') +
  xlab("Host species") +
  invasion_color +
  invasion_fill +
  #theme(legend.position="none") +
  NULL

ggsave(plot = species, "data/raw_data/2019_serdp_data/esa figures/camera_photos_boxplot_species.png", height = 8, width = 8)

ggsave(plot = status_hosts, "data/raw_data/2019_serdp_data/esa figures/camera_photos_boxplot_status.png", height = 8, width = 8)

ggsave(plot = species_breakdown, "data/raw_data/2019_serdp_data/esa figures/camera_photos_bar_species.png", height = 8, width = 8)

#24 cameras in invaded plots and 14 in native
invaded <- photos_combined %>% 
  filter(status=="invaded")
native <- photos_combined %>% 
  filter(status=="native")


###### begin making actual figures used for esa ####

ggplot() +
  geom_bar(data = trapped_days, aes(x = status, y = total_days, color = status, fill = status, position = "dodge"), stat = "identity") +
  invasion_color +
  invasion_fill +
  ylab("Total days trapped") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none") +
  NULL

count_per_day_trapped <- ggplot()+
  geom_bar(data = days_by_photos, aes(x = species, y = species_per_day, group = status, color = status, fill = status), position = "dodge", stat = "identity") +
  invasion_color +
  invasion_fill +
  theme_classic() +
  xlab("Species") +
  ylab("Count/day trapped") +
  theme(legend.position="none") +
  NULL

ggsave(plot = count_per_day_trapped, "data/raw_data/2019_serdp_data/esa figures/count_per_day_trapped.png", height = 7, width = 8)

trapping_days <- ggplot(data = trapped_days, mapping = aes(x = status, y = total_days, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Days trapped per plot') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  NULL

ggsave(plot = trapping_days, "data/raw_data/2019_serdp_data/esa figures/trapping_days_status.png", height = 7, width = 7)
