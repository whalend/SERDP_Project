#### qaqc on all camera photos ######

library(readr)
library(dplyr)
library(ggplot2)
library(stringi)

old_with_many_from_wes <- read_csv("data/raw_data/2019_serdp_data/all_camera_photos_with_extra.csv")

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


photos$camera_number[photos$camera_number=="4-2"] <- "4"
photos$camera_number[photos$camera_number=="6-3"] <- "6"
photos$camera_number[photos$camera_number=="8-2"] <- "8"

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


photos$sd_card[photos$sd_card=="4-2-0"] <- "4-2"
photos$sd_card[photos$sd_card=="6-3-0"] <- "6-3"
photos$sd_card[photos$sd_card=="8-2-0"] <- "8-2"
photos$sd_card[photos$sd_card=="28-3-"] <- "28-3"

photos$sd_card[photos$sd_card=="25-3-"] <- "25-3"
#^video sd card
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
#removed hancock n1 cow pasture plot but kept the turkeys

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

photos_combined <- photos_combined[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,24,25,26,28,29,30,31)]

photos_combined <- photos_combined[,c(22,23,24,20,21,25,26,27,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]

colnames(photos_combined)[colnames(photos_combined)=="plot_id.y"] <- "plot_id"
colnames(photos_combined)[colnames(photos_combined)=="status.y"] <- "status"
colnames(photos_combined)[colnames(photos_combined)=="camera_number.x"] <- "camera_number"

photos_combined <- photos_combined %>% 
  filter(installation!="avonpark")

##### MAIN DATA FRAME TO BE MANIPULATED (photos_combined) #####
# need to figure out time manipulations for ever 5 or so minutes to remove chance of repeat individuals as best possible
# "ctime" is uploaded date/time?, "mtime" is date/time photo was actually taken

write_csv(photos_combined, "data/processed_data/2019 serdp processed data/2019-camera-trap-photos-all.csv")

#trapped_days <- read_csv("C:/Users/Steven/Desktop/serdp/testing camera trap stuff/CamearaDays.csv")
#^ read in camera trap days test from drew, true number of days from first photo taken to last photo taken. preferred over camera out/camera in because of failures due to mech/battery dead/sd full in a few days instead of the full range of deployment

# filter(photos_combined, !is.na(other))$other %>% unique(.)
## Done here by using the 'tidyr::gather' function after some other manipulations

##attemping counts/days trapped using file name extractions

#write_csv(photos_days, "data/processed_data/2019-camera-trap-photos-with-days.csv") #additions done in excel, writing to not overwrite old photos_combined

photos_combined <- read_csv("data/processed_data/2019-camera-trap-photos-all-no-days.csv")

sort(unique(photos_combined$sd_card))

# photos_days <- photos_combined %>%
#   select(status, installation, plot_id, sd_card, file_days) #%>%
#   # mutate(file_days = lubridate::ymd(file_days),
#   #        start_date = min(file_days),
#   #        end_date = max(file_days),
#   #        camera_days = end_date - start_date)
# 
# photos_days_nodup <- photos_days[!duplicated(photos_days),]
# 
# before_wes_photos <- photos_days_nodup %>%
#       mutate(file_days = lubridate::ymd(file_days)) %>%
#       group_by(status, installation, plot_id, sd_card) %>%
#       summarise(start_date = min(file_days),
#                 end_date = max(file_days),
#                 camera_days = end_date - start_date)
# 
# write_csv(before_wes_photos, "data/processed_data/camera-trap-days.csv")
#below manual edits done after removing edits

camera_days <- read_csv("data/processed_data/2019 serdp processed data/camera-trap-days-all.csv")
camera_days <- camera_days %>% 
  filter(installation!="avonpark")
#^manual addition of the missing cameras in excel 

test_missing <- anti_join(camera_traps_report
                          ,camera_days)
###### 6 missing sd cards are (4 from avon park removed,  2 no photos not run from machine learning, #####

photo_stats_cow <- photos_combined %>%
  mutate(species = "cow") %>% 
  group_by(status, installation, plot_id, sd_card, species) %>%
  summarise(count = sum(cow))
          
photo_stats_deer <- photos_combined %>%
  mutate(species = "deer") %>%
  group_by(status, installation, plot_id, sd_card, species) %>%
  summarise(count = sum(deer))

photo_stats_turkey <- photos_combined %>%
  mutate(species = "turkey") %>%
  group_by(status, installation, plot_id, sd_card, species) %>%
  summarise(count = sum(turkey))

photo_stats_pig <- photos_combined %>%
  mutate(species = "pig") %>%
  group_by(status, installation, plot_id, sd_card, species) %>%
  summarise(count = sum(pig))

# photo_stats_raccoon <- photos_combined %>%
#   mutate(species = "raccoon") %>%
#   group_by(status, installation, plot_id, sd_card, species) %>%
#   summarise(count = sum(raccoon))
# 
# photo_stats_armadillo <- photos_combined %>%
#   mutate(species = "armadillo") %>%
#   group_by(status, installation, plot_id, sd_card, species) %>%
#   summarise(count = sum(armadillo))
#raccoon at 4 plots, armadillo at 1

species_counts_by_sd_card <- rbind(photo_stats_cow, photo_stats_deer, photo_stats_turkey, photo_stats_pig)

# species_counts_by_sd_card_zero <- species_counts_by_sd_card %>% 
#   filter(count > 0)
#all individuals across native/invaded only main 4 hosts, keeping zeros for now

species_counts_status <- species_counts_by_sd_card %>%
  group_by(status, species) %>%
  summarise(count = sum(count))

count_and_days <- left_join(species_counts_by_sd_card, camera_days)

count_and_days_camera <- count_and_days %>% 
  group_by(status, installation, plot_id, sd_card, species) %>% 
  summarise(count_per_day_camera = count/camera_days)

write_csv(count_and_days_camera, "data/processed_data/2019 serdp processed data/species-per-camera-per-day.csv")
#^counts per day per camera (2 usually)

count_and_days_plot <- count_and_days %>% 
  group_by(status, installation, plot_id, species) %>% 
  summarise(total_plot_days = sum(camera_days),
            total_count = sum(count))

count_and_days_plot <- count_and_days_plot %>% 
  group_by(status, installation, plot_id, species) %>% 
  summarise(count_per_day_plot = total_count/total_plot_days)  
write_csv(count_and_days_plot, "data/processed_data/2019 serdp processed data/species-per-plot-per-day.csv")
#^counts per PLOT per day (summed total days of both cameras and their respective counts)

count_and_days_plot <- read_csv("data/processed_data/2019 serdp processed data/species-per-plot-per-day.csv")

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


status_hosts <- ggplot(data = species_counts_status, mapping = aes(x = status, y = count, color= status, group())) +
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
  geom_point(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Individual counts per sd card') +
  xlab("Host species") +
  invasion_color +
  invasion_fill +
  NULL


species_plot <- ggplot(data = count_and_days_plot, mapping = aes(x = species, y = count_per_day_plot, color= status)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Count per plot per day') +
  xlab("Host species") +
  invasion_color +
  invasion_fill +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
  NULL

#testing turkey only
turkeys <- count_and_days_plot %>% 
  filter(species=="turkey")

only_turkey <- ggplot(data = turkeys, mapping = aes(x = species, y = count_per_day_plot, color= status)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Count per plot per day') +
  xlab("Turkey") +
  invasion_color +
  invasion_fill +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
  NULL
ggsave(plot = only_turkey, "C:/Users/Steven/Desktop/turkey_only_host.png", width = 7, height = 7)


ggsave(plot = species_plot, "C:/Users/Steven/Desktop/species_per_day_plot_level.png", width = 7, height = 7)

species_camera <- ggplot(data = count_and_days_camera, mapping = aes(x = species, y = count_per_day_camera, color= status)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Count per camera per day') +
  xlab("Host species") +
  invasion_color +
  invasion_fill +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom") +
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



#playin with no pig

cd_plot_no_pig <- count_and_days_plot %>% 
  filter(species!="pig")

species_plot_np <- ggplot(data = cd_plot_no_pig, mapping = aes(x = species, y = count_per_day_plot, color= status)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Count per plot per day') +
  xlab("Host species") +
  invasion_color +
  invasion_fill +
  facet_wrap(~species, scales = "free") + 
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
  NULL



cd_cam_no_pig <- count_and_days_camera %>% 
  filter(species!="pig")

species_camera_np <- ggplot(data = cd_cam_no_pig, mapping = aes(x = species, y = count_per_day_camera, color= status)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Count per camera per day') +
  xlab("Host species") +
  invasion_color +
  invasion_fill +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom") +
  NULL

ggsave(plot = species_camera, "C:/Users/Steven/Desktop/host_species_by_camera.png", height = 8, width = 6)
ggsave(plot = species_camera_np, "C:/Users/Steven/Desktop/host_species_by_camera_no_pig.png", height = 4, width = 6)
