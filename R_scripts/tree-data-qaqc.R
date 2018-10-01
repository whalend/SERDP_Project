#' # Script for doing QA/QC on tree data

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#+ load processed plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_csv("data/raw_data/trees.csv")
### tree_data_sub <- filter(tree_data, date>"2017-06-01", plot_id != "bland03", plot_id != "bland02")

summary(tree_data)

filter(tree_data, is.na(plot_id)) %>%
  select(date, installation, plot_id, record_id, stem_id, dbh, tag, species)

tree_data = filter(tree_data, !is.na(plot_id))

filter(tree_data, is.na(dbh)) %>%
  select(date, plot_id, record_id, stem_id, dbh, tag, species, health)

filter(tree_data, dbh==0) %>%
  select(date, plot_id, stem_id, tag, species, dbh)

tree_data$dbh[tree_data$tag=="1138"] <- 15.2
tree_data$dbh[tree_data$tag=="1139"] <- 14.0

### Corrected two tree DBH that were entered as 0          ###
### Two tree DBH are to be left NA, trees burned up/downed ###

filter(tree_data, dbh<3) %>%
  select(date, plot_id, stem_id, tag, species, dbh)

tree_data$dbh[tree_data$tag=="3056"] <- 25.7
tree_data$dbh[tree_data$tag=="4395"] <- 11.4

### Changed trees with dbh that were <3 with estimated values ###


tree_data$plot_id[tree_data$plot_id=="cogon plot"] <- "theater_cogon"

tree_data <- tree_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

filter(tree_data, is.na(azimuth), plot_id!="blanding theater_cogon") %>%
  select(date, stem_id, plot_id, distance, azimuth, species, tag, dbh)

filter(tree_data, is.na(azimuth), plot_id=="blanding theater_cogon") %>%
  select(date, stem_id, installation, plot_id, distance, azimuth, species, tag, dbh)

tree_data$azimuth[tree_data$tag=="4092"] <- 320
tree_data$azimuth[tree_data$tag=="4402"] <- 337
tree_data$azimuth[tree_data$tag=="4409"] <- 340
tree_data$azimuth[tree_data$tag=="4380"] <- 27
tree_data$azimuth[tree_data$tag=="4389"] <- 37
tree_data$azimuth[tree_data$tag=="4397"] <- 70
tree_data$azimuth[tree_data$tag=="4396"] <- 91
tree_data$azimuth[tree_data$tag=="4450"] <- 127
tree_data$azimuth[tree_data$tag=="4449"] <- 163
tree_data$azimuth[tree_data$stem_id=="2793"] <- 161

tree_data$azimuth[tree_data$tag=="3447"] <- 23.8

tree_data$azimuth[tree_data$stem_id=="3072"] <- 299.5
tree_data$azimuth[tree_data$stem_id=="3078"] <- 307.5
tree_data$azimuth[tree_data$stem_id=="3079"] <- 307.7

filter(tree_data, azimuth=="-3") %>%
  select(record_id, date, plot_id, tag, species, dbh, azimuth)

tree_data$azimuth[tree_data$tag=="3766"] <- 0

filter(tree_data, azimuth=="-1") %>%
  select(record_id, date, plot_id, tag, species, dbh, azimuth)

tree_data$azimuth[tree_data$tag=="3765"] <- 0

## Added azimuth values for trees in Benning A1/J1 and Gordon V1 ##
## Stem IDs 1199 and 1807 have no azimuth recorded ##

unique(plot_visit_data$plot_id) %in% unique(tree_data$plot_id)

n_distinct(tree_data$plot_id)
n_distinct(plot_visit_data$plot_id)

summary(tree_data)

unique(tree_data$plot_id)

filter(tree_data, is.na(health), plot_id!="blanding theater_cogon") %>%
  select(installation, plot_id, date, tag, species, health, canopy)

tree_data$health[tree_data$plot_id=="jackson m1" & tree_data$tag=="3235"] <- 0

tree_data$health[tree_data$tag=="3888"] <- 0

## Added 0 values to tree health for 3235 and 3888 ##
## No health or canopy entered for tag 4083 ##


filter(tree_data, is.na(distance), plot_id!="blanding theater_cogon") %>%
  select(installation, plot_id, date, tag, species, distance)

tree_data$distance[tree_data$tag=="1755"] <- 11.5

## Distance -- Changed tag 1755, tag 120 Eglin P1 has no value NA ##

filter(tree_data, is.na(height)) %>%
  select(date, plot_id, stem_id, tag, species, dbh, height)

filter(tree_data, is.na(char)) %>%
  select(date, plot_id, stem_id, tag, species, dbh, char)

summary(tree_data)

filter(tree_data, distance=="124") %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance)

tree_data$distance[tree_data$tag=="1995"] <- 12.4

filter(tree_data, distance=="121") %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance)

tree_data$distance[tree_data$tag=="3933"] <- 12.1

filter(tree_data, distance=="101") %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance)

tree_data$distance[tree_data$tag=="1996"] <- 10.0

filter(tree_data, distance>12.6) %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance, azimuth)

tree_data$distance[tree_data$tag=="1386"] <- 7.3
tree_data$distance[tree_data$stem_id=="1321"] <- 10.5
tree_data$distance[tree_data$stem_id=="1383"] <- 10.9
tree_data$distance[tree_data$tag=="3034"] <- 8.9
tree_data$distance[tree_data$tag=="4223"] <- 8.3
tree_data$distance[tree_data$tag=="3775"] <- 5.3
tree_data$distance[tree_data$stem_id=="1818"] <- 8.1

## Corrected distances over 12.6 due to straight error or no decimal ##

filter(tree_data, height>50) %>%
  select(date, plot_id, stem_id, tag, species, dbh, height)

tree_data$height[tree_data$tag=="3650"] <- 7.6
tree_data$height[tree_data$tag=="4613"] <- 13.2

## Corrected unrealistic high heights that were missing decimal points ##

filter(tree_data, height<2) %>%
  select(date, plot_id, stem_id, tag, species, dbh, height)

## Low tree heights of 1.4, 1.4, 0.6 are correct ##

filter(tree_data, char>15) %>%
  select(date, plot_id, stem_id, tag, species, dbh, char)

tree_data$char[tree_data$tag=="3076"] <- 0.42

## Changed char from 42 to 0.42 ##

summary(tree_data)
tree_data$date <- as.Date(as.character(tree_data$date), format = "%Y%m%d")
tree_data <- tree_data %>% 
  mutate(visit_year = lubridate::year(tree_data$date))


write_csv(tree_data, "data/processed_data/trees.csv")

##Steven stopped processing here, begin filter by installation ####

read_csv(tree_data, "data/processed_data/trees.csv")
summary(tree_data)
names(tree_data)


#### grouping summary table ####

tree_data <- tree_data %>% 
  mutate(health = if_else(health==0, "dead", "alive"))
  
trees_grouped <- tree_data %>% 
  group_by(installation, plot_id, date, visit_year, species, canopy, health, Genus, Species) %>% 
  summarise(total_dbh = sum(dbh, na.rm = T),
            avg_height = mean(height, na.rm = T),
            avg_char = mean(char, na.rm = T))

trees_grouped <- trees_grouped %>% 
  mutate(species_name = paste(Genus, Species, sep = " "))

summary(trees_grouped)

#### Filter for Camp Blanding ####

tree_summary_blanding <- trees_grouped %>% 
  filter(installation=="blanding")

tree_summary_blanding <- tree_summary_blanding %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_blanding)

write_csv(tree_summary_blanding, "data/processed_by_installation/camp_blanding/tree_summary.csv")

#### Filter for Avon Park AFR ####

tree_summary_avonpark <- trees_grouped %>% 
  filter(installation=="avonpark")

tree_summary_avonpark <- tree_summary_avonpark %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_avonpark)

write_csv(tree_summary_avonpark, "data/processed_by_installation/avon_park_afr/tree_summary.csv")

#### Filter for Eglin AFB ####

tree_summary_eglin <- trees_grouped %>% 
  filter(installation=="eglin")

tree_summary_eglin <- tree_summary_eglin %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_eglin)

write_csv(tree_summary_eglin, "data/processed_by_installation/eglin_afb/tree_summary.csv")

#### Filter for Tyndall AFB ####

tree_summary_tyndall <- trees_grouped %>% 
  filter(installation=="tyndall")

tree_summary_tyndall <- tree_summary_tyndall %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_tyndall)

write_csv(tree_summary_tyndall, "data/processed_by_installation/tyndall_afb/tree_summary.csv")

#### Filter for Fort Jackson ####

tree_summary_jackson <- trees_grouped %>% 
  filter(installation=="jackson")

tree_summary_jackson <- tree_summary_jackson %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_jackson)

write_csv(tree_summary_jackson, "data/processed_by_installation/fort_jackson/tree_summary.csv")

#### Filter for Fort Benning ####

tree_summary_benning <- trees_grouped %>% 
  filter(installation=="benning")

tree_summary_benning <- tree_summary_benning %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_benning)

write_csv(tree_summary_benning, "data/processed_by_installation/fort_benning/tree_summary.csv")

#### Filter for Camp Shelby ####

tree_summary_shelby <- trees_grouped %>% 
  filter(installation=="shelby")

tree_summary_shelby <- tree_summary_shelby %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_shelby)

write_csv(tree_summary_shelby, "data/processed_by_installation/camp_shelby/tree_summary.csv")

#### Filter for Fort Gordon ####

tree_summary_gordon <- trees_grouped %>% 
  filter(installation=="gordon")

tree_summary_gordon <- tree_summary_gordon %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_gordon)

write_csv(tree_summary_gordon, "data/processed_by_installation/fort_gordon/tree_summary.csv")

#### Filter for Moody AFB ####

tree_summary_moody <- trees_grouped %>% 
  filter(installation=="moody")

tree_summary_moody <- tree_summary_moody %>% 
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species_name, canopy, health, 
         total_dbh, avg_height, avg_char)

summary(tree_summary_moody)

write_csv(tree_summary_moody, "data/processed_by_installation/moody_afb/tree_summary.csv")

#### Steven done filtering out tree summary data to installations ####

# Quick plot checks ####
tree_data <- read_csv("data/processed_data/trees.csv")
qplot(x = dbh, y = height, data = tree_data)
qplot(x = height, y = char, data = tree_data) +
      geom_abline(intercept = 0, slope = 1)
## all looks good - "outlier" values already checked
str(tree_data)
summary(tree_data)

sort(unique(tree_data$species))
filter(tree_data, species=="tlt") %>%
      select(plot_id, dbh, date)
