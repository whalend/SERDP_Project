#' # Script for doing QA/QC on small woody plot #`#

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#+ load processed plot visit data #
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#+ subplot data #
subplot_data <- read_csv("data/raw_data/subplot-woody-species.csv")

summary(subplot_data)
names(subplot_data)

subplot_data <- subplot_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

subplot_data <- subplot_data[ -c(4:5)]

#### Removed 0-50cm and 51-99cm columns ####

sort(unique(subplot_data$plot_id))
n_distinct(subplot_data$plot_id)
n_distinct(plot_visit_data$plot_id)

anti_join(plot_visit_data, subplot_data, "plot_id") %>%
  select(visit_date, installation, plot_id)

### Missing 4 subplot entries ###
#### Added placeholder zeros as plot visit entries for missing plots ####

sort(unique(subplot_data$veg_id))

# Will fix random subplot veg_id when entered 

filter(subplot_data, over_100cm>100)
filter(subplot_data, plot_id=="tyndall h1")

## 1300 Ilex for Tyndall h1 is correct ##

write_csv(subplot_data, "data/processed_data/woodysubplot.csv")

#### Steven stopped processing here, begin filtering by installation ####

subplot_data <- read_csv("data/processed_data/woodysubplot.csv")
summary(subplot_data)
names(subplot_data)

subplot_data$date <- as.Date(as.character(subplot_data$date), format = "%Y%m%d")
subplot_data <- subplot_data %>% 
  mutate(visit_year = lubridate::year(subplot_data$date))

subplot_data <- subplot_data %>% 
  mutate(species_name = paste(Genus, Species, sep = " "))

#### Filter for Camp Blanding ####

woody_subplot_blanding <- subplot_data %>% 
  filter(installation=="blanding") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_blanding, "data/processed_by_installation/camp_blanding/woody_subplot_blanding.csv")

#### Filter for Avon Park AFR ####

woody_subplot_avonpark <- subplot_data %>% 
  filter(installation=="avonpark") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_avonpark, "data/processed_by_installation/avon_park_afr/woody_subplot_avonpark.csv")

#### Filter for Eglin AFB ####

woody_subplot_eglin <- subplot_data %>% 
  filter(installation=="eglin") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_eglin, "data/processed_by_installation/eglin_afb/woody_subplot_eglin.csv")

#### Filter for Tyndall AFB ####

woody_subplot_tyndall <- subplot_data %>% 
  filter(installation=="tyndall") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_tyndall, "data/processed_by_installation/tyndall_afb/woody_subplot_tyndall.csv")

#### Filter for Fort Jackson ####

woody_subplot_jackson <- subplot_data %>% 
  filter(installation=="jackson") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_jackson, "data/processed_by_installation/fort_jackson/woody_subplot_jackson.csv")

#### Filter for Fort Benning ####

woody_subplot_benning <- subplot_data %>% 
  filter(installation=="benning") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_benning, "data/processed_by_installation/fort_benning/woody_subplot_benning.csv")

#### Filter for Camp Shelby ####

woody_subplot_shelby <- subplot_data %>% 
  filter(installation=="shelby") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_shelby, "data/processed_by_installation/camp_shelby/woody_subplot_shelby.csv")

#### Filter for Fort Gordon ####

woody_subplot_gordon <- subplot_data %>% 
  filter(installation=="gordon") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_gordon, "data/processed_by_installation/fort_gordon/woody_subplot_gordon.csv")

#### Filter for Moody AFB ####

woody_subplot_moody <- subplot_data %>% 
  filter(installation=="moody") %>% 
  select(plot_id, date, visit_year, species_name, over_100cm)

write_csv(woody_subplot_moody, "data/processed_by_installation/moody_afb/woody_subplot_moody.csv")

write_csv(subplot_data, "data/processed_data/woodysubplot.csv")

