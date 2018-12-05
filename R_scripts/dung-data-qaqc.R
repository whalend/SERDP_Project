#' # Script for doing QA/QC on dung data

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#' ## Host Abundance Estimates
#+ dung data ####
dung_data <- read_csv("data/raw_data/dung.csv")

#+ plot visit data ####

plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

names(dung_data)

dung_data <- dung_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

# filter(plotid %in% c("n","s","e"))

d <- dung_data %>%
      group_by(installation, plot_id) %>%
      summarise(n_transect = n_distinct(location))

summary(d)
summary(dung_data)
summary(plot_visit_data)

unique(plot_visit_data$plot_id) %in% unique(dung_data$plot_id)

n_distinct(dung_data$plot_id)
n_distinct(plot_visit_data$plot_id)

anti_join(plot_visit_data, dung_data, "plot_id") %>%
  select(installation, plot_id)

### Blanding theater_cogon not included in dung, all other plots account for ###

filter(d, n_transect>4)
n_distinct(dung_data$location)
unique(dung_data$location)
unique(dung_data$species)

dung_data$location[dung_data$location=="North"] <- "north"
dung_data$location[dung_data$location=="South"] <- "south"
dung_data$location[dung_data$location=="East"] <- "east"
dung_data$location[dung_data$location=="West"] <- "west"

dung_data$species[dung_data$species=="Cottontail"] <- "cottontail"
dung_data$species[dung_data$species=="Deer"] <- "deer"
dung_data$species[dung_data$species=="Cow"] <- "cow"
dung_data$species[dung_data$species=="Armadillo"] <- "armadillo"
dung_data$species[dung_data$species=="Hog"] <- "hog"
dung_data$species[dung_data$species=="Racoon"] <- "racoon"
dung_data$species[dung_data$species=="Other"] <- "other"

### Cleaned up transect location and species ID to be consistent ###

filter(dung_data, plot_id=="avonpark a1") %>%
  select(date, plot_id, location, species, dung1m, dung2m)

### n_transect of 8 from plot revisits ###

summary(dung_data)

filter(dung_data, is.na(dung1m)) %>%
  select(date, plot_id, location, species, dung1m, dung2m)

filter(dung_data, is.na(dung2m)) %>%
  select(date, plot_id, location, species, dung1m, dung2m)


dung_data$dung2m[dung_data$plot_id=="blanding m1" & dung_data$location=="east"] <- 0

filter(dung_data, plot_id=="eglin i1")
filter(plot_visit_data, plot_id=="eglin")
#### Eglin i1 north, south, west missing? east is 0 and 0 ####

dung_data$date <- as.Date(as.character(dung_data$date), format = "%Y%m%d")
dung_data <- dung_data %>%
  mutate(visit_year = lubridate::year(date))
summary(dung_data)

write_csv(dung_data, "data/processed_data/dung.csv")

#### Steven checked processing, 3 missing Eglin i1 entries, waiting on response from elena 9/17 ####

#### Steven begin processing by installation ####

dung_data <- read_csv("data/processed_data/dung.csv")
summary(dung_data)
names(dung_data)

dung_grouped <- dung_data %>%
  group_by(installation, plot_id, date, visit_year, species) %>%
  summarise(dung1m = sum(dung1m, na.rm = T), dung2m = sum(dung2m, na.rm = T))

summary(dung_grouped)

#### Filter for Camp Blanding ####

dung_blanding <- dung_grouped %>%
  filter(installation=="blanding", !is.na(species))

blanding_plots <- plot_visit_data %>%
      filter(installation=="blanding", plot_id != "blanding theater_cogon") %>%
      select(installation, plot_id, visit_year)

d1 <- left_join(blanding_plots, dung_blanding)
unique(d1$species)

dung_blanding <- filter(dung_blanding) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_blanding)

filter(dung_blanding, plot_id=="blanding h1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_blanding, "data/processed_by_installation/camp_blanding/dung_blanding.csv")

#### Filter for Avon Park ####

dung_avonpark <- dung_grouped %>%
  filter(installation=="avonpark")

dung_avonpark <- filter(dung_avonpark) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_avonpark)

filter(dung_avonpark, plot_id=="avonpark c1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_avonpark, "data/processed_by_installation/avon_park_afr/dung_avonpark.csv")

#### Filter for Eglin AFB ####

dung_eglin <- dung_grouped %>%
  filter(installation=="eglin")

dung_eglin <- filter(dung_eglin) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_eglin)

filter(dung_eglin, plot_id=="eglin c1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_eglin, "data/processed_by_installation/eglin_afb/dung_eglin.csv")

#### Filter for Tyndall AFB ####

dung_tyndall <- dung_grouped %>%
  filter(installation=="tyndall")

dung_tyndall <- filter(dung_tyndall) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_tyndall)

filter(dung_tyndall, plot_id=="tyndall h1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_tyndall, "data/processed_by_installation/tyndall_afb/dung_tyndall.csv")


#### Filter for Fort Jackson ####

dung_jackson <- dung_grouped %>%
      filter(installation=="jackson", !is.na(species))

jackson_plots <- plot_visit_data %>%
      filter(installation=="jackson") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_jackson <- left_join(jackson_plots, dung_jackson)

dung_jackson <- dung_jackson %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)


summary(dung_jackson)

filter(dung_jackson, plot_id=="jackson h1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_jackson, "data/processed_by_installation/fort_jackson/dung_jackson.csv")

#### Filter for Fort Benning ####

dung_benning <- dung_grouped %>%
  filter(installation=="benning")

dung_benning <- filter(dung_benning) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_benning)

filter(dung_benning, plot_id=="benning h1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_benning, "data/processed_by_installation/fort_benning/dung_benning.csv")

#### Filter for Camp Shelby ####

dung_shelby <- dung_grouped %>%
  filter(installation=="shelby")

dung_shelby <- filter(dung_shelby) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_shelby)

filter(dung_shelby, plot_id=="shelby h1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_shelby, "data/processed_by_installation/camp_shelby/dung_shelby.csv")

#### Filter for Fort Gordon ####

dung_gordon <- dung_grouped %>%
  filter(installation=="gordon")

dung_gordon <- filter(dung_gordon) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_gordon)

filter(dung_gordon, plot_id=="gordon x1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_gordon, "data/processed_by_installation/fort_gordon/dung_gordon.csv")

#### Filter for Moody AFB ####

dung_moody <- dung_grouped %>%
  filter(installation=="moody")

dung_moody <- filter(dung_moody) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

summary(dung_moody)

filter(dung_moody, plot_id=="moody k1") %>%
  select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_moody, "data/processed_by_installation/moody_afb/dung_moody.csv")

#### Steven stopped processing to installation, ask about random NAs as in ticks ####
