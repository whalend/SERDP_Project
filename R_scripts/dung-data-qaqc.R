#' # Script for doing QA/QC on dung data

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#' ## Host Abundance Estimates
#+ dung data ####
dung_data <- read_csv("data/raw_data/dung.csv")

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

write_csv(dung_data, "data/processed_data/dung.csv")

summary(dung_data)
###Steven stopped processing here

