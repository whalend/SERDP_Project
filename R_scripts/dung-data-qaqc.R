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


dung_data <- dung_data %>%
      mutate(plot_id = case_when(
            plot_id=="gordon z1" ~ "gordon a1",
            plot_id=="gordon y1" ~ "gordon b1",
            plot_id=="gordon x1" ~ "gordon c1",
            plot_id=="gordon w1" ~ "gordon d1",
            plot_id=="gordon v1" ~ "gordon e1",
            plot_id=="gordon t1" ~ "gordon f1",
            plot_id=="gordon s1" ~ "gordon g1",
            plot_id=="gordon r1" ~ "gordon h1",
            TRUE ~ plot_id
      ))

unique(dung_data$plot_id)

write_csv(dung_data, "data/processed_data/dung.csv")

#### Steven checked processing, 3 missing Eglin i1 entries, waiting on response from elena 9/17 ####


###### steven adding qaqc for 2019 dung data #####

dung_2019 <- read_csv("data/raw_data/2019_serdp_data/dung-data-entry.csv")
tail(dung_2019)

dung_2019 <- dung_2019 %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  mutate(visit_year = lubridate::year(date)) %>% 
  filter(visit_year == 2019)
unique(dung_2019$installation)

dung_all <- rbind(dung_data, dung_2019)

write_csv(dung_data, "data/processed_data/dung.csv")
