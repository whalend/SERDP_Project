#' # Script for doing QA/QC on tick data

library(plyr)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
# library(tidyverse)

#' Tick Abundance Estimates

#+ tick data ####
tick_data <- read_csv("data/raw_data/ticks.csv")

#+ plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

sort(unique(tick_data$plot_id))
tick_data$plot_id <- tolower(tick_data$plot_id)
# tick_data$plot_id[tick_data$plot_id=="c"] <- "c1"

tick_data <- tick_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id = tolower(plot_id)) %>%
  filter(date>20170601)

sort(unique(tick_data$plot_id))
filter(tick_data, plot_id=="tyndall na")
filter(tick_data, plot_id=="tyndall extra")

tick_data$plot_id[tick_data$plot_id=="tyndall na"] = "tyndall extra"

# tick_data <- left_join(
# tick_data,
# select(plot_data, installation,plot_id,fire_year:full_names),
unique(tick_data$location)
# by = c("installation", "plot_id")
# )

summary(tick_data)

# filter(tick_data, is.na(years_since_fire))
# "extra" ticks at Moody AFB were from an area last burned in 2005
# tick_data$years_since_fire[tick_data$installation=="moody"] <- 2017-2005

## Steven processing, added concatenation for plot_id & installation line 17 ##

## Made locations consistent ####

tick_data$location <- tolower(tick_data$location)
unique(tick_data$location)

# tolower()
# toupper()
# tick_data$location[tick_data$location=="nw"] <- "NW"
# tick_data$location[tick_data$location=="ne"] <- "NE"
# tick_data$location[tick_data$location=="Se"] <- "SE"
# tick_data$location[tick_data$location=="se"] <- "SE"
# tick_data$location[tick_data$location=="sw"] <- "SW"

# tick_data$location[tick_data$location=="north"] <- "n"
# tick_data$location[tick_data$location=="south"] <- "s"
# tick_data$location[tick_data$location=="east"] <- "e"
# tick_data$location[tick_data$location=="West"] <- "W"
# tick_data$location[tick_data$location=="west"] <- "w"
# tick_data$location[tick_data$location=="C"] <- "Center"
# tick_data$location[tick_data$location=="center"] <- "Center"
# tick_data$location[tick_data$location=="extra"] <- "Extra"


unique(plot_visit_data$plot_id) %in% unique(tick_data$plot_id)

n_distinct(tick_data$plot_id)
n_distinct(plot_visit_data$plot_id)

anti_join(plot_visit_data, tick_data, "plot_id") %>%
  select(visit_date, installation, plot_id)

sort(unique(tick_data$plot_id))

# Added zeroes for all visited plots that were missing


# Make species id consistent ####
sort(unique(tick_data$species))
tick_data$species <- tolower(tick_data$species)

tick_data$species[tick_data$species=="am. am"] <- "Am. am"
# tick_data$species[tick_data$species=="Am. Am"] <- "Am. am"
tick_data$species[tick_data$species=="am. mac"] <- "Am. mac"
tick_data$species[tick_data$species=="de. var"] <- "De. var"
# tick_data$species[tick_data$species=="De. Var"] <- "De. var"
tick_data$species[tick_data$species=="rh. san"] <- "Rh. san"

tick_data$date <- as.Date(as.character(tick_data$date), format = "%Y%m%d")
tick_data <- tick_data %>%
  mutate(visit_year = lubridate::year(tick_data$date))

summary(tick_data)

# filter(tick_data, plot_id!="tyndall c2", species=="Rh. san")

write_csv(tick_data, "data/processed_data/ticks.csv")


# Merge corrected/confirmed data from Illinois -----------------------

## Page sent a spreadsheet with all the 2017/2018 vials processed
## Species and life stages were confirmed/corrected
## A sample id code was added
## I'm adding back the zeroes in the original data for completeness

library(readxl)
library(readr)
library(dplyr)

illini_data <- read_xlsx("data/raw_data/ticks-dna-2018_Updated_Datasheet.xlsx", sheet = 1)

tick_data <- read_csv("data/processed_data/ticks.csv")
tick_data_zeroes <- filter(tick_data, count<1)

names(illini_data)
names(tick_data_zeroes)

sort(unique(illini_data$location))
sort(unique(tick_data_zeroes$location))
illini_data$location <- tolower(illini_data$location)

illini_data$location[illini_data$location=="c"] <- "center"
illini_data$location[illini_data$location=="e"] <- "east"
illini_data$location[illini_data$location=="n"] <- "north"
illini_data$location[illini_data$location=="s"] <- "south"
illini_data$location[illini_data$location=="w"] <- "west"

sort(unique(illini_data$species))
# sort(unique(tick_data$species))

illini_data <- illini_data %>%
      mutate(date = as.Date(date),
             installation = tolower(installation))

sort(unique(tick_data_zeroes$plot_id))
# unique(tick_data_zeroes$life_stage)

tick_data_corrected <- full_join(illini_data,
                        tick_data_zeroes)
summary(tick_data_corrected)

tick_data_corrected <- tick_data_corrected %>%
      select(-notes, -Notes, -drag_time) %>%
      mutate(visit_year = lubridate::year(date))


## Add full species names ####
# tick_data <- read_csv("data/processed_data/ticks.csv")
# summary(tick_data)
names(tick_data_corrected)
unique(tick_data_corrected$species)
tick_data_corrected <- tick_data_corrected %>%
  mutate(species_name = case_when(
        species=="Am. am" ~ "Amblyomma americanum",
        species=="Am. mac" ~ "Amblyomma maculatum",
        species=="De. var" ~ "Dermacentor variabilis",
        species=="Rh. san" ~ "Rhipicephalus sanguineus",
        species=="unknown" ~ "unknown"))

# tick_data_corrected$species_name
unique(tick_data_corrected$species_name)

write_csv(tick_data_corrected, "data/processed_data/ticks.csv")



## initial plotting ####
# ggplot(tick_data %>%
# group_by(installation,years_since_fire,Species) %>%
# summarise(tick_number = sum(count)),
# aes(as.factor(years_since_fire), tick_number)) +
# # geom_point(position = "jitter") +
# geom_bar(fill = "#124873", stat = "identity", position = "dodge") +
# # facet_grid(installation~., scales = "free_y") +
# xlab("Years since fire") +
# ylab("Tick abundance") +
# theme_bw() +
# theme(axis.text = element_text(size = 18),
# axis.title = element_text(size = 22))
#
# ggplot(tick_data %>%
# group_by(full_names, years_since_fire, Species) %>%
# summarise(tick_number = sum(count)),
# aes(as.factor(years_since_fire), tick_number, fill = Species)) +
# # geom_point(position = "jitter") +
# geom_bar(stat = "identity", position = "dodge") +
# facet_grid(full_names~., scales = "free_y") +
# xlab("Years since fire") +
# ylab("Tick abundance") +
# theme_bw() +
# theme(axis.text = element_text(size = 18),
# axis.title = element_text(size = 22))
