#' # Script for doing QA/QC on tick data

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#' Tick Abundance Estimates

#+ tick data ####
tick_data <- read_csv("data/raw_data/ticks.csv")

#+ plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

sort(unique(tick_data$plot_id))
tick_data$plot_id <- tolower(tick_data$plot_id)
tick_data$plot_id[tick_data$plot_id=="c"] <- "c1"

tick_data <- tick_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
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

### Made locations consistent ####

# tolower()
# toupper()
tick_data$location[tick_data$location=="nw"] <- "NW"
tick_data$location[tick_data$location=="ne"] <- "NE"
tick_data$location[tick_data$location=="Se"] <- "SE"
tick_data$location[tick_data$location=="se"] <- "SE"
tick_data$location[tick_data$location=="sw"] <- "SW"

tick_data$location[tick_data$location=="North"] <- "N"
tick_data$location[tick_data$location=="South"] <- "S"
tick_data$location[tick_data$location=="East"] <- "E"
tick_data$location[tick_data$location=="West"] <- "W"
tick_data$location[tick_data$location=="west"] <- "W"
tick_data$location[tick_data$location=="C"] <- "Center"
tick_data$location[tick_data$location=="center"] <- "Center"
tick_data$location[tick_data$location=="extra"] <- "Extra"



unique(plot_visit_data$plot_id) %in% unique(tick_data$plot_id)

n_distinct(tick_data$plot_id)
n_distinct(plot_visit_data$plot_id)

anti_join(plot_visit_data, tick_data, "plot_id") %>%
  select(visit_date, installation, plot_id)

sort(unique(tick_data$plot_id))

### Added zeroes for all visited plots that were missing ###

sort(unique(tick_data$species))

tick_data$species[tick_data$species=="am. Am"] <- "Am. am"
tick_data$species[tick_data$species=="Am. Am"] <- "Am. am"
tick_data$species[tick_data$species=="Am. Mac"] <- "Am. mac"
tick_data$species[tick_data$species=="de. Var"] <- "De. var"
tick_data$species[tick_data$species=="De. Var"] <- "De. var"
tick_data$species[tick_data$species=="rh. san"] <- "Rh. san"

tick_data$date <- as.Date(as.character(tick_data$date), format = "%Y%m%d")
tick_data <- tick_data %>%
  mutate(visit_year = lubridate::year(tick_data$date))

summary(tick_data)

write_csv(tick_data, "data/processed_data/ticks.csv")

### Steven stopped processing, begin processing by installation ####

tick_data <- read_csv("data/processed_data/ticks.csv")
summary(tick_data)
names(tick_data)
unique(tick_data$species)
tick_data <- tick_data %>%
  mutate(species_name = case_when(species=="Am. am" ~ "Amblyomma americanum",
         species=="Am. mac" ~ "Amblyomma maculatum",
         species=="De. var" ~ "Dermacentor variabilis",
         species=="Rh. san" ~ "Rhipicephalus sanguineus",
         species=="unknown" ~ "unknown"))

tick_data$species_name
unique(tick_data$species_name)
filter(tick_data, species=="unknown")$species_name

write_csv(tick_data, "data/processed_data/ticks.csv")

tick_data <- read_csv("data/processed_data/ticks.csv")

ticks_grouped <- tick_data %>%
  group_by(installation, plot_id, date, visit_year, species_name, life_stage) %>%
  summarise(tick_count = sum(count, na.rm = T))

summary(ticks_grouped)

###is.na(ticks_grouped$species)
###ticks_grouped <- ticks_grouped %>%
###  filter(!is.na(species)) %>%
###  select(installation, plot_id, date, visit_year, species, life_stage, tick_count)

#### Filter for Camp Blanding ####

ticks_blanding <- ticks_grouped %>%
  filter(installation=="blanding", !is.na(species_name))

blanding_plots <- plot_visit_data %>%
  filter(installation=="blanding") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_blanding <- left_join(blanding_plots, ticks_blanding)

ticks_blanding <- ticks_blanding %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_blanding, "data/processed_by_installation/camp_blanding/ticks_blanding.csv")

#### Filter for Avon Park ####

ticks_avonpark <- ticks_grouped %>%
  filter(installation=="avonpark", !is.na(species_name))

avonpark_plots <- plot_visit_data %>%
  filter(installation=="avonpark") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_avonpark <- left_join(avonpark_plots, ticks_avonpark)

ticks_avonpark <- ticks_avonpark %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_avonpark, "data/processed_by_installation/avon_park_afr/ticks_avonpark.csv")

#### Filter for Eglin AFB ####

ticks_eglin <- ticks_grouped %>%
  filter(installation=="eglin", !is.na(species_name))

eglin_plots <- plot_visit_data %>%
  filter(installation=="eglin") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_eglin <- left_join(eglin_plots, ticks_eglin)

ticks_eglin <- ticks_eglin %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_eglin, "data/processed_by_installation/eglin_afb/ticks_eglin.csv")

#### Filter for Tyndall AFB ####

ticks_tyndall <- ticks_grouped %>%
  filter(installation=="tyndall", !is.na(species_name))

tyndall_plots <- plot_visit_data %>%
  filter(installation=="tyndall") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_tyndall <- left_join(tyndall_plots, ticks_tyndall)

ticks_tyndall <- ticks_tyndall %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_tyndall, "data/processed_by_installation/tyndall_afb/ticks_tyndall.csv")

#### Filter for Fort Jackson ####

ticks_jackson <- ticks_grouped %>%
      filter(installation=="jackson", !is.na(species_name))

jackson_plots <- plot_visit_data %>%
      filter(installation=="jackson") %>%
      select(installation, plot_id, visit_year, date=visit_date)

ticks_jackson <- left_join(jackson_plots, ticks_jackson)

ticks_jackson <- ticks_jackson %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_jackson, "data/processed_by_installation/fort_jackson/ticks_jackson.csv")

#### Filter for Fort Benning ####

ticks_benning <- ticks_grouped %>%
  filter(installation=="benning", !is.na(species_name))

benning_plots <- plot_visit_data %>%
  filter(installation=="benning") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_benning <- left_join(benning_plots, ticks_benning)

ticks_benning <- ticks_benning %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_benning, "data/processed_by_installation/fort_benning/ticks_benning.csv")

#### Filter for Camp Shelby ####

ticks_shelby <- ticks_grouped %>%
  filter(installation=="shelby", !is.na(species_name))

shelby_plots <- plot_visit_data %>%
  filter(installation=="shelby") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_shelby <- left_join(shelby_plots, ticks_shelby)

ticks_shelby <- ticks_shelby %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_shelby, "data/processed_by_installation/camp_shelby/ticks_shelby.csv")

#### Filter for Fort Gordon####

ticks_gordon <- ticks_grouped %>%
  filter(installation=="gordon", !is.na(species_name))

gordon_plots <- plot_visit_data %>%
  filter(installation=="gordon") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_gordon <- left_join(gordon_plots, ticks_gordon)

ticks_gordon <- ticks_gordon %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_gordon, "data/processed_by_installation/fort_gordon/ticks_gordon.csv")

#### Filter for Moody AFB ####

ticks_moody <- ticks_grouped %>%
  filter(installation=="moody", !is.na(species_name))

gordon_moody <- plot_visit_data %>%
  filter(installation=="moody") %>%
  select(installation, plot_id, visit_year, date=visit_date)

ticks_moody <- left_join(moody_plots, ticks_moody)

ticks_moody <- ticks_moody %>%
  mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
  select(plot_id, date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_moody, "data/processed_by_installation/moody_afb/ticks_moody.csv")

#### Steven stopped, question about multiple dates for all data and random NAs ####
# fixed NAs

########################
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
