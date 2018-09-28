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

### Steven stopped processing, begin processing by installation ###

tick_data <- read_csv("data/processed_data/ticks.csv")
summary(tick_data)
names(tick_data)

ticks_grouped <- tick_data %>% 
  group_by(installation, plot_id, date, visit_year, species, life_stage) %>% 
  summarise(tick_count = sum(count, na.rm = T))

summary(ticks_grouped)

ticks_grouped <- ticks_grouped %>% 
  filter(ticks_grouped, !is.na(species)) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

#### Filter for Camp Blanding ####

ticks_blanding <- ticks_grouped %>% 
  filter(installation=="blanding")

ticks_blanding <- filter(ticks_blanding) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_blanding)

filter(ticks_blanding, plot_id=="blanding h1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_blanding, "data/processed_by_installation/camp_blanding/ticks_blanding.csv")

#### Filter for Avon Park ####

ticks_avonpark <- ticks_grouped %>% 
  filter(installation=="avonpark")

ticks_avonpark <- filter(ticks_avonpark) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_avonpark)

filter(ticks_avonpark, plot_id=="avonpark c2") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_avonpark, "data/processed_by_installation/avon_park_afr/ticks_avonpark.csv")

#### Filter for Eglin AFB ####

ticks_eglin <- ticks_grouped %>% 
  filter(installation=="eglin")

ticks_eglin <- filter(ticks_eglin) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_eglin)

filter(ticks_eglin, plot_id=="eglin i1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_eglin, "data/processed_by_installation/eglin_afb/ticks_eglin.csv")

#### Filter for Tyndall AFB ####

ticks_tyndall <- ticks_grouped %>% 
  filter(installation=="tyndall")

ticks_tyndall <- filter(ticks_tyndall) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_tyndall)

filter(ticks_tyndall, plot_id=="tyndall c1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_tyndall, "data/processed_by_installation/tyndall_afb/ticks_tyndall.csv")

#### Filter for Fort Jackson ####

ticks_jackson <- ticks_grouped %>% 
  filter(installation=="jackson")

ticks_jackson <- filter(ticks_jackson) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_jackson)

filter(ticks_jackson, plot_id=="jackson c1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_jackson, "data/processed_by_installation/fort_jackson/ticks_jackson.csv")

#### Filter for Fort Benning ####

ticks_benning <- ticks_grouped %>% 
  filter(installation=="benning")

ticks_benning <- filter(ticks_benning) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_benning)

filter(ticks_benning, plot_id=="benning c1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_benning, "data/processed_by_installation/fort_benning/ticks_benning.csv")

#### Filter for Camp Shelby ####

ticks_shelby <- ticks_grouped %>% 
  filter(installation=="shelby")

ticks_shelby <- filter(ticks_shelby) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_shelby)

filter(ticks_shelby, plot_id=="shelby c1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_shelby, "data/processed_by_installation/camp_shelby/ticks_shelby.csv")

#### Filter for Fort Gordon####

ticks_gordon <- ticks_grouped %>% 
  filter(installation=="gordon")

ticks_gordon <- filter(ticks_gordon) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_gordon)

filter(ticks_gordon, plot_id=="gordon v1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_gordon, "data/processed_by_installation/fort_gordon/ticks_gordon.csv")

#### Filter for Moody AFB ####

ticks_moody <- ticks_grouped %>% 
  filter(installation=="moody")

ticks_moody <- filter(ticks_moody) %>%
  ungroup(.) %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

summary(ticks_moody)

filter(ticks_moody, plot_id=="moody k1") %>% 
  select(plot_id, date, visit_year, species, life_stage, tick_count)

write_csv(ticks_moody, "data/processed_by_installation/moody_afb/ticks_moody.csv")

#### Steven stopped, question about multiple dates for all data and random NAs ####


######################## 
ggplot(tick_data %>%
group_by(installation,years_since_fire,Species) %>%
summarise(tick_number = sum(count)),
aes(as.factor(years_since_fire), tick_number)) +
# geom_point(position = "jitter") +
geom_bar(fill = "#124873", stat = "identity", position = "dodge") +
# facet_grid(installation~., scales = "free_y") +
xlab("Years since fire") +
ylab("Tick abundance") +
theme_bw() +
theme(axis.text = element_text(size = 18),
axis.title = element_text(size = 22))

ggplot(tick_data %>%
group_by(full_names, years_since_fire, Species) %>%
summarise(tick_number = sum(count)),
aes(as.factor(years_since_fire), tick_number, fill = Species)) +
# geom_point(position = "jitter") +
geom_bar(stat = "identity", position = "dodge") +
facet_grid(full_names~., scales = "free_y") +
xlab("Years since fire") +
ylab("Tick abundance") +
theme_bw() +
theme(axis.text = element_text(size = 18),
axis.title = element_text(size = 22))
