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

tick_data$plot_id <- tolower(tick_data$plot_id)
tick_data$plot_id[tick_data$plot_id=="c"] <- "c1"

tick_data <- tick_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

sort(unique(tick_data$plot_id))

# tick_data <- left_join(
# tick_data,
# select(plot_data, installation,plot_id,fire_year:full_names),
# by = c("installation", "plot_id")
# )

summary(tick_data)

# filter(tick_data, is.na(years_since_fire))
# "extra" ticks at Moody AFB were from an area last burned in 2005
# tick_data$years_since_fire[tick_data$installation=="moody"] <- 2017-2005

## Steven processing, added concatenation for plot_id & installation line 17 ##

unique(tick_data$location)

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

### Made locations consistent ###

unique(plot_visit_data$plot_id) %in% unique(tick_data$plot_id)

n_distinct(tick_data$plot_id)
n_distinct(plot_visit_data$plot_id)

anti_join(plot_visit_data, tick_data, "plot_id") %>%
  select(visit_date, installation, plot_id)

sort(unique(tick_data$plot_id))

### ADDED zeros Eglin b1, & moody b1, a1, k1 to excel sheet, need to update from dropbox ###
### Missing data sheet entry for Blanding b1 (2017) and Avon Park b1 (2017),
### and Eglin a1, c1, e1 (2018) ####

write_csv(tick_data, "data/processed_data/ticks.csv")

summary(tick_data)
### Steven stopped processing ###


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
