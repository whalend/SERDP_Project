#' # Script for doing QA/QC on small woody plot #`#

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#+ load processed plot visit data #
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#+ subplot data #
#subplot_data <- read_csv("data/raw_data/subplot-woody-species.csv")

subplot_data <- read_csv("data/raw_data/2019_serdp_data/subplot-woody-species-data-entry.csv")
###### steven changed file read in to add 2019 data to end of 17/18 data. edited 9/24/19 #####

summary(subplot_data)
names(subplot_data)

subplot_data <- subplot_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

subplot_data <- subplot_data %>%
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

unique(subplot_data$plot_id)


# subplot_data <- read_csv("data/processed_data/woodysubplot.csv")
summary(subplot_data)
names(subplot_data)

subplot_data <- subplot_data %>%
  mutate(stems_100m2 = round(if_else(date<"2018-01-01", over_100cm*.599,over_100cm*1)),
         species_name = paste(Genus, Species, sep = " "),
         date = as.Date(as.character(date), format = "%Y%m%d"),
         visit_year = lubridate::year(date))


filter(subplot_data, plot_id=="tyndall h1") %>%
  select(plot_id, date, species_name, over_100cm, stems_100m2)

subplot_data$stems_100m2[subplot_data$plot_id=="tyndall h1" &
                           subplot_data$species_name=="Ilex glabra"] <- 1300

names(subplot_data)
names(plot_visit_data)

subplot_data <- left_join(subplot_data,
                          select(plot_visit_data, -notes))

write_csv(subplot_data, "data/processed_data/woodysubplot.csv")
