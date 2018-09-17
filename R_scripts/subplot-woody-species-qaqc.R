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

subplot_data <- subplot_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

sort(unique(subplot_data$plot_id))
n_distinct(subplot_data$plot_id)
n_distinct(plot_visit_data$plot_id)

anti_join(plot_visit_data, subplot_data, "plot_id") %>%
  select(visit_date, installation, plot_id)

#### Missing 4 subplot entries ####

sort(unique(subplot_data$veg_id))

#### Steven stopped processing here ####


