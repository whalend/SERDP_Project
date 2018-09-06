#' # Script for doing QA/QC on the canopy cover data

#' ## Canopy Cover Data
#'

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)

#+ load plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")
summary(plot_visit_data)
n_distinct(plot_visit_data$plot_id)

#+ load canopy cover data ####
canopy_cover <- read_csv("data/raw_data/densiometer-data.csv")
summary(canopy_cover)
unique(canopy_cover$plot_id)
unique(canopy_cover$installation)

#' * 4 `NA` values in 'fill_dots' & 'distance'
#' * need to create a unique plot id
#'
#' The distance is a bit complicated because of the way sampling was changed from year 1 to year 2. I think we keep all observations for now because we could relate the canopy cover at the farther distances to the tick trapping in year 1.
#'

#+ check installation and plot_id NAs ####
filter(canopy_cover, is.na(installation))
filter(canopy_cover, is.na(plot_id))
## extra rows; Excel artifact
## remove extra rows and extra columns

canopy_cover <- canopy_cover %>%
      mutate(plot_id = paste(installation, plot_id, sep = " "))
summary(canopy_cover)

#+ check distance and fill_dots NAs ####
filter(canopy_cover, is.na(fill_dots))
## probably pre-calculated the canopy cover...not sure with distance; since it was early in the project I would assume that it was at the 10m distance
filter(canopy_cover, plot_id=="blanding c1")
canopy_cover$distance[is.na(canopy_cover$distance)] <- 10
summary(canopy_cover)

#+ check unique installation and plot id ####
unique(canopy_cover$plot_id)# 95 plots
unique(plot_visit_data$plot_id)# 95 plots
canopy_cover$plot_id[canopy_cover$plot_id=="blanding cogan"] <- "blanding theater_cogon"

# Check for missing in the canopy cover data
unique(plot_visit_data$plot_id) %in% unique(canopy_cover$plot_id)
anti_join(plot_visit_data, canopy_cover)$plot_id
## missing canopy cover data for Blanding D1 from 2017-06-20 visit; do we have this data somewhere? Drew's phone/pics?
### I found these pics and entered them into the data entry sheet on Dropbox

summary(canopy_cover)

#+ write out processed data
write_csv(canopy_cover, "data/processed_data/canopy-cover.csv")
