#' Script for doing QA/QC on the general plot and plot visit data

#+ load packages ####
library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi)

#' Plot location data are sourced from the 'update_plot_locations.R' script.
#'

#+ examine plot location data ####

plot_shp <- rgdal::readOGR("data/plot-locations.shp")# read in spatial data
plot_shp@data$xcoord_lon <- plot_shp@coords[,1]# add coords to data frame slot
plot_shp@data$ycoord_lat <- plot_shp@coords[,2]

plot_locs <- plot_shp@data# make normal data frame
summary(plot_locs)
str(plot_locs)
sort(as.character(plot_locs$name))# sort plot names alphabetically

#' We have a few "plots" that are locations of cogongrass invasions that we marked to send to land managers. I'll remove those in the next section. Currently there are 98 plot locations and we need to remove three "extra" cogon plots. There was a "movie theater cogon" plot at Camp Blanding that we sampled but does not have any recorded burn history. It's in a kind of funny place.
#'

#+ revise plot location data ####
plot_locs <- plot_locs %>%
      rename(installation = instal) %>%
      filter(name!="Cogon10x10", name!="CogonCoastal", name!="Cogon pop") %>%
      mutate(name = as.character(name),
             installation= as.character(installation),
             pid = tolower(substr(name, start = nchar(name)-1, stop = nchar(name))),
             plot_id = paste(installation, pid, sep = " ")
      )

str(plot_locs)
summary(plot_locs)
duplicated(plot_locs$plot_id)# check for duplicate plot ids
unique(plot_locs$plot_id)

plot_locs$plot_id[plot_locs$plot_id=="movietheatercog on"] <- "blanding theater_cogon"

#+ Create labels column for installation full names
unique(plot_locs$installation)
plot_locs$installation[plot_locs$installation=="movietheatercog"] <- "blanding"

plot_locs$installation_full_name[plot_locs$installation=="blanding"] <- "Camp Blanding"
plot_locs$installation_full_name[plot_locs$installation=="eglin"] <- "Eglin AFB"
plot_locs$installation_full_name[plot_locs$installation=="tyndall"] <- "Tyndall AFB"
plot_locs$installation_full_name[plot_locs$installation=="avonpark"] <- "Avon Park AFR"
plot_locs$installation_full_name[plot_locs$installation=="shelby"] <- "Camp Shelby"
plot_locs$installation_full_name[plot_locs$installation=="moody"] <- "Moody AFB"
plot_locs$installation_full_name[plot_locs$installation=="jackson"] <- "Fort Jackson"
plot_locs$installation_full_name[plot_locs$installation=="benning"] <- "Fort Benning"
plot_locs$installation_full_name[plot_locs$installation=="gordon"] <- "Fort Gordon"

unique(plot_locs$installation_full_name)
summary(plot_locs)

write_csv(plot_locs, "data/processed_data/plot_locations.csv")# save checked data


#' We have located 94 unique plots and some have been visited multiple times. Next I assess the plot visit records, which are entered as the plot id, the date of visit, whether or not the plot is invaded, and the year of the last fire. Year since last fire is a coarse measure - we need to do further data processing and analysis to obtain the precise date of the last fire and relate that to the season of burn and perhaps months since last fire.

#+ plot visit data ####

plot_visit_data <- read_csv("data/raw_data/plot_visit.csv")
summary(plot_visit_data)
sort(plot_visit_data$plot_id)# do we need to exclude the movie theater cogon?

plot_visit_data <- plot_visit_data %>%
      # rename(pid = plot_id) %>%
      mutate(
      years_since_fire = as.numeric(format(visit_date, "%Y")) - last_fire_year,
      plot_id = paste(installation, plot_id),
      visit_year = lubridate::year(visit_date)
)

#' We have three NA values in "years since fire"
#'
#+ investigate NAs in "years since fire"
filter(plot_visit_data, is.na(last_fire_year))
# theater cogon at Blanding that has no history, and two plots at Shelby that need data requested
## I sent emails to Lisa Yager & Heide Stinson to request fire history data for Shelby. I also emailed the fire person at Tyndall requesting actual fire history data - right now the year is the best we have and in some cases it is a guess.

# plot_visit_data <- filter(plot_visit_data, !is.na(last_fire_year))

names(plot_locs)
names(plot_visit_data)
unique(plot_locs$plot_id)
unique(plot_visit_data$plot_id)
unique(plot_visit_data$plot_id) %in% unique(plot_locs$plot_id)

# join plot locations to plot visit data
plot_visit_data <- left_join(
      plot_visit_data,
      select(plot_locs, -pid, -name, -lon, -lat))
summary(plot_visit_data)

# filter(plot_visit_data, is.na(xcoord_lon))$plot_id

plot_visit_data$notes

sort(plot_visit_data$plot_id)
sort(duplicated(plot_visit_data$plot_id))
#' We revisited 9 of the 29 plots that were initially sampled in 2017.

#+ write out plot visit data ####

write_csv(plot_visit_data, "data/processed_data/plot_visit_data.csv")

#+ initial plotting of plot visit data ####

ggplot(plot_data %>%
             group_by(installation) %>%
             summarise(plot_ct = n_distinct(plot_id)),
       aes(installation, plot_ct, label=plot_ct)) +
      geom_col() +
      geom_text(nudge_y = .2) +
      ylab("Number of plots") +
      ggtitle("Number of plots established at each installation") +
      theme_classic()

ggplot(plot_visit_data, aes(as.factor(years_since_fire))) +
      geom_bar(aes(fill = imcy_inv), position = "dodge") +
      ylab("Number of plots") +
      xlab("Years since last fire") +
      ggtitle("Plots sampled across burn units") +
      theme_bw()
# Number of burn units sampled
length(unique(plot_visit_data$years_since_fire))

ggplot(plot_visit_data, aes(as.factor(years_since_fire))) +
      geom_bar() +
      facet_grid(installation ~ .) +
      ylab("Number of plots") +
      xlab("Years since last fire") +
      ggtitle("Number of plots across burn units by installation") +
      theme_bw()

ggplot(plot_visit_data, aes(imcy_inv)) +
      geom_bar() +
      ylab("Number of plots") +
      xlab("") +
      # ggtitle("Invaded plots: 8, Uninvaded plots: 21") +
      theme_bw()


#' ## Host Abundance Estimates
#+ dung data ####
dung_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/dung.csv")
