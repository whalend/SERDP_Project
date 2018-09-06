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



#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_csv("data/raw_data/trees.csv")
tree_data_sub <- filter(tree_data, date>"2017-06-01", plot_id != "bland03", plot_id != "bland02")

str(tree_data)
n_distinct(tree_data$tag)
# tail(tree_data)
# summary(as.numeric(tree_data$tag))

tree_data_sub <- left_join(
      tree_data_sub,
      select(plot_data, installation:full_names),
      by = c("installation","plot_id"))

ggplot(tree_data_sub %>%
             group_by(installation, species, last_fire_year, cogongrass) %>%
             summarise(total_dbh = sum(dbh)),
       aes(last_fire_year, total_dbh)
      ) +
      geom_bar(aes(fill = species), stat = "identity", position = "dodge") +
      facet_grid(.~installation) +
      theme_bw()

ggplot(tree_data_sub %>%
             group_by(installation, species, years_since_fire, cogongrass),
       aes(years_since_fire, dbh, color = species)) +
      geom_point(position = "jitter") +
      facet_grid(installation~cogongrass) +
      theme_bw() +
      xlab("Years since fire") +
      ylab("Diameter at breast height")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/tree-dbh-jitter.png", height=7)

ggplot(tree_data_sub %>%
             group_by(installation, species, years_since_fire, cogongrass),
       aes(years_since_fire, height, color = species)) +
      geom_point(position = "jitter") +
      facet_grid(installation~cogongrass) +
      theme_bw() +
      xlab("Years since fire") +
      ylab("Tree height")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/tree-height-jitter.png", height=7)



#' ## Cogongrass Data Sampled from Invasions
#'
#+ cogongrass data ####
cogon_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/cogon-data.csv")
cogon_biomass <- cogon_data %>%
      # select(site,plot_id,Quad,fresh_biomass) %>%
      filter(site!="NA") %>%
      mutate(biomass=fresh_biomass*16)
cogon_biomass <- left_join(
      cogon_biomass,
      select(plot_data, installation,plot_id,years_since_fire),
      by=c("site"="installation","plot_id"))
ggplot(cogon_biomass, aes(years_since_fire, biomass)) +
      geom_point() +
      theme_bw() +
      xlab("Years since fire") +
      ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
      ggtitle("Cogongrass invasion subsample")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/cogon-biomass.png", height = 7)


#' ## Tick Abundance Estimates
#+ tick data ####
tick_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/ticks.csv")

tick_data$plot_id <- tolower(tick_data$plot_id)
tick_data$plot_id[tick_data$plot_id=="c"] <- "c1"

tick_data <- left_join(
      tick_data,
      select(plot_data, installation,plot_id,fire_year:full_names),
      by = c("installation", "plot_id")
)
summary(tick_data)
filter(tick_data, is.na(years_since_fire))
# "extra" ticks at Moody AFB were from an area last burned in 2005
tick_data$years_since_fire[tick_data$installation=="moody"] <- 2017-2005


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

#' ## Host Abundance Estimates
#+ dung data ####
dung_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/dung.csv")
