#' # Script for doing QA/QC on the 25cm quadrat data (biomass estimates)
#'
#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)

#+ load processed plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#+ 25cm biomass data ####
biomass_data <- read_csv("data/raw_data/quadrat25cm.csv")
biomass_data
summary(biomass_data)
biomass_data <- biomass_data %>%
      mutate(plot_id = paste(installation, plot_id, sep = " "))
## NA's for date
filter(biomass_data, is.na(date))$plot_id
filter(biomass_data, plot_id == "blanding c1")$date
## missing the date of the 2018 visit to Blanding C1
biomass_data$date[is.na(biomass_data$date)] <- 20180516
## Exclude pilot plot sampling from data
biomass_data <- biomass_data %>% filter(date > 20170601)
unique(biomass_data$plot_id)
biomass_data$plot_id[biomass_data$plot_id=="blanding cogan"] <- "blanding theater_cogon"

summary(biomass_data)
## 7 NAs for dry standing fuel mass
filter(biomass_data, is.na(standing_fuel_mass_dry))
filter(biomass_data, is.na(standing_fuel_mass_dry)) %>%
      select(date, plot_id)
## Shelby B1 in 2017 is probably missing because cogongrass in MS
## Not sure why the others are missing




biomass_data$fuel_mass_wet <- round(as.numeric(biomass_data$fuel_mass_wet),2)
biomass_data <- left_join(
      select(biomass_data, installation,plot_id,fuel_mass_wet,litter_mass_wet),
      select(plot_data, installation:full_names),
      by = c("installation","plot_id")
)
# biomass_data$last_fire_year <- as.integer(biomass_data$last_fire_year)
# biomass_data$years_since_fire <- 2017 - biomass_data$last_fire_year

ggplot(biomass_data,
       aes(years_since_fire, fuel_mass_wet*16)) +
      geom_point(aes(color = installation), position = "jitter") +
      # facet_grid(.~installation) +
      # geom_smooth(method = "loess")
      theme_bw() +
      ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
      xlab("Years since last fire") +
      ggtitle("Fresh standing biomass across burn units at each installation",
              subtitle = "(measurements at 25cm quadrats)")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/standing-biomass-hitter.png", height=7)

ggplot(biomass_data %>% group_by(installation, last_fire_year),
       aes(years_since_fire, litter_mass_wet*16)) +
      geom_point(aes(color = installation), position = "jitter") +
      # facet_grid(.~installation) +
      theme_bw() +
      ylab(expression(paste("Litter biomass (g/", m^{2}, ")"))) +
      xlab("Years since last fire") +
      ggtitle("Fresh litter biomass across burn units at each installation",
              subtitle = "(measurements at 25cm quadrats)")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/litter-biomass-jitter.png", height=7)
