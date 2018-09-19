#' # Script for doing QA/QC on cogongrass data

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#' ## Cogongrass Data Sampled from Invasions
#'
#+ cogongrass data ####
cogon_data <- read_csv("data/raw_data/cogon-data.csv")

cogon_data$plot_id[cogon_data$plot_id=="theatercogon"] <- "theater_cogon"

cogon_data <- cogon_data %>%
  mutate(plot_id = paste(site, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

sort(unique(cogon_data$plot_id))

summary(cogon_data)

filter(cogon_data, is.na(dry_biomass)) %>% 
  select(date, plot_id, Quadrant, CogonSampleID, dry_biomass)

#### Missing dry biomass from Shelby due to transportation regulations ####
#### Other missing biomass measurements (blanding theater and shelby 2018) due to mixed cogon biomass ####

write_csv(cogon_data, "data/processed_data/cogongrass.csv")

#### Steven stopped processing ####

###################### Whalen's processing #####
# cogon_biomass <- cogon_data %>%
#       # select(site,plot_id,Quad,fresh_biomass) %>%
#       filter(site!="NA") %>%
#       mutate(biomass=fresh_biomass*16)
# cogon_biomass <- left_join(
#       cogon_biomass,
#       select(plot_data, installation,plot_id,years_since_fire),
#       by=c("site"="installation","plot_id"))
# ggplot(cogon_biomass, aes(years_since_fire, biomass)) +
#       geom_point() +
#       theme_bw() +
#       xlab("Years since fire") +
#       ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
#       ggtitle("Cogongrass invasion subsample")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/cogon-biomass.png", height = 7)
