# Camp Blanding GIS Data

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# Camp Blanding Fire Data -------------------------------------------------

blanding_fire <- st_read("data/CampBlanding/Fire_Layer_for_Whalen.shp")
summary(blanding_fire)
blanding_fire$inst_name <- "Camp Blanding"
# blanding_fire$FID <- seq(1,nrow(blanding_fire),1)
blanding_fire <- blanding_fire %>%
      select(inst_name,
             fArea_ac = featureAre,
             fCause = ignSource,
             fType = wildland_2,
             fDate = Burn_date,
             season = Season,
             purpose)

blanding_fire <- blanding_fire %>%
      mutate(fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate))

# Camp Blanding "Growing Season" and "Dormant Season"
summary(filter(blanding_fire, season == "Growing Season"))
## a "Growing Season" burn is Feburary through September
filter(blanding_fire, season == "Growing Season", fMonth==2)

summary(filter(blanding_fire, season == "Dormant Season"))
## a "Dormant Seson" burn is October through January

# cogon_fire <- readOGR("data/fire-cogon-intersect.shp")
# summary(cogon_fire)
#
# cogon_fire@data <- right_join(cogon_fire@data %>%
#       group_by(Site_Name) %>%
#       summarise(fire_frequency = (2016-2002)/length(Fire_Year)),
#       cogon_fire@data)
# st_write(fire_freq, dsn = "data/CampBlanding/cogon_fire_frequency.shp")

## Blanding Rx fire data ####

# plot(cogon_fire, col = cogon_fire$fire_frequency)
str(blanding_fire)
summary(blanding_fire)
# blanding_fire$fireCat[is.na(blanding_fire$fireCat)] <- "TBD"
unique(blanding_fire$fireCat)
rxfire <- blanding_fire[blanding_fire$fireCat == "Prescribed",]
rxfire$burnDt <- as.Date(rxfire$burnDt)
summary(rxfire)
rxfire <- rxfire %>%
      mutate(ignite = droplevels(ignite),
             fireCat = droplevels(fireCat)) %>%
      select(-eventDy)

# library(lubridate)
rxfire <- mutate(rxfire,
                 burnYr = lubridate::year(burnDt),
                 burnMo = lubridate::month(burnDt),
                 burnMo2 = lubridate::round_date(burnDt, "month"))
# detach("package:lubridate", unload=TRUE)
# writeOGR(rxfire, "data/Camp-Blanding/","rxfires", "ESRI Shapefile",
# overwrite_layer = T)

# summary(readOGR("data/Camp-Blanding/rxfires.shp"))

ggplot(blanding_fire %>%
             group_by(fYear, season) %>%
             summarise(fires = length(fDate)),
       aes(fYear, fires, fill = season)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      # geom_line() +
      # scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
      # scale_y_continuous(limits = c(0, 13)) +
      theme_bw()

ggplot(rxfire %>% group_by(burnYr) %>%
             summarise(area = sum(fArea)),
       aes(burnYr, area)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      # geom_line() +
      # scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
      # scale_y_continuous(limits = c(0, 13)) +
      theme_bw()


