# Camp Shelby GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####

# data provided by Camp Shelby personnel
shelby_fire_history <- st_read("data/CampShelby/FireHistory.shp")

# data provided in 2017 by USFS
shelby_fire.03_16 <- st_read("data/CampShelby/fireAndFlora/firehis_pl.shp")

# data provided in 2018 by USFS


# shelby_fire <- st_read("data/CampShelby/FireManagementArea.shp")
plot(shelby_fire)
summary(shelby_fire)


# congongrass data ####
shelby_cogon <- readOGR("data/CampShelby/NuisanceSpeciesManagement.shp")
summary(shelby_cogon)

shelby_cogon_untrt <- shelby_cogon[shelby_cogon$Status=="MAPPED ONLY",]
summary(shelby_cogon_untrt)
shelby_cogon_untrt@data <- droplevels(select(
      shelby_cogon_untrt@data,
      OBJECTID:dateDesign,featureAre,featurePer,narrative,sdsFeatu_1,
      treatmentT,user_flag,Date_sampl,pop_year,Shape_STAr,Shape_STLe))
shelby_cogon_untrt$sdsFeatu_1[shelby_cogon_untrt$sdsFeatu_1=="IMPERATA CYLINDRICA, INVASIVE"] <- "COGONGRASS"
shelby_cogon_untrt$sdsFeatu_1 <- tolower(droplevels(shelby_cogon_untrt$sdsFeatu_1))
writeOGR(shelby_cogon_untrt, "data/CampShelby/", "untreated_cogon", driver = "ESRI Shapefile")

