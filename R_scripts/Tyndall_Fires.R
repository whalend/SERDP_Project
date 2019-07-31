# Tyndal AFB GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
tyndall_fires <- st_read("data/Tyndall/tyndall_fire_2010_2018.shp")
tyndall_fires2 <- st_read("data/Tyndall/tyndall_fire_2000_2009.shp")

tyndall_fires$inst_name = "Tyndall AFB"
summary(tyndall_fires)
tyndall_fires <- tyndall_fires %>%
      select(inst_name,
             fArea_ac = areaSize,
             fCause = causeType,
             fType = fireType,
             fDate = fireStartD) %>%
      mutate(fDate = as.Date(as.character(fDate), format = "%Y%m%d"),
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"
             ),
             purpose = NA
      ) %>%
      select(inst_name:fDate, season, purpose, fYear, fMonth)
tyndall_fires

summary(tyndall_fires2)
tyndall_fires2$inst_name = "Tyndall AFB"
tyndall_fires2 <- tyndall_fires2 %>%
      select(inst_name,
             fArea_ac = Acres,
             fCause = CAUSE,
             fType = BURN_TYPE,
             fDate = DET_DATE) %>%
      mutate(fDate = as.Date(as.character(fDate), format = "%Y-%m-%d"),
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"
             ),
             purpose = NA
      ) %>%
      select(inst_name:fDate, season, purpose, fYear, fMonth)

tyndall_fires2

tyndall_fires <- rbind(tyndall_fires, tyndall_fires2)

rm(tyndall_fires2)
# filter(eglin_fires, season=="dormant")$fMonth
# str(eglin_fires)

# convert to date format
# require(lubridate)
# eglin_fires@data$fire_date <- lubridate::ymd(eglin_fires@data$FIRESTARTD)
# eglin_fires@data$fire_year <- lubridate::year(eglin_fires@data$fire_date)
# eglin_fires@data$fire_mnth <- lubridate::floor_date(eglin_fires@data$fire_date, unit="month")
# eglin_fires@data$fire_yday <- lubridate::yday(eglin_fires@data$fire_date)
# detach("package:lubridate", unload=TRUE)

## Eglin Rx Fires (Not Run) ####
# eglin_rxfires <- eglin_fires[eglin_fires$FIRETYPE=="prescribed",]
# summary(eglin_rxfires)
# plot(eglin_rxfires[eglin_rxfires$fire_year > 2013,], col = eglin_rxfires$fire_year)
# # writeOGR(eglin_rxfires, "data/Eglin-AFB/", "eglin_rxfires", driver = "ESRI Shapefile")
#
# # shapefiles for each year
# yr <- seq(2006,2018,1)
# for(y in yr){
#       df <- eglin_rxfires[eglin_rxfires@data$fire_year == y,]
#       writeOGR(df,
#                dsn = "data/Eglin-AFB/rxfire_by_year/",
#                layer = paste("Eglin_rxfire", y, sep=""),
#                driver = "ESRI Shapefile", overwrite_layer = T)
#
# }
#
# shp_list <- list.files("data/Eglin-AFB/rxfire_by_year/", pattern = ".shp")
# shp_list <- strsplit(shp_list, ".shp")
# for(shp in shp_list){
#       assign(shp,
#              readOGR(dsn = "data/Eglin-AFB/rxfire_by_year/", layer = shp))
# }
#
# plot(`Eglin-rxfire2015`)
# plot(`Eglin-rxfire2006`)
# plot(gDifference(`Eglin-rxfire2006`, `Eglin-rxfire2015`), add = T, col = "blue")
# plot(`Eglin-rxfire2015`, add = T, col = "orange")

