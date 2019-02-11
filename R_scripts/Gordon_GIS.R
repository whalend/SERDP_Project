# Fort Gordon GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
gordon_fire <- st_read("data/FtGordon/wildland_fire.shp")
summary(gordon_fire)
names(gordon_fire)

gordon_fire <- gordon_fire %>%
      mutate(inst_name = "Fort Gordon",
             fDate = as.Date( as.character(gordon_fire$DATE_EVENT), format = "%Y%m%d"),
            fYear = lubridate::year(fDate),
            fMonth = lubridate::month(fDate),
            season = case_when(
                  sdsFeature=="Dormant" ~ "Dormant",
                  sdsFeature=="Growing" ~ "Growing",
                  TRUE ~ "NA"
            ),
            fType = case_when(
                  sdsFeature=="Prescribed Burn" ~ "prescribed",
                  sdsFeature=="Prescribed Fire" ~ "prescribed",
                  TRUE ~ "NA"
            )) %>%
      select(inst_name,
             fArea_ac = featureAre,
             fCause = wildlandFi,
             fType, fDate, season, purpose, fYear, fMonth)

summary(gordon_fire)

# yr <- seq(1995,2018,1)
# library(raster)
# for(y in yr){
#       df <- gordon_fire[gordon_fire$fYear == y,]
#       centroids <- rgeos::gCentroid(df, byid = T)
#       centroids <- SpatialPointsDataFrame(
#             centroids, #SpatialPoints object
#             df[1]@data #Dataframe
#       )
#       names(centroids) <- "ptFID"
#       centroids <- raster::intersect(centroids, gordon_fire)
#       df@data <- left_join(
#             df@data,
#             ## Summarise data to calculate number of fires and fire frequency
#             gordon_fire@data %>%
#                   filter(burnYr >= 2003) %>%
#                   group_by(OBJECTID) %>%
#                   summarise(fires = length(OBJECTID),
#                             fri15yr = (2018-2003)/fires,
#                             lastBrn = max(burnYr)
#                             # frindex = (1/fri15yr)/range(burnYr)
#                   ),
#             by = "OBJECTID"
#       )
#       writeOGR(df,
#                dsn = "data/FtGordon/",
#                layer = paste("gordon_fire_", y, sep=""),
#                driver = "ESRI Shapefile", overwrite_layer = T)
#       assign(paste("gordon_fire_", y, sep=""), df)
# }
# detach("package:raster", unload=TRUE)
