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

gordon_fire@data$DATE_EVENT <- as.Date(
      as.character(gordon_fire@data$DATE_EVENT),
      format = "%Y%m%d")

gordon_fire@data <- mutate(gordon_fire@data,
                           burnYr = lubridate::year(DATE_EVENT),
                           burnMo = lubridate::month(DATE_EVENT),
                           burnMo2 = lubridate::round_date(DATE_EVENT, "month"))
summary(gordon_fire@data)

yr <- seq(1995,2018,1)
library(raster)
for(y in yr){
      df <- gordon_fire[gordon_fire@data$burnYr == y,]
      centroids <- rgeos::gCentroid(df, byid = T)
      centroids <- SpatialPointsDataFrame(
            centroids, #SpatialPoints object
            df[1]@data #Dataframe
      )
      names(centroids) <- "ptFID"
      centroids <- raster::intersect(centroids, gordon_fire)
      df@data <- left_join(
            df@data,
            ## Summarise data to calculate number of fires and fire frequency
            gordon_fire@data %>%
                  filter(burnYr >= 2003) %>%
                  group_by(OBJECTID) %>%
                  summarise(fires = length(OBJECTID),
                            fri15yr = (2018-2003)/fires,
                            lastBrn = max(burnYr)
                            # frindex = (1/fri15yr)/range(burnYr)
                  ),
            by = "OBJECTID"
      )
      writeOGR(df,
               dsn = "data/FtGordon/",
               layer = paste("gordon_fire_", y, sep=""),
               driver = "ESRI Shapefile", overwrite_layer = T)
      assign(paste("gordon_fire_", y, sep=""), df)
}
detach("package:raster", unload=TRUE)