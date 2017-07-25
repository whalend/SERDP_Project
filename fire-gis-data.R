# Fire Data
library(sp); library(rgdal); library(maptools); library(rgeos); #library(RSAGA)
library(plyr); library(dplyr);
library(ggplot2)
# library(sf);

# Camp Blanding Fire Data -------------------------------------------------

blanding_fire <- readOGR("data/Camp-Blanding/Camp_Blanding_Fire.shp", integer64 = "allow.loss")
summary(blanding_fire)
blanding_fire@data$installation <- "Camp Blanding"
blanding_fire@data$FID <- seq(1,nrow(blanding_fire@data),1)
blanding_fire@data <- blanding_fire@data %>%
      select(-sdsFeatu_1, -wildland_1) %>%
      select(FID, everything()) %>%
      rename(eventDy = dateEvent, fArea = featureAre, fPeri = featurePer,
             risklvl = riskLevel, fireCat = wildlandFi, fireTyp = wildland_2,
             burnDt = Burn_date, ignite = ignSource, FireYr = Fire_Year)

cogon_fire <- readOGR("data/fire-cogon-intersect.shp")
summary(cogon_fire)

cogon_fire@data <- right_join(cogon_fire@data %>%
      group_by(Site_Name) %>%
      summarise(fire_frequency = (2016-2002)/length(Fire_Year)),
      cogon_fire@data)
# st_write(fire_freq, dsn = "data/CampBlanding/cogon_fire_frequency.shp")

plot(cogon_fire, col = cogon_fire$fire_frequency)

blanding_fire@data$fireCat[is.na(blanding_fire@data$fireCat)] <- "TBD"
rxfire <- blanding_fire[blanding_fire$fireCat == "Prescribed",]
rxfire@data$burnDt <- as.Date(rxfire@data$burnDt)
summary(rxfire)
rxfire@data <- droplevels(rxfire@data %>%
      select(-ignite, -fireCat))

# library(lubridate)
rxfire@data <- mutate(rxfire@data,
                 burnYr = lubridate::year(burnDt),
                 burnMo = lubridate::month(burnDt),
                 burnMo2 = lubridate::round_date(burnDt, "month"))
# detach("package:lubridate", unload=TRUE)
writeOGR(rxfire, "data/Camp-Blanding/","rxfires", "ESRI Shapefile",
         overwrite_layer = T)

# summary(readOGR("data/Camp-Blanding/rxfires.shp"))

ggplot(rxfire@data %>% group_by(burnMo2) %>%
             summarise(fires = length(burnDt)),
       aes(burnMo2, fires)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      # geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
      # scale_y_continuous(limits = c(0, 13)) +
      theme_bw()

ggplot(rxfire@data %>% group_by(burnYr) %>%
             summarise(area = sum(fArea)),
       aes(burnYr, area)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      # geom_line() +
      # scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
      # scale_y_continuous(limits = c(0, 13)) +
      theme_bw()

temp <- gCentroid(rxfire, byid = T)
temp <- SpatialPointsDataFrame(temp, rxfire[1]@data)
names(temp) <- "ptFID"
temp <- raster::intersect(temp, rxfire)
df <- temp@data %>%
      group_by(ptFID) %>%
      summarise(fires = length(FID),
                fri15yr = (2017-2002)/fires,
                lastBrn = (2018)-max(burnYr),
                frindex = (1/fri15yr)/lastBrn)
rxfire@data <- left_join(rxfire@data, df, by = c("FID" = "ptFID"))
plot(rxfire, col = rxfire$fri15yr)

writeOGR(rxfire, "data/Camp-Blanding/","rxfires2","ESRI Shapefile")

yr <- seq(2007,2017,1)
# library(raster)
for(y in yr){
      df <- rxfire[rxfire@data$burnYr == y,]
      centroids <- gCentroid(df, byid = T)
      centroids <- SpatialPointsDataFrame(
            centroids, #SpatialPoints object
            df[1]@data #Dataframe
            )
      names(centroids) <- "ptFID"
      centroids <- raster::intersect(centroids, rxfire)
      df@data <- left_join(
            df@data,
            ## Summarise data to calculate number of fires and fire frequency
            centroids@data %>%
                  group_by(ptFID) %>%
                  summarise(fires = length(FID),
                            fri15yr = (2017-2002)/fires,
                            lastBrn = max(burnYr)
                            # frindex = (1/fri15yr)/range(burnYr)
                            ),
            by = c("FID"="ptFID")
            )
      writeOGR(df,
               dsn = "data/Camp-Blanding/rxfire_by_year/",
               layer = paste("rxfire", y, sep=""),
               driver = "ESRI Shapefile", overwrite_layer = T)
      assign(paste("rxfire", y, sep=""), df)
}
detach("package:raster", unload=TRUE)

plot(gDifference(rxfire2012, rxfire[rxfire$burnYr>2012,], byid = T), col='red',pbg='white')
plot(gDifference(rxfire2012, rxfire2015), col='red',pbg='white', add=T)
# Read in shapefiles from directory ####
# shp_list <- list.files("data/Camp-Blanding/rxfire_by_year/", pattern = ".shp")
# shp_list <- strsplit(shp_list, ".shp")
# for(shp in shp_list){
#       assign(shp,
#              readOGR(dsn = "data/Camp-Blanding/rxfire_by_year/", layer = shp))
# }


# Intersect shapefiles ####
## Goal: calculate fire return interval for polygons of time since last fire

# intersecting polygons, ends up including small overlapping or touching areas
rxfire2017intersect <- intersect(rxfire2017,rxfire)
summary(rxfire2017intersect)
plot(rxfire2017intersect, col = rxfire2017intersect$burn_year)
plot(rxfire2017, add = T)

# rxfire2017@data <- left_join(
#       rxfire2017@data,
#       rxfire2017intersect@data %>%
#       group_by(FID.1) %>%
#       # length(FID.2)
#       summarise(
#             fires = length(FID.2),
#             fire_frequency = length(FID.2)/(max(burn_year)-min(burn_year))
#             ),
#       by = c("FID"="FID.1"))


# rxfire2015intersect <- intersect(rxfire2015,rxfire)
# summary(rxfire2015intersect)
# plot(rxfire2015intersect, col = rxfire2015intersect$burn_year)
# plot(rxfire2015, add = T)
# filter(rxfire2015intersect@data, FID==578)

# crs(plot_locations)
# plot_locations <- spTransform(plot_locations, CRSobj = crs(rxfire))
# intrsct <- intersect(plot_locations, rxfire)
# intrsct@data %>% group_by(name) %>% summarise(fri = length(name))

## Tool in development that might get what we want directly at some point
# devtools::install_github("openfigis/RFigisGeo")
# # library(RFigisGeo)
# rxfire2015intersect2 <- RFigisGeo::intersection(rxfire2015, rxfire)
# filter(rxfire2015intersect2@data, FID==578)


## Generate polygon centroids, intersect these with all Rx fires and join the attributes back to the original polygon shapefile

rxfire2015centroids <- gCentroid(rxfire2015, byid = T)
rxfire2015centroids <- SpatialPointsDataFrame(
      rxfire2015centroids, #SpatialPoints object
      rxfire2015[1]@data #Dataframe
      )
names(rxfire2015centroids) <- "ptFID"
# extract(rxfire2015, rxfire2015centroids)
## Check to make sure 'FID' numbers match up between centroids and polygons
intersect(rxfire2015centroids, rxfire2015)@data

## Intersect the identified centroids with the multipolygon shapefile
rxfire2015centroids <- intersect(rxfire2015centroids, rxfire)
# length(unique(rxfire2015centroids$FID))


## Join data back to the 2015 prescribed fire polygon shapefile attributes
rxfire2015@data <- left_join(
      rxfire2015@data,
      ## Summarise data to calculate number of fires and fire frequency
      rxfire2015centroids@data %>%
            filter(burnYr <= 2015) %>%
            group_by(ptFID) %>%
            summarise(
                  fires = length(FID),
                  fri10yr = 10/fires
            ),
      by = c("FID"="ptFID")
      )

# blanding_fire <- readOGR("data/CampBlanding/Camp_Blanding_Fire.shp")
# summary(blanding_fire)
# unique(blanding_fire$sdsFeatu_1)
# plot(blanding_fire, col = blanding_fire$sdsFeatu_1)

# rx_fire_blanding<- readOGR("data/CampBlanding/rxFires.shp")
# summary(rx_fire_blanding)

## how do i randomly distribute points into a multi-polygon shapefile?
# test_sample <- lapply(rxfire2016,spsample(n=4, type="random"))
# tmp <- as.SpatialPolygons.owin(rxfire2016)




# Plot Locations -----------------------------------------------------
library(sp); library(rgdal); library(dplyr)
waypts <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_26-MAY-17.gpx",
                  layer = "waypoints")
# ogrListLayers(("/Volumes/GARMIN/Garmin/GPX/Waypoints_26-MAY-17.gpx"))
all_plots <- readOGR("data/plot-locations.shp")
summary(all_plots)
summary(waypts)
# all_plots@coords
## Add elevation coordinats to match dimentions of 'all_plots' coords slot
waypts@coords <- cbind(waypts@coords, waypts@data$ele)

names(all_plots); names(waypts)
waypts@data <- select(waypts@data, time, name, ele) %>%
      rename(elevGPS = ele) %>%
      mutate(descriptio = "Semipermanent plot", blockID = "", blockDesc = "")
waypts <- waypts[-7,]
waypts@data <- droplevels(
      select(waypts@data, time, name, descriptio:blockDesc, elevGPS))

all_plots <- rbind(all_plots, waypts)
# spRbind(all_plots, waypts)

summary(all_plots)

writeOGR(all_plots, "data", "plot-locations",
         "ESRI Shapefile", overwrite_layer = T)


# Eglin AFB  --------------------------------------------------------------
eglin_fires <- readOGR("data/Eglin-AFB/Eglin_Fire_History_2006_2017.shp", integer64 = "allow.loss")
summary(eglin_fires)
eglin_fires@data <- select(eglin_fires@data, -SDSMETADAT, -AREASIZEUO, -(PERIMETERS:MGRSCENTRO), -CONTAINDAT, -INSTALLATI, -(CREATEDATE:SHAPE_LEN))
str(eglin_fires@data)

# convert to date format
library(lubridate)
eglin_fires@data$fire_date <- ymd(eglin_fires@data$FIRESTARTD)
eglin_fires@data$fire_year <- year(eglin_fires@data$fire_date)
eglin_fires@data$fire_mnth <- floor_date(eglin_fires@data$fire_date, unit="month")
eglin_fires@data$fire_yday <- yday(eglin_fires@data$fire_date)
detach("package:lubridate", unload=TRUE)


eglin_rxfires <- eglin_fires[eglin_fires$FIRETYPE=="prescribed",]
summary(eglin_rxfires)
plot(eglin_rxfires[eglin_rxfires$fire_year > 2013,], col = eglin_rxfires$fire_year)
writeOGR(eglin_rxfires, "data/Eglin-AFB/", "eglin_rxfires", driver = "ESRI Shapefile")

# shapefiles for each year
yr <- seq(2006,2017,1)
for(y in yr){
      df <- eglin_rxfires[eglin_rxfires@data$fire_year == y,]
      writeOGR(df,
               dsn = "data/Eglin-AFB/rxfire_by_year/",
               layer = paste("Eglin_rxfire", y, sep=""),
               driver = "ESRI Shapefile", overwrite_layer = T)

}

shp_list <- list.files("data/Eglin-AFB/rxfire_by_year/", pattern = ".shp")
shp_list <- strsplit(shp_list, ".shp")
for(shp in shp_list){
      assign(shp,
             readOGR(dsn = "data/Eglin-AFB/rxfire_by_year/", layer = shp))
}

plot(`Eglin-rxfire2015`)
plot(`Eglin-rxfire2006`)
plot(gDifference(`Eglin-rxfire2006`, `Eglin-rxfire2015`), add = T, col = "blue")
plot(`Eglin-rxfire2015`, add = T, col = "orange")


# Avon Park AFB -----------------------------------------------------------
# burn2017 <- readOGR("data/Avon-Park/fire_shapefiles/Burns_2017.shp")

shp_list <- list.files("data/Avon-Park/fire_shapefiles/", pattern = "[shp]$")
shp_list <- strsplit(shp_list, ".shp")
for(shp in shp_list){
      # df <- readOGR(dsn = "data/Avon-Park/fire_shapefiles/", layer = shp)
      # df <-
      assign(shp,
             readOGR(dsn = "data/Avon-Park/fire_shapefiles/", layer = shp))
}
summary(burn06)
burn06$Type[burn06$Type=="prescribed"] <- "Prescribed"
burn06@data <- droplevels(burn06@data)
writeOGR(burn06, "data/Avon-Park/fire_shapefiles/", "Burns_2006", driver = "ESRI Shapefile", overwrite_layer = T)
summary(burn07)
summary(burn08)
summary(Burns_2009)
Burns_2009$Type[Burns_2009$Type=="Prescibed"] <- "Prescribed"
Burns_2009@data <- droplevels(Burns_2009@data)
writeOGR(Burns_2009, "data/Avon-Park/fire_shapefiles/", "Burns_2009", driver = "ESRI Shapefile", overwrite_layer = T)
summary(Burns_2010)
summary(Burns_2011)
summary(Burns_2012)
summary(Burns_2013)
summary(Burns_2014)
summary(Burns_2015)
Burns_2015$Type[Burns_2015$Type=="Prescibed"|Burns_2015$Type=="Presrcibed"] <- "Prescribed"
Burns_2015@data <- droplevels(Burns_2015@data)
writeOGR(Burns_2015, "data/Avon-Park/fire_shapefiles/", "Burns_2015", driver = "ESRI Shapefile", overwrite_layer = T)
summary(Burns_2016)
summary(Burns_2017)

rm(shp)
shp_list <- as.list(.GlobalEnv)
for(shp in shp_list){
      df <- shp[shp@data$Type=="Prescribed",]
      # df <-
      writeOGR(df,
               dsn = "data/Avon-Park/fire_shapefiles/",
               layer = paste("AvonPark_rxfire", sep=""),
               driver = "ESRI Shapefile", overwrite_layer = T)
}

all_invasives <- readOGR("data/Avon-Park/APAFR_AllInvasives_Merge2.shp")
summary(all_invasives)
unique(all_invasives$CommonName)
avp_cogongrass <- all_invasives[all_invasives$CommonName=="Cogon Grass",]
avp_cogongrass@data <- droplevels(avp_cogongrass@data)
writeOGR(avp_cogongrass, "data/Avon-Park/", "avp_cogon", driver = "ESRI Shapefile")


readShapePoly()
sf::read_sf("data/Avon-Park/FireBreaks.shp")
