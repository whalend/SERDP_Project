# Fire Data
library(sp); library(rgdal); library(maptools); library(rgeos)
library(dplyr);
library(ggplot2)
# library(sf);
# Camp Blanding Fire Data -------------------------------------------------


blanding_fire <- readOGR("data/Camp-Blanding/Camp_Blanding_Fire.shp")
summary(blanding_fire)
blanding_fire@data$installation <- "Camp Blanding"
blanding_fire@data$FID <- seq(1,nrow(blanding_fire@data),1)

cogon_fire <- readOGR("data/fire-cogon-intersect.shp")
summary(cogon_fire)

cogon_fire@data <- right_join(cogon_fire@data %>%
      group_by(Site_Name) %>%
      summarise(fire_frequency = (2016-2002)/length(Fire_Year)),
      cogon_fire@data)
# st_write(fire_freq, dsn = "data/CampBlanding/cogon_fire_frequency.shp")

plot(cogon_fire, col = cogon_fire$fire_frequency)

rxfire <- blanding_fire[blanding_fire$wildland_2 == "prescribed_fire",]
rxfire@data$Burn_date <- as.Date(rxfire@data$Burn_date)
summary(rxfire)
library(lubridate)
rxfire@data <- mutate(rxfire@data,
                 burn_year = year(Burn_date),
                 burn_month = month(Burn_date),
                 burn_month2 = round_date(Burn_date, "month"))
detach("package:lubridate", unload=TRUE)


ggplot(rxfire@data %>% group_by(burn_month2) %>%
             summarise(fires = length(Burn_date)),
       aes(burn_month2, fires)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      # geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
      # scale_y_continuous(limits = c(0, 13)) +
      theme_bw()

ggplot(rxfire@data %>% group_by(burn_year) %>%
             summarise(area = sum(featureAre)),
       aes(burn_year, area)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      # geom_line() +
      # scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
      # scale_y_continuous(limits = c(0, 13)) +
      theme_bw()



yr <- seq(2002,2017,1)
for(y in yr){
      df <- rxfire[rxfire@data$burn_year == y,]
      writeOGR(df,
               dsn = "data/Camp-Blanding/rxfire_by_year/",
               layer = paste("Blanding-rxfire", y, sep=""),
               driver = "ESRI Shapefile", overwrite_layer = T)

}

shp_list <- list.files("data/Camp-Blanding/rxfire_by_year/", pattern = ".shp")
shp_list <- strsplit(shp_list, ".shp")
for(shp in shp_list){
      assign(shp,
             readOGR(dsn = "data/Camp-Blanding/rxfire_by_year/", layer = shp))
}

library(raster)
rxfire2017polys <- intersect(rxfire2017,rxfire)
summary(rxfire2017polys)
plot(rxfire2017polys, col = rxfire2017polys$burn_yr)
plot(rxfire2017, add = T)

rxfire2017polys@data <- rxfire2017polys@data %>%
      group_by(FID.1) %>%
      length(FID.2)
      summarise(fire_frequency = (max(burn_yr)-min(burn_yr))/length(FID.2))



# blanding_fire <- readOGR("data/CampBlanding/Camp_Blanding_Fire.shp")
# summary(blanding_fire)
# unique(blanding_fire$sdsFeatu_1)
# plot(blanding_fire, col = blanding_fire$sdsFeatu_1)

# rx_fire_blanding<- readOGR("data/CampBlanding/rxFires.shp")
# summary(rx_fire_blanding)

## how do i randomly distribute points into a multi-polygon shapefile?
# test_sample <- lapply(rxfire2016,spsample(n=4, type="random"))
# tmp <- as.SpatialPolygons.owin(rxfire2016)


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