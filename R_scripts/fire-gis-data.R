# Fire Data
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr);
library(ggplot2)
#library(RSAGA)
# library(raster)

plot_locations <- st_read("data/plot_visit.shp")

plot_locations <- plot_locations %>%
      rename(inst = instal,
             visit_date = vist_dt,
             last_fire = lst_fr_,
             yrs_since_fire = yrs_sn_,
             visit_yr = vist_yr,
             imcy = imcy_nv) %>%
      select(-instllt, -instl__) %>%
      filter(!is.na(visit_yr))

# summary(plot_locations)
# filter(plot_locations, is.na(last_fire))$plot_id
## no fire history for shelby k1

source("R_scripts/AvonPark_GIS.R")
source("R_scripts/Blanding_GIS.R")
source("R_scripts/Eglin_GIS.R")


fire_master <- rbind(
      st_transform(blanding_fire, crs = 4326),
      st_transform(eglin_fires, crs = 4326)
)

## Working on calculation a fire return interval ####

# plot(rxfire[1])
# plot(st_centroid(rxfire, of), add = T, pch = 3, size = 5)
# plot(tmp)
# st_point_on_surface(rxfire)
temp <- blanding_fire
temp$FID <- seq(1,nrow(temp),1)
# temp1 <- gCentroid(temp, byid = T)
#
# temp1 <- SpatialPointsDataFrame(temp1, temp[1])

tmp <- st_centroid(temp, of_largest_polygon = T)
tmp <- select(tmp, FID)

tmp <- st_join(tmp, temp)

tmp <- tmp %>%
      filter(fYear > 2002) %>%
      group_by(FID.x) %>%
      summarise(
            lastBrn = 2018-max(fYear, na.rm = T),
            fires = length(FID.x),
            fri15yr = 15/fires,
            frindex = (1/fri15yr)/lastBrn)
tmp <- st_join(temp, tmp)
plot(tmp[14])
# plot(plot_locations[1], add = T, pch = 3, size = 10)


# st_crs(rxfire)
temp <- st_transform(plot_locations, crs = st_crs(temp)) %>%
      mutate(ptFID = seq(1, nrow(.),1)) %>%
      st_join(.,
              select(temp, FID, fYear))

# temp <- raster::intersect(plot_locations, rxfire)
temp17 <- temp %>%
      filter(visit_yr == 2017, fYear<2018) %>%
      group_by(plot_id) %>%
      summarise(
            lastBrn = 2017-max(fYear, na.rm = T),
            fires = length(FID),
            fri15yr = 15/fires,
            frindex = (1/fri15yr)/lastBrn)



fri <- st_join(blanding_fire, temp17)
fri <- fri %>%
      filter(!is.na(fri15yr)) %>%
      select(plot_id, fri15yr, fType)
plot(fri[1])

# detach("package:raster", unload=TRUE)

plot(gDifference(rxfire2012, rxfire[rxfire$burnYr>2012,], byid = T), col='red',pbg='white')
plot(gDifference(rxfire2012, rxfire2015), col='red',pbg='white', add=T)



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

## how do i randomly distribute points into a multi-polygon shapefile?
# test_sample <- lapply(rxfire2016,spsample(n=4, type="random"))
# tmp <- as.SpatialPolygons.owin(rxfire2016)
#
# blanding_fire_2018 <- readOGR("data/CampBlanding/Fire_Layer_for_Whalen.shp")
# summary(blanding_fire_2018)
# blanding_fire_2018$wildlandFi <- as.character(blanding_fire_2018$wildlandFi)
# blanding_rx_fire <- blanding_fire_2018[blanding_fire$wildlandFi=="Prescribed",]






# Moody AFB ---------------------------------------------------------------
shp_list <- list.files("data/MoodyAFB/fire-shapefiles/", pattern = "[shp]$")
shp_list <- strsplit(shp_list, ".shp")
for(shp in shp_list){
      # df <- readOGR(dsn = "data/Avon-Park/fire_shapefiles/", layer = shp)
      # df <-
      assign(shp,
             readOGR(dsn = "data/MoodyAFB/fire-shapefiles/", layer = shp))
}
rm(shp)
rm(shp_list)
rm(list = ls(pattern = "Moody"))

# Assign and fill new field with fire fiscal year
shp_list <- as.list(.GlobalEnv)

library(stringi)
shp_number <- 1
for(shp in shp_list){
      year = stri_sub(names(shp_list)[shp_number], from = 3, length = 4)
      shp$fire_fiscal_year = year
      assign(names(shp_list)[shp_number], shp)
      shp_number = shp_number + 1
      rm(shp)
}

RX2017_select <- spTransform(RX2017_select, crs(RX2013_select))
RX2016_select <- spTransform(RX2016_select, crs(RX2013_select))
RX2015_select <- spTransform(RX2015_select, crs(RX2013_select))

rx_merged <- rbind(RX1996_select, RX1999_select, RX2003_select, RX2005_select, RX2006_select, RX2007_select, RX2009_select, RX2012_select, RX2013_select, RX2015_select, RX2016_select, RX2017_select)
# rx9699 <- raster::union(RX1996_select, RX1999_select)

rx_merged@data$id <- rownames(rx_merged@data)
rx_merged_pts <- fortify(rx_merged, region = "id")
rx_merged_df <- join(rx_merged_pts, rx_merged@data, by = "id")

writeOGR(rx_merged, dsn = "data/MoodyAFB/", layer = "rx_selected_fires_merged", driver = "ESRI Shapefile")

library(RColorBrewer)
library(viridis)

ggplot(rx_merged_df) +
      aes(long, lat, fill = fire_fiscal_year) +
      geom_polygon() +
      # geom_path(color = "white") +
      coord_equal() +
      scale_fill_gradientn(colors = inferno(n = n_distinct(rx_merged_df$fire_fiscal_year))) +
      theme_bw()


# Fort Gordon ####
gordon_fire <- readOGR("data/FtGordon/wildland_fire.shp")
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
