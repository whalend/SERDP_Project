# Camp Blanding GIS Data

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

plot_locations <- st_read("data/plot_visit.shp")# all plot locations/visits

plot_locations <- plot_locations %>%
      dplyr::rename(inst = instal,
                    visit_date = vist_dt,
                    last_fire = lst_fr_,
                    yrs_since_fire = yrs_sn_,
                    visit_yr = vist_yr,
                    imcy = imcy_nv) %>%
      dplyr::select(-instllt, -instl__) %>%
      dplyr::filter(!is.na(visit_yr))

blanding_plots <- filter(plot_locations, inst=="blanding")
## plots visited in 2017
blanding2017 <- filter(plot_locations, inst=="blanding", visit_yr==2017)

## plots visited in 2018
blanding2018 <- filter(plot_locations, inst=="blanding", visit_yr==2018)

# Camp Blanding Fire Data -------------------------------------------------

blanding_fires <- st_read("data/CampBlanding/Fire_Layer_for_Whalen.shp")
summary(blanding_fires)
blanding_fires$inst_name <- "Camp Blanding"
# blanding_fire$FID <- seq(1,nrow(blanding_fire),1)
blanding_fires <- blanding_fires %>%
      select(inst_name,
             fArea_ac = featureAre,
             fCause = ignSource,
             fType = wildland_2,
             fDate = Burn_date,
             season = Season,
             purpose)

blanding_fires <- blanding_fires %>%
      mutate(fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate))

# Camp Blanding "Growing Season" and "Dormant Season"
summary(filter(blanding_fires, season == "Growing Season"))
## a "Growing Season" burn is Feburary through September
filter(blanding_fires, season == "Growing Season", fMonth==2)

summary(filter(blanding_fires, season == "Dormant Season"))
## a "Dormant Seson" burn is October through January


# Calculate 15-year FRI ####
## This needs to be done to be accurate for plots visited in 2017 and 2018


blanding_fires$FID <- seq(1,nrow(temp),1)
# temp1 <- gCentroid(temp, byid = T)
#
# temp1 <- SpatialPointsDataFrame(temp1, temp[1])

## Summarize number of fires in growing/dormant season
# fires_blanding_plots <- st_join(
#       st_transform(blanding_plots, crs = st_crs(blanding_fires)),
#       temp %>%
#             filter(fYear > 2001, fDate < "2018-05-25")
#       ) %>%
#       group_by(plot_id, season) %>% ### issue with double counting plot id ###
#       summarise(
#             n_fires = length(plot_id)
#       )


## Calculate 15-year fire return interval for 2018 plot visits
fires2018 <- st_join(st_transform(blanding2018, crs = st_crs(blanding_fires)),
                 blanding_fires %>%
                       filter(fDate > "2003-05-25",
                              fDate < max(blanding2018$visit_date))
                 )

fri2018 <- fires2018 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            y_since_fire = w_since_fire/52,
            frindex = (1/fri15yr)/y_since_fire
                )

fires2017 <- st_join(st_transform(blanding2017, crs = st_crs(blanding_fires)),
                 blanding_fires %>%
                       filter(fDate > "2002-06-22",
                              fDate < max(blanding2017$visit_date))
                 )
fri2017 <- fires2017 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            y_since_fire = (w_since_fire/52),
            frindex = (1/fri15yr)/y_since_fire
      )
blanding_fri <- rbind(fri2017, fri2018)
summary(blanding_fri)

## Write shapefile
st_write(blanding_fri, "data/CampBlanding/blanding_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(blanding_fri), "data/CampBlanding/blanding_15yr_fri.csv")



# tmp <- st_centroid(temp, of_largest_polygon = F)
# tmp <- select(tmp, FID)
#
# tmp <- st_join(tmp, temp)
#
# fri18 <- tmp %>%
#       group_by(FID.x) %>%
#       summarise(
#             max_fYear = max(fYear, na.rm = T),
#             lastBrn18 = 2018 - max_fYear,
#             # lastBrn17 = 2017-max(fYear, na.rm = T),
#             fires = length(FID.x),
#             fri15yr_18 = 15/fires,
#             frindex_18 = (1/fri15yr_18)/lastBrn18)
# summary(fri18)
#
#
# fri18 <- st_join(temp, fri18, largest = T)
#
# blanding2018
# blanding_fires
# t <- st_transform(blanding2018, crs = st_crs(blanding_fires)) %>%
#       mutate(ptFID = seq(1, nrow(.),1)) %>%
#       st_join(.,
#               select(fri18, FID.x, lastBrn18, fires, fri15yr_18), largest = T)
#
#
# t2 <- t %>%
#       group_by(ptFID) %>%
#       summarise(fDate = max(fDate))
#
#
# fri17 <- tmp %>%
#       filter(fYear > 2001, fDate < ) %>%
#       group_by(plot_id) %>%
#       summarise(
#             lastBrn = 2017 - max(fYear, na.rm = T),
#             fires = length(FID),
#             fri15yr = 15/fires,
#             frindex = (1/fri15yr)/lastBrn)
# summary(fri17)
#
#
# fri17 <- st_join(blanding_fire, temp17)
# fri17 <- fri17 %>%
#       filter(!is.na(fri15yr)) %>%
#       select(plot_id, fri15yr, fType)
# plot(fri[1])


# temp <- raster::intersect(plot_locations, rxfire)


# cogon_fire <- readOGR("data/fire-cogon-intersect.shp")
# summary(cogon_fire)
#
# cogon_fire@data <- right_join(cogon_fire@data %>%
#       group_by(Site_Name) %>%
#       summarise(fire_frequency = (2016-2002)/length(Fire_Year)),
#       cogon_fire@data)
# st_write(fire_freq, dsn = "data/CampBlanding/cogon_fire_frequency.shp")

## Blanding Rx fire data ####

# # plot(cogon_fire, col = cogon_fire$fire_frequency)
# str(blanding_fire)
# summary(blanding_fire)
#
# rxfire <- blanding_fire[blanding_fire$fireCat == "Prescribed",]
# rxfire$burnDt <- as.Date(rxfire$burnDt)
# summary(rxfire)
# rxfire <- rxfire %>%
#       mutate(ignite = droplevels(ignite),
#              fireCat = droplevels(fireCat)) %>%
#       select(-eventDy)
#
# # library(lubridate)
# rxfire <- mutate(rxfire,
#                  burnYr = lubridate::year(burnDt),
#                  burnMo = lubridate::month(burnDt),
#                  burnMo2 = lubridate::round_date(burnDt, "month"))
# # detach("package:lubridate", unload=TRUE)
# # writeOGR(rxfire, "data/Camp-Blanding/","rxfires", "ESRI Shapefile",
# # overwrite_layer = T)

# summary(readOGR("data/Camp-Blanding/rxfires.shp"))

# ggplot(blanding_fires %>%
#              group_by(fYear, season) %>%
#              summarise(fires = length(fDate)),
#        aes(as.factor(fYear), fires, fill = season)) +
#       geom_bar(stat = "identity") +
#       # geom_point() +
#       # geom_line() +
#       # scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
#       # scale_y_continuous(limits = c(0, 13)) +
#       theme_classic() +
#       theme(legend.position = c(.15,.9),
#             legend.title = element_blank())
#
# ggplot(blanding_fires %>% group_by(fYear) %>%
#              summarise(area = sum(fArea_ac)),
#        aes(as.factor(fYear), area)) +
#       # geom_bar(stat = "identity") +
#       # geom_boxplot(aes(group = fYear)) +
#       geom_point() +
#       # geom_line() +
#       # scale_x_date(date_breaks = "1 year", date_labels = "%Y/%m") +
#       # scale_y_continuous(limits = c(0, 13)) +
#       theme_bw() +
#       xlab("Calendar year (Jan. 1 - Dec. 31)") +
#       ylab("Area burned (acres)") +
#       ggtitle("Camp Blanding")

## Make inidividual shapefiles for each fire year ####

# yr <- seq(2007,2018,1)#REQUIRED: change the end year as needed
# # library(raster)#REQUIRED in your library
# for(y in yr){
#       df <- rxfire[rxfire@data$burnYr == y,]
#       centroids <- gCentroid(df, byid = T)
#       centroids <- SpatialPointsDataFrame(
#             centroids, #SpatialPoints object
#             df[1]@data #Dataframe
#       )
#       names(centroids) <- "ptFID"
#       centroids <- raster::intersect(centroids, rxfire)
#       df@data <- left_join(
#             df@data,
#             ## Summarise data to calculate number of fires and fire frequency
#             centroids@data %>%
#                   group_by(ptFID) %>%
#                   summarise(fires = length(FID),
#                             fri15yr = (2018-2002)/fires,
#                             lastBrn = max(burnYr)
#                             # frindex = (1/fri15yr)/range(burnYr)
#                   ),
#             by = c("FID"="ptFID")
#       )
#       writeOGR(df,
#                dsn = "data/Camp-Blanding/rxfire_by_year/",
#                layer = paste("rxfire", y, sep=""),
#                driver = "ESRI Shapefile", overwrite_layer = T)
#       assign(paste("rxfire", y, sep=""), df)
# }
