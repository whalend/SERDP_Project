
# Plot Locations -----------------------------------------------------
library(sp); library(rgdal); library(dplyr)
# eglin_plots <- rbind(
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_19-AUG-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_18-AUG-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_17-AUG-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_16-AUG-17.gpx",
#               layer = "waypoints"))
# tyndall_plots <- rbind(
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_18-JUL-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_20-JUL-17.gpx",
#               layer = "waypoints"))
#
# avonpark_plots1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_27-JUN-17.gpx",
#                            layer = "waypoints")
# avonpark_plots2 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_28-JUN-17.gpx",
#               layer = "waypoints")
# avonpark_plots3 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_29-JUN-17.gpx",
#               layer = "waypoints")

list.files("/Volumes/GARMIN/Garmin/GPX", pattern = "AUG-17")

# blandingF1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_21-JUN-17.gpx",
#         layer = "waypoints")
# blandingE1H1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_14-JUN-17.gpx",
#         layer = "waypoints")

waypts <-
readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_30-AUG-17.gpx", layer = "waypoints")
all_plots <- readOGR("data/plot-locations.shp")
summary(all_plots)
summary(waypts)
# all_plots@coords
## Add elevation coordinates to match dimentions of 'all_plots' coords slot
# waypts@coords <- cbind(waypts@coords, waypts@data$ele)

names(all_plots)
names(waypts)
waypts@data <- select(waypts@data, time, name, ele) %>%
      rename(elevGPS = ele) %>%
      mutate(descriptio = "Semipermanent plot", blockID = "", blockDesc = "",
             fire_year = "")
# waypts <- waypts[-7,]
waypts@data <- droplevels(
      select(waypts@data, time, name, descriptio:blockDesc, elevGPS, fire_year))

all_plots <- rbind(all_plots, waypts)
# spRbind(all_plots, waypts)

summary(all_plots)
all_plots$name


writeOGR(all_plots, "data", "plot-locations",
         "ESRI Shapefile", overwrite_layer = T)
