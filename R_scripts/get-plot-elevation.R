#+ #Get elevation data points for plot locations

library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(sf)
library(elevatr)

plot_locations <- read_sf("data/plot-locations.shp")

plot_elevations <- get_elev_point(plot_locations, src = "epqs")

installations <- read_sf("data/selected_installations.shp")
installations_dem <- get_elev_raster(plot_locations, z = 9, clip = c("locations"), expand = 7)

plot(installations_dem)
plot(installations, add=T)
