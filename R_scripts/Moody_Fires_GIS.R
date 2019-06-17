# Moody AFB GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)
library(stringr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####

shp_list <- list.files("data/MoodyAFB/fire-shapefiles/", pattern = "[shp]$", full.names = F)

path <- "data/MoodyAFB/fire-shapefiles/"

shp_list <- shp_list[grep("20", shp_list)]
shp_list <- shp_list[grep("Burned", shp_list)]

shp_list <- strsplit(shp_list, ".shp")

moody_fires <- st_read(paste(path,shp_list[3], ".shp", sep = ""))
moody_fires <- moody_fires %>%
      mutate(fYear = str_sub(paste(shp_list[3]), start = 15, end = 18))

projection <- st_crs(moody_fires)
for(shp in shp_list[4:length(shp_list)]){

      sf = st_read(paste(path, shp, ".shp", sep = ""))
      sf = mutate(sf, fYear = str_sub(paste(shp), start = 15, end = 18))
      sf = st_transform(sf, crs = projection)
      moody_fires <- rbind(moody_fires, sf)
      # assign(shp, sf)
}
rm(shp)
rm(shp_list)
rm(path)
rm(sf); rm(projection)



moody_fires <- moody_fires %>%
      mutate(inst_name = "Moody AFB",
             # fDate = NA,
             fDate = as.Date(paste(fYear,"-10-31", sep="")),
             fMonth = NA,
             fCause = NA,
             fType = "prescribed",
             fArea_ac = st_area(moody_fires)/4046.86,
             purpose = NA,
             season = NA) %>%
      select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose,
             fYear, fMonth)

# summary(moody_fires)

# RX2017_select <- spTransform(RX2017_select, crs(RX2013_select))
# RX2016_select <- spTransform(RX2016_select, crs(RX2013_select))
# RX2015_select <- spTransform(RX2015_select, crs(RX2013_select))
#
# rx_merged <- rbind(RX1996_select, RX1999_select, RX2003_select, RX2005_select, RX2006_select, RX2007_select, RX2009_select, RX2012_select, RX2013_select, RX2015_select, RX2016_select, RX2017_select)
# rx9699 <- raster::union(RX1996_select, RX1999_select)

# rx_merged@data$id <- rownames(rx_merged@data)
# rx_merged_pts <- fortify(rx_merged, region = "id")
# rx_merged_df <- join(rx_merged_pts, rx_merged@data, by = "id")

# writeOGR(rx_merged, dsn = "data/MoodyAFB/", layer = "rx_selected_fires_merged", driver = "ESRI Shapefile")

# library(RColorBrewer)
# library(viridis)

# ggplot(rx_merged_df) +
#       aes(long, lat, fill = fire_fiscal_year) +
#       geom_polygon() +
#       # geom_path(color = "white") +
#       coord_equal() +
#       scale_fill_gradientn(colors = inferno(n = n_distinct(rx_merged_df$fire_fiscal_year))) +
#       theme_bw()
