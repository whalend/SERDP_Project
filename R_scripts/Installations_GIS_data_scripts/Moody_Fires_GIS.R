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
             fArea_ac = as.numeric(st_area(moody_fires)/4046.86),# convert square meters of st_area calculation to acres
             season = NA,# no season because don't have actual fire dates
             purpose = NA) %>%
      select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose,
             fYear, fMonth)

# summary(moody_fires)

st_write(moody_fires, "data/MoodyAFB/moody_fires_2002_2017.shp")


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

# unique(plot_locations$inst)
moody_plots <- filter(plot_locations, inst=="moody")

moody2017 <- filter(moody_plots, visit_yr==2017)
# moody2018 <- filter(moody_plots, visit_yr==2018)

moody_fires$FID <- seq(1, nrow(moody_fires), 1)

## No 2018 visit
# fires2018 <- st_join(st_transform(moody2018, crs = st_crs(moody_fires)),
#                      moody_fires %>%
#                            filter(fDate > "2003-06-01",
#                                   fDate < max(moody2018$visit_date))
# )
#
# fri2018 <- fires2018 %>%
#       group_by(plot_id, inst_nm, visit_date) %>%
#       summarise(
#             n_fires = length(plot_id),
#             fri15yr = 15/n_fires,
#             d_since_fire = max(visit_date) - max(fDate),
#             w_since_fire = d_since_fire/7,
#             y_since_fire = w_since_fire/52,
#             frindex = (1/fri15yr)/y_since_fire
#       )

fires2017 <- st_join(st_transform(moody2017, crs = st_crs(moody_fires)),
                     moody_fires %>%
                           filter(fDate > "2002-08-30",
                                  fDate < max(moody2017$visit_date))
)
fri2017 <- fires2017 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            y_since_fire = w_since_fire/52,
            frindex = (1/fri15yr)/y_since_fire
      )
moody_fri <- fri2017
summary(moody_fri)

## Write shapefile
st_write(moody_fri, "data/MoodyAFB/moody_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(moody_fri), "data/MoodyAFB/moody_15yr_fri.csv")

