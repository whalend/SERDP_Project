# Avon Park GIS Data

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

## common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")


## Read in shapefiles ####
# burn2017 <- readOGR("data/Avon-Park/fire_shapefiles/Burns_2017.shp")

## st_xxxx functions are from the sf package

shp_list <- list.files("data/AvonPark/fire_shapefiles/", pattern = "[shp]$")
shp_list <- strsplit(shp_list, ".shp")
# avp_fires <- st_read("data/AvonPark/fire_shapefiles/Burns_2006.shp")
for(shp in shp_list){
      assign(shp, st_read(dsn = "data/AvonPark/fire_shapefiles/",
                          layer = shp) %>%
                   st_transform(., crs = 26917))# match projections
      # avp_fires = st_join(avp_fires, df)
}
# remove duplicated(?) shapefiles
# rm(fixed_Burns_2017)# deleted these files from directory
rm(burn06)

## Combine shapefiles into one ####
## check field names: create and rename as necessary
# Burns_2006
avp_fires <- Burns_2006 %>%
      mutate(inst_name = "Avon Park AFR",
             fDate = as.Date(paste(Year,Month,Day, sep = "-")),
             fMonth = Month,
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"),
             purpose = NA) %>%
      select(inst_name,
             fArea_ac = Acres,
             fCause = Ignition,
             fType = Type,
             fDate,
             season,
             purpose,
             fYear = Year,
             fMonth)

avp_fires <- rbind(
      avp_fires,
      burn07 %>%
            mutate(inst_name = "Avon Park AFR",
                   fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                   fMonth = Month_,
                   season = case_when(
                         between(fMonth, 2,9) ~ "growing",
                         TRUE ~ "dormant"),
                   purpose = NA) %>%
            select(inst_name,
                   fArea_ac = Acres,
                   fCause = Ignition,
                   fType = Type,
                   fDate,
                   season,
                   purpose,
                   fYear = Year_,
                   fMonth)
)

avp_fires <- rbind(avp_fires,
                   burn08 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2009 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2010 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2011 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)


avp_fires <- rbind(avp_fires,
                   Burns_2012 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2013 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2014 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2015 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2016 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, Day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2017 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, Day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2018 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(Date),
                                fMonth = lubridate::month(fDate),
                                fYear = lubridate::year(fDate),
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Cause,
                                fType = Type,
                                fDate, season, purpose, fYear, fMonth)
)


rm(list=ls(pattern = "urn"))
rm(list=ls(pattern = "shp"))

avp_fires

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
avp_plots <- filter(plot_locations, inst=="avonpark")

avp2017 <- filter(avp_plots, visit_yr==2017)
avp2018 <- filter(avp_plots, visit_yr==2018)

avp_fires$FID <- seq(1, nrow(avp_fires), 1)


fires2018 <- st_join(st_transform(avp2018, crs = st_crs(avp_fires)),
                     avp_fires %>%
                           filter(fYear > 2002,
                                  fDate < max(avp2018$visit_date))
)

fri2018 <- fires2018 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            y_since_fire = max(visit_yr) - max(fYear),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            frindex = (1/fri15yr)/y_since_fire
      )

fires2017 <- st_join(st_transform(avp2017, crs = st_crs(avp_fires)),
                     avp_fires %>%
                           filter(fYear > 2001,
                                  fDate < max(avp2017$visit_date))
)
fri2017 <- fires2017 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            y_since_fire = max(visit_yr) - max(fYear),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            frindex = (1/fri15yr)/(w_since_fire/52)
      )
avp_fri <- rbind(fri2017, fri2018)
summary(avp_fri)

## Write shapefile
st_write(avp_fri, "data/AvonPark/avp_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(avp_fri), "data/AvonPark/avp_15yr_fri.csv")
