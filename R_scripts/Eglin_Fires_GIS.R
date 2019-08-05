# Eglin AFB GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
eglin_fires <- st_read("data/EglinAFB/Eglin_Fire_Database_04_26_2018.shp")
eglin_fires$inst_name = "Eglin AFB"
summary(eglin_fires)
eglin_fires <- eglin_fires %>%
      select(inst_name,
             fArea_ac = AREASIZE,
             fCause = CAUSETYPE,
             fType = FIRETYPE,
             fDate = FIRESTARTD) %>%
      mutate(fDate = as.Date(as.character(fDate), format = "%Y%m%d"),
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"
             ),
             purpose = NA
      ) %>%
      select(inst_name:fDate, season, purpose, fYear, fMonth)
# summary(eglin_fires)

old_fires <- st_read("data/EglinAFB/Eglin_Old_Fire_Database_09_12_2016.shp")
# summary(old_fires)
# unique(old_fires$EVENT_DESC)
old_fires$inst_name = "Eglin AFB"
old_fires <- old_fires %>%
      select(inst_name,
             fArea_ac = AREA_SIZE,
             fCause = CAUSE_NAME,
             fType = TYPE_BURN,
             fDate = BURN_DATE) %>%
      mutate(fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"
             ),
             purpose = NA
      ) %>%
      select(inst_name:fDate, season, purpose, fYear, fMonth) %>%
      filter(fYear > 2001, fDate < "2008-04-27")

eglin_fires <- rbind(eglin_fires, old_fires)
summary(eglin_fires)

st_write(eglin_fires, "data/EglinAFB/eglin_fires_2002_2018.shp")


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
eglin_plots <- filter(plot_locations, inst=="eglin")

eglin2017 <- filter(eglin_plots, visit_yr==2017)
eglin2018 <- filter(eglin_plots, visit_yr==2018)

eglin_fires$FID <- seq(1, nrow(eglin_fires), 1)


fires2018 <- st_join(st_transform(eglin2018, crs = st_crs(eglin_fires)),
                     eglin_fires %>%
                           filter(fDate > "2003-06-15",
                                  fDate < max(eglin2018$visit_date))
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

fires2017 <- st_join(st_transform(eglin2017, crs = st_crs(eglin_fires)),
                     eglin_fires %>%
                           filter(fDate > "2002-08-19",
                                  fDate < max(eglin2017$visit_date))
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
eglin_fri <- rbind(fri2017, fri2018)
summary(eglin_fri)

## Write shapefile
st_write(eglin_fri, "data/EglinAFB/eglin_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(eglin_fri), "data/EglinAFB/eglin_15yr_fri.csv")
