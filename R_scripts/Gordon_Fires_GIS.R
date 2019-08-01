# Fort Gordon GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
gordon_fire <- st_read("data/FtGordon/wildland_fire.shp", quiet = T)
summary(gordon_fire)
names(gordon_fire)

gordon_fire <- gordon_fire %>%
      mutate(inst_name = "Fort Gordon",
             fDate = as.Date( as.character(gordon_fire$DATE_EVENT), format = "%Y%m%d"),
            fYear = lubridate::year(fDate),
            fMonth = lubridate::month(fDate),
            season = case_when(
                  sdsFeature=="Dormant" ~ "Dormant",
                  sdsFeature=="Growing" ~ "Growing",
                  TRUE ~ "NA"
            ),
            ## Estimate that the "Growing" season they identified begins on March 15th
            tmp = as.numeric(fMonth,lubridate::day(fDate), sep=""),
            season = if_else(
                  between(tmp, 315,930), "growing","dormant"
                  ),
            fType = case_when(
                  sdsFeature=="Prescribed Burn" ~ "prescribed",
                  sdsFeature=="Prescribed Fire" ~ "prescribed",
                  TRUE ~ "NA"
            )) %>%
      select(inst_name,
             fArea_ac = featureAre,
             fCause = wildlandFi,
             fType, fDate, season, purpose, fYear, fMonth)

summary(gordon_fire)


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
gordon_plots <- filter(plot_locations, inst=="gordon")


gordon2018 <- filter(gordon_plots, visit_yr==2018)

gordon_fire$FID <- seq(1, nrow(gordon_fire), 1)


fires2018 <- st_join(st_transform(gordon2018, crs = st_crs(gordon_fire)),
                     gordon_fire %>%
                           filter(fDate > "2003-08-22",
                                  fDate < max(gordon2018$visit_date))
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

gordon_fri <-fri2018
summary(gordon_fri)

## Write shapefile
st_write(gordon_fri, "data/FtGordon/gordon_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(gordon_fri), "data/FtGordon/gordon_15yr_fri.csv")
