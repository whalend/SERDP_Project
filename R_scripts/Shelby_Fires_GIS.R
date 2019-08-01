# Camp Shelby GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
## data provided by Camp Shelby personnel
## shelby_fire_history <- st_read("data/CampShelby/FireHistory.shp")
## data provided by USFS is "Prescribed_Burn" or "DeSoto_Rx_Blocks"

path <- "data/CampShelby/fire_gis/"
shp_list <- list.files(path, pattern = "[shp]$", full.names = F)
shp_list <- strsplit(shp_list, ".shp")

## Files 1-4 in the list have a varying number of columns
# st_read(shp_list[4])
# shp_list_1_4 <- shp_list[1:4]

## I tried aggregating some subsets that appeared to have some matching, but it looks like there may be some major errors. I'm revising to going through the files one-by-one. (Uggh)


for(shp in shp_list){
      assign(shp, st_read(paste(path, shp, ".shp", sep = "")))
}
rm(shp)
rm(shp_list)
rm(path)


shelby_fires <- DeSoto_Rx_Blocks_2018_Proposed %>%
      mutate(inst_name = "Camp Shelby",
             fDate = NA,
             fMonth = NA,
             fYear = 2018,
             fCause = "proposed",
             fType = "prescribed"
      ) %>%
      select(inst_name,
             fArea_ac = ACREAGE,
             fCause, fType, fDate,
             season = SEASON,
             purpose = PURPOSE,
             fYear, fMonth)
# summary(shelby_fires)

shelby_fires <- rbind(
      shelby_fires,
      DeSoto_Rx_Blocks_2017 %>%
      mutate(inst_name = "Camp Shelby",
             fDate = NA,
             fMonth = NA,
             fYear = 2017,
             fCause = NA,
             fType = "prescribed"
             ) %>%
      select(inst_name,
             fArea_ac = ACREAGE,
             fCause, fType, fDate,
             season = SEASON,
             purpose = PURPOSE,
             fYear, fMonth)
)
# summary(shelby_fires)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY16 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2016,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY15 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2015,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY14 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2014,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY13 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2013,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY12 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2012,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY11 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2011,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY10 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2010,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY09 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2009,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY08 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2008,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY07 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2007,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY06 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2006,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY05 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fMonth = NA,
                   fYear = 2005,
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY04 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = as.Date(as.character(DATE_BURNE), format = "%m%d%y"),
                   fMonth = lubridate::month(fDate),
                   fYear = lubridate::year(fDate),
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY03 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = as.Date(as.character(DATE_BURNE), format = "%m/%d/%Y"),
                   fMonth = lubridate::month(fDate),
                   fYear = lubridate::year(fDate),
                   fYear = if_else(is.na(fYear)==TRUE,2003,fYear),
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)

shelby_fires <- rbind(
      shelby_fires,
      Prescribed_Burn_Blocks_FY02 %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = as.Date(as.character(DATE_BURNE), format = "%m/%d/%y"),
                   fMonth = lubridate::month(fDate),
                   fYear = lubridate::year(fDate),
                   fCause = NA,
                   fType = "prescribed"
            ) %>%
            select(inst_name,
                   fArea_ac = ACREAGE,
                   fCause, fType, fDate,
                   season = SEASON,
                   purpose = PURPOSE,
                   fYear, fMonth)
)
summary(shelby_fires)


FireHistory <- FireHistory %>%
      mutate(fYear = case_when(
            !is.na(fy14) ~ 2014,
            !is.na(FY15) ~ 2015,
            !is.na(FY16) ~ 2016,
            !is.na(FY17) ~ 2017,
            !is.na(FY13) ~ 2013,
            !is.na(FY12) ~ 2012,
            !is.na(FY11) ~ 2011,
            !is.na(FY10) ~ 2010,
            !is.na(FY18) ~ 2018,
            !is.na(FY19) ~ 2019
      )) %>%
      st_transform(., crs = st_crs(shelby_fires))


shelby_fires <- rbind(
      shelby_fires,
      FireHistory %>%
            mutate(inst_name = "Camp Shelby",
                   fDate = NA,
                   fDate = as.Date(fDate),
                   fMonth = NA,
                   fCause = NA,
                   fType = "prescribed",
                   purpose = NA
            ) %>%
            select(inst_name,
                   fArea_ac = Acres,
                   fCause, fType, fDate,
                   season = Season,
                   purpose, fYear, fMonth)
) %>%
      mutate(fDate = as.Date(paste(fYear, "-10-31", sep = "")))

rm(list=ls(pattern = "Presc"))
rm(list=ls(pattern = "DeSo"))
rm(FireHistory)

# summary(shelby_fires)
st_write(shelby_fires, "data/CampShelby/shelby_fires_2002_2019.shp")


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
shelby_plots <- filter(plot_locations, inst=="shelby")

shelby2017 <- filter(shelby_plots, visit_yr==2017)
shelby2018 <- filter(shelby_plots, visit_yr==2018)

shelby_fires$FID <- seq(1, nrow(shelby_fires), 1)


fires2018 <- st_join(st_transform(shelby2018, crs = st_crs(shelby_fires)),
                     shelby_fires %>%
                           filter(fDate > "2003-08-15",
                                  fDate < max(shelby2018$visit_date))
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

fires2017 <- st_join(st_transform(shelby2017, crs = st_crs(shelby_fires)),
                     shelby_fires %>%
                           filter(fDate > "2002-09-25",
                                  fDate < max(shelby2017$visit_date))
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
shelby_fri <- rbind(fri2017, fri2018)
summary(shelby_fri)

## Write shapefile
st_write(shelby_fri, "data/CampShelby/shelby_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(shelby_fri), "data/CampShelby/shelby_15yr_fri.csv")



# congongrass data ####
# shelby_cogon <- readOGR("data/CampShelby/NuisanceSpeciesManagement.shp")
# summary(shelby_cogon)
#
# shelby_cogon_untrt <- shelby_cogon[shelby_cogon$Status=="MAPPED ONLY",]
# summary(shelby_cogon_untrt)
# shelby_cogon_untrt@data <- droplevels(select(
#       shelby_cogon_untrt@data,
#       OBJECTID:dateDesign,featureAre,featurePer,narrative,sdsFeatu_1,
#       treatmentT,user_flag,Date_sampl,pop_year,Shape_STAr,Shape_STLe))
# shelby_cogon_untrt$sdsFeatu_1[shelby_cogon_untrt$sdsFeatu_1=="IMPERATA CYLINDRICA, INVASIVE"] <- "COGONGRASS"
# shelby_cogon_untrt$sdsFeatu_1 <- tolower(droplevels(shelby_cogon_untrt$sdsFeatu_1))
# writeOGR(shelby_cogon_untrt, "data/CampShelby/", "untreated_cogon", driver = "ESRI Shapefile")

