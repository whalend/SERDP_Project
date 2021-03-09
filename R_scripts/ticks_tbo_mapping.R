# Map showing variation in the metrics of tick-borne disease risk
# (tick abundance and infection prevalence) among installations.
# Maybe something that uses a color ramp to depict variation among the bases?

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

## From pathogen_data.R
tbo_plot_agg <- read_csv("data/tbo_ticks_plot_year.csv")
tbo_trap_ticks <- read_csv("data/tbo_ticks_onTraps.csv")
tbo_human_strict_traps <- read_csv("data/tbo_human_strict_onTraps.csv")

## From plot-data-report.Rmd
trap_effort <- read_csv("data/installation_trapping_effort.csv")

sites_sf <- st_read("data/gis_files/selected_installations.shp")

unique(tbo_plot_agg$Installation)
unique(sites_sf$FULLNAME)

## Rename installations to match across data
trap_effort$installation <- str_to_title(trap_effort$installation)
trap_effort <- trap_effort %>%
      rename(Installation = installation) %>%
      mutate(
            Installation = case_when(
                  Installation=="Avonpark" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

tbo_plot_agg <- tbo_plot_agg %>%
      mutate(
            Installation = case_when(
                  Installation=="Avon" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

tbo_trap_ticks <- tbo_trap_ticks %>%
      mutate(
            Installation = case_when(
                  Installation=="Avon" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

tbo_human_strict_traps <- tbo_human_strict_traps %>%
      mutate(
            Installation = case_when(
                  Installation=="Avon" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

mapping_data <- tbo_human_strict_traps
## Use only ticks collected via trapping, i.e. excluded any drags/people

tbo_inst_N <- mapping_data %>%
      filter(human_path=="No") %>%
      group_by(Installation) %>%
      summarise(
            Tick_abundanceN = sum(Tick_abundance),
            Pathogen_abundanceN = sum(Pathogen_abundance),
            )
tbo_inst_Y <- mapping_data %>%
      filter(human_path=="Yes") %>%
      group_by(Installation) %>%
      summarise(
            Tick_abundanceY = sum(Tick_abundance),
            Pathogen_abundanceY = sum(Pathogen_abundance),
      )

tbo_inst <- left_join(tbo_inst_N, tbo_inst_Y)
tbo_inst <- tbo_inst %>%
      mutate(Tick_abundanceY = ifelse(is.na(Tick_abundanceY)==TRUE, 0,
                                      Tick_abundanceY),
             Pathogen_abundanceY = ifelse(is.na(Pathogen_abundanceY)==TRUE, 0,
                                      Pathogen_abundanceY),
             total_ticks = Tick_abundanceN + Tick_abundanceY,
             Human_pathogen_prevalence = Tick_abundanceY/total_ticks)
tbo_inst <- left_join(tbo_inst, trap_effort)
tbo_inst <- tbo_inst %>%
      mutate(ticks_per_trap = total_ticks/trap_effort,
             PxA = Human_pathogen_prevalence*ticks_per_trap
             )



sites_tbo_sf <- left_join(
      sites_sf,
      tbo_inst,
      by = c("FULLNAME"="Installation")
) %>%
      select(FULLNAME, total_ticks, Human_pathogen_prevalence, ticks_per_trap, PxA)


tmap_mode("view")
# qtm(sites_sf, fill = "red")
# tmap_mode("plot")

# bm <- read_osm(st_bbox(sites_sf))
sites_tbo_sf <- sites_tbo_sf %>%
      mutate(Ticks = total_ticks,
             Prevalence = Human_pathogen_prevalence,
             Abundance = ticks_per_trap
             )

tp_map <- tm_shape(sites_tbo_sf) +
      # tm_polygons(text = "FULLNAME") +
      # # tm_dots(col = "Pathogen_prevalence", size = "Pathogen_prevalence",
      #         group = "Pathogens") +
      # tm_shape(sites_tbo_sf) +
      tm_dots(col = "Abundance", size = "Abundance",
              # breaks = c(0, 50, 100, 200, 408),
              group = "Ticks per trap (abundance)") +
      tm_shape(sites_tbo_sf) +
      tm_dots(col = "Prevalence", size = "Prevalence",
              # breaks = c(0, 50, 100, 200, 408),
              group = "Human pathogen prevalence ") +
      tm_shape(sites_tbo_sf) +
      tm_dots(col = "PxA", size = "PxA",
              # breaks = c(0, 50, 100, 200, 408),
              group = "Prevalence x Abundance") +
      tm_text(text = "FULLNAME", just = "right", size = 1.5, xmod = -2, bg.color = "gray40")
tp_map
tmap_save(tp_map, filename = "figures/map_tickAbun_strictlyHuman_pathPrev.html")
# tmap_save(tp_map, filename = "figures/map_tickAbun_pathPrev.png")

#
# library(ggmap)
# bb <- st_bbox(sites_sf)
# names(bb) <- c("left","bottom","right","top")
# bm <- get_map(bb)
# ggmap(bm) +
#       geom_point(data = sites_tbo_sf)

