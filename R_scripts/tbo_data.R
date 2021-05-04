## Tick Pathogen Data
## The pathogens identified for each tick. 
## Human, animal, endosymbiont categories in "pathogenicity"
## QAQC and munging to be able to use with other data.

library(tidyverse)
library(readxl)

tbo_data_import <- read_excel("data/TBO_SERDP_Results_202008.xlsx",
                  sheet = "2017-18 info & TBA hits")
names(tbo_data_import)
names(tbo_data_import)[2] <- "Installation"
summary(tbo_data_import)
# tbo_data_import %>% filter(is.na(date)) %>% nrow()
n_distinct(tbo_data_import$`sample ID`)

tbo_data <- tbo_data_import %>%
      select(`sample ID`:`Coxiella endosymbiont`, `number of TBAs per tick`)
names(tbo_data)
n_distinct(tbo_data$`sample ID`)

tbo_data <- tbo_data %>%
      rename(
            sample_ID = `sample ID`, plot_id = `plot ID`,
            collection_method = `collection method`, 
            ng_ul_DNA = `ng/ul DNA`,
            DNA_method = `DNA extract method`,
            tbo1 = `Tick-borne Agent...12`, 
            tbo2 = `Tick-borne Agent...13`,
            tbo3 = `Tick-borne Agent...14`, 
            tbo4 = `Tick-borne Agent...15`,
            tbo5 = `Tick-borne Agent...16`,
            tbo6 = `Rickettsia amblyommatis...17`,
            tbo7 = `Francisella sp....18`, 
            tbo8 = `Coxiella endosymbiont`,
            tbo_per_tick = `number of TBAs per tick`
      ) %>% 
   mutate(plot_id = tolower(plot_id),
          tbo1 = case_when(tbo_per_tick == 0 ~ "none",
                           TRUE ~ tbo1))
n_distinct(tbo_data$sample_ID)
# tbo_data %>% summary()
## deleted row in original file that had counts of records
# tbo_data <- filter(tbo_data, !is.na(date))

# xtabs(~collection_method, data = tbo_data)
# tbo_data %>% group_by(collection_method) %>%
      # summarise(tbos = sum(tbo_per_tick),
                # ticks = length(plot_ID))

tbo_long <- tbo_data %>%
   select(sample_ID:life_stage, -location, tbo1:tbo8) %>%
   pivot_longer(cols = c(tbo1:tbo8),
                   names_to = "tboid", values_to = "tbo",
                   values_drop_na = TRUE
                ) %>%
   # distinct(sample_ID, tbo, .keep_all = TRUE) #%>% 
   select(-tboid)
## The long-format repeats sample_ID that have multiple TBOs

# n_distinct(tbo_long$sample_ID)
# summary(tbo_long)

# tmp <- tbo_long %>% distinct(sample_ID, .keep_all = TRUE)
# unique(tmp$tboid)

sort(unique(tbo_long$tbo))
## Correct tbo names
tbo_long$tbo[tbo_long$tbo=="Ehrlichia muris subsp. euclariensis" |
            tbo_long$tbo=="Ehrlichia muris subsp. euclairensis"] <- "Ehrlichia muris subsp. eauclairensis"
tbo_long$tbo[tbo_long$tbo=="Ricettsia bellii"] <- "Rickettsia bellii"
tbo_long$tbo[tbo_long$tbo=="Theileria sp"] <- "Theileria sp."


pathogenecity <- read_excel("data/TBO_pathogenicity.xlsx")
p <- pathogenecity %>%
   select(-`Human/Endosymbiont`, -`Human/Animal`) %>% 
      rename(
            Pathogen_Name = `PATHOGEN NAME`,
            Domestic_Animals = `Domestic Animals`,
            # Human_Endo = `Human/Endosymbiont`,
            # Human_Animal = `Human/Animal`,
            tick_hosts_literature = `TICK HOST(S)`,
            tick_hosts_data = `TICK HOSTS, DOD SERDP STUDY`,
            vertebrate_tick_hosts = `VERTEBRATE TICK HOST(S)`,
            disease = DISEASE,
            disease_host = `DISEASE HOST`
      ) %>%
      # select(Pathogen_Name:Unknown) %>%
      mutate(Human = if_else(Human == "+", "Yes", "No"),
             Wildlife = ifelse(Wildlife == "+", "Yes", "No"),
             Domestic_Animals = ifelse(Domestic_Animals == "+", "Yes", "No"),
             Endosymbiont = ifelse(Endosymbiont == "+", "Yes", "No"),
             # Human_Endo = ifelse(Human_Endo == "+", "Yes", "No"),
             # Human_Animal = ifelse(Human_Animal == "+", "Yes", "No"),
             Unknown = ifelse(Unknown == "+", "Yes", "No"))
p$Pathogen_Name[p$Pathogen_Name=="Coxiella endosymbiont of Amblyomma americanum"] <- "Coxiella endo. of A. americanum"
p[is.na(p)] <- "No"

write_csv(p, "data/tbo_pathogenicity_reference.csv")


d <- left_join(tbo_long, 
               p %>% select(Pathogen_Name:Unknown), 
               by = c("tbo"="Pathogen_Name"))
# n_distinct(d$sample_ID)
# summary(d)

## Change names to match with other datasets
d <- d %>% 
   mutate(inst_name = case_when(
      Installation == "Avon" ~ "Avon Park AFR",
      Installation == "Blanding" ~ "Camp Blanding",
      Installation == "Eglin" ~ "Eglin AFB",
      Installation == "Gordon" ~ "Fort Gordon",
      Installation == "Jackson" ~ "Fort Jackson",
      Installation == "Moody" ~ "Moody AFB",
      Installation == "Benning" ~ "Fort Benning",
      Installation == "Shelby" ~ "Camp Shelby",
      Installation == "Tyndall" ~ "Tyndall AFB",
   ),
   Installation = tolower(Installation),
   Installation = ifelse(Installation == "avon", "avonpark", Installation),
   plot_id = paste(Installation, plot_id),
   visit_year = lubridate::year(date),
   plot_id = case_when(
      plot_id=="gordon z1" ~ "gordon a1",
      plot_id=="gordon y1" ~ "gordon b1",
      plot_id=="gordon x1" ~ "gordon c1",
      plot_id=="gordon w1" ~ "gordon d1",
      plot_id=="gordon v1" ~ "gordon e1",
      plot_id=="gordon t1" ~ "gordon f1",
      plot_id=="gordon s1" ~ "gordon g1",
      plot_id=="gordon r1" ~ "gordon h1",
      plot_id=="blanding c" ~ "blanding c1",
      TRUE ~ plot_id
   )
   ) %>% 
   select(Installation, plot_id, everything())

# n_distinct(d$sample_ID)

write_csv(d, "data/tbo_pathogenicity_all.csv")


## Example to get a tick abundance summary
d %>% 
   group_by(inst_name, plot_id, visit_year, life_stage) %>%
   summarise(tick_count = n_distinct(sample_ID))

## Compile a abundance and infection prevalence data frame

sum(d$tbo!="none")

ap_human_life_stage_data <- d %>% 
   filter(Human == "Yes") %>% 
   group_by(inst_name, plot_id, visit_year, life_stage) %>% 
   summarise(tick_abundance = n_distinct(sample_ID),
             pathogen_abundance = sum(tbo!="none"),
             pathogen_prevalence = pathogen_abundance/tick_abundance) #%>% 
   # pivot_wider(names_from = life_stage, values_from = c(tick_abundance, pathogen_abundance, pathogen_prevalence))

ap_plot_visit_data <- d %>% 
   group_by(inst_name, plot_id, visit_year) %>% 
   summarise(tick_abundance = n_distinct(sample_ID),
             pathogen_abundance = sum(tbo!="none"),
             pathogen_prevalence = pathogen_abundance/tick_abundance)
   

d2 <- d %>%
      filter(Human == "Yes")
write_csv(d2, "data/tbo_pathogenicity_human_all.csv")
sid <- unique(d2$sample_ID)

human_strict <- d %>%
      filter(Human == "Yes", tbo != "Rickettsia amblyommatis")
write_csv(human_strict, "data/tbo_pathogenicity_human_only.csv")

human_strict %>% 
   group_by(inst_name, plot_id, date, life_stage) %>%
   summarise(human_patho_ticks = n_distinct(sample_ID))

sid2 <- unique(human_strict$sample_ID)


## What to do with Unknown?
d %>% filter(Unknown == "Yes") %>% distinct(sample_ID)

d %>% 
   filter(tbo != "Rickettsia amblyommatis",
          Human == "Yes"| Wildlife == "Yes"| Domestic_Animals == "Yes") %>%
   # view()
   write_csv(., "data/tbo_pathogenecity_human_animal_no_R_ambly.csv")

d %>% 
   filter(Human == "Yes"| Wildlife == "Yes" | Domestic_Animals == "Yes") %>%
   # view
   write_csv(., "data/tbo_pathogenecity_human_animal_yes_R_ambly.csv")


tbo2 <- tbo %>%
      select(sample_ID:life_stage, tbo_per_tick) %>%
      mutate(human_path = ifelse(sample_ID%in%sid, "Yes","No"))

tbo_plot_agg <- tbo2 %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(Installation, site, plot_ID, year, human_path) %>%
      summarise(Pathogen_abundance = sum(tbo_per_tick),
                Tick_abundance = length(plot_ID)) %>%
      mutate(tbo_per_tick = Pathogen_abundance/Tick_abundance)
summary(tbo_plot_agg)
write_csv(tbo_plot_agg, "data/tbo_ticks_plot_year.csv")

tbo_ticks_traps <- tbo2 %>%
      filter(collection_method=="trap") %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(Installation, site, plot_ID, year, human_path) %>%
      summarise(Pathogen_abundance = sum(tbo_per_tick),
                Tick_abundance = length(plot_ID)) %>%
      mutate(tbo_per_tick = Pathogen_abundance/Tick_abundance)
write_csv(tbo_ticks_traps, "data/tbo_ticks_onTraps.csv")


tbo_human_strict <- tbo %>%
      select(sample_ID:life_stage, tbo_per_tick) %>%
      mutate(human_path = ifelse(sample_ID%in%sid2, "Yes","No"))

tbo_human_strict_traps <- tbo_human_strict %>%
      filter(collection_method=="trap") %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(Installation, site, plot_ID, year, human_path) %>%
      summarise(Pathogen_abundance = sum(tbo_per_tick),
                Tick_abundance = length(plot_ID)) %>%
      mutate(tbo_per_tick = Pathogen_abundance/Tick_abundance)
write_csv(tbo_human_strict_traps, "data/tbo_human_strict_onTraps.csv")


p <- p %>%
      pivot_longer(
            cols = c(Human:Unknown),
            names_to = "Pathogen_type",
            values_to = "foo",
            values_drop_na = TRUE) %>%
      select(-foo)

summary(d)

d2 <- d %>% filter(collection_method=="Whalen",
             Human=="Yes"|Human_Animal=="Yes"|Human_Endo=="Yes")
d2 %>% select(tbo)
xtabs(~Installation, data = d2)

d3 <- d %>% filter(tbo=="Ehrlichia muris subsp. eauclairensis")
unique(d3$Installation)