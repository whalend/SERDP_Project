#' # Script for doing QA/QC on tree data


#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_csv("data/raw_data/trees.csv")
### tree_data_sub <- filter(tree_data, date>"2017-06-01", plot_id != "bland03", plot_id != "bland02")

summary(tree_data)

filter(tree_data, is.na(plot_id)) %>% 
  select(date, installation, plot_id, record_id, stem_id, dbh, tag, species)

tree_data = filter(tree_data, !is.na(plot_id))

filter(tree_data, is.na(dbh)) %>% 
  select(date, installation, plot_id, record_id, stem_id, dbh, tag, species, health)

### Two tree DBH are to be left NA, trees burned up/downed ###

filter(tree_data, is.na(azimuth), plot_id!="cogon plot") %>% 
  select(date, installation, plot_id, distance, azimuth, species, tag, dbh)

filter(tree_data, is.na(azimuth), plot_id=="cogon plot") %>% 
  select(date, installation, plot_id, distance, azimuth, species, tag, dbh)

tree_data$plot_id[tree_data$plot_id=="cogon plot"] <- "theater_cogon"

tree_data <- tree_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>% 
  filter(date>20170601)

unique(plot_visit_data$plot_id) %in% unique(tree_data$plot_id)

n_distinct(tree_data$plot_id)
n_distinct(plot_visit_data$plot_id)

summary(tree_data)

unique(tree_data$)

### pause data processing steven ###
write_csv(tree_data, "data/processed_data/trees.csv")
       
##

str(tree_data)
n_distinct(tree_data$tag)
# tail(tree_data)
# summary(as.numeric(tree_data$tag))

tree_data_sub <- left_join(
      tree_data_sub,
      select(plot_data, installation:full_names),
      by = c("installation","plot_id"))

ggplot(tree_data_sub %>%
             group_by(installation, species, last_fire_year, cogongrass) %>%
             summarise(total_dbh = sum(dbh)),
       aes(last_fire_year, total_dbh)
) +
      geom_bar(aes(fill = species), stat = "identity", position = "dodge") +
      facet_grid(.~installation) +
      theme_bw()

ggplot(tree_data_sub %>%
             group_by(installation, species, years_since_fire, cogongrass),
       aes(years_since_fire, dbh, color = species)) +
      geom_point(position = "jitter") +
      facet_grid(installation~cogongrass) +
      theme_bw() +
      xlab("Years since fire") +
      ylab("Diameter at breast height")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/tree-dbh-jitter.png", height=7)

ggplot(tree_data_sub %>%
             group_by(installation, species, years_since_fire, cogongrass),
       aes(years_since_fire, height, color = species)) +
      geom_point(position = "jitter") +
      facet_grid(installation~cogongrass) +
      theme_bw() +
      xlab("Years since fire") +
      ylab("Tree height")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/tree-height-jitter.png", height=7)
