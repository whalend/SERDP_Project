#' # Script for doing QA/QC on tree data


#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_csv("data/raw_data/trees.csv")
tree_data_sub <- filter(tree_data, date>"2017-06-01", plot_id != "bland03", plot_id != "bland02")

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
