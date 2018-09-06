#' # Script for doing QA/QC on cogongrass data

#' ## Cogongrass Data Sampled from Invasions
#'
#+ cogongrass data ####
cogon_data <- read_csv("data/raw_data/cogon-data.csv")
cogon_biomass <- cogon_data %>%
      # select(site,plot_id,Quad,fresh_biomass) %>%
      filter(site!="NA") %>%
      mutate(biomass=fresh_biomass*16)
cogon_biomass <- left_join(
      cogon_biomass,
      select(plot_data, installation,plot_id,years_since_fire),
      by=c("site"="installation","plot_id"))
ggplot(cogon_biomass, aes(years_since_fire, biomass)) +
      geom_point() +
      theme_bw() +
      xlab("Years since fire") +
      ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
      ggtitle("Cogongrass invasion subsample")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/cogon-biomass.png", height = 7)
