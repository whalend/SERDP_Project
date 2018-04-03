## Copy raw data from Dropbox to project directory ----

cogon_data <- read.csv("~/Dropbox (UF)/SERDP-Project/data/cogon-data.csv")
write.csv(cogon_data, "data/raw_data/cogon-data.csv", row.names = F)

densiometer <- read.csv("~/Dropbox (UF)/SERDP-Project/data/densiometer-canopy-cover.csv")
write.csv(densiometer, "data/raw_data/densiometer-data.csv", row.names = F)

dung <- read.csv("~/Dropbox (UF)/SERDP-Project/data/dung.csv")
write.csv(dung, "data/raw_data/dung.csv", row.names = F)

plot_info <- read.csv("~/Dropbox (UF)/SERDP-Project/data/plot-info.csv")
write.csv(plot_info, "data/raw_data/plot-info.csv", row.names = F)

quadrat1m <- read.csv("~/Dropbox (UF)/SERDP-Project/data/quadrat1m.csv")
write.csv(quadrat1m, "data/raw_data/quadrat1m.csv", row.names = F)

quadrat25cm <- read.csv("~/Dropbox (UF)/SERDP-Project/data/quadrat25cm.csv")
write.csv(quadrat25cm, "data/raw_data/quadrat25cm.csv", row.names = F)

species1m <- read.csv("~/Dropbox (UF)/SERDP-Project/data/species1m.csv")
write.csv(species1m, "data/raw_data/species1m.csv", row.names = F)

woody_subplot <- read.csv("~/Dropbox (UF)/SERDP-Project/data/subplot-woody-species.csv")
write.csv(woody_subplot, "data/raw_data/subplot-woody-species.csv", row.names = F)

ticks <- read.csv("~/Dropbox (UF)/SERDP-Project/data/ticks.csv")
write.csv(ticks, "data/raw_data/ticks.csv", row.names = F)

trees <- read.csv("~/Dropbox (UF)/SERDP-Project/data/tree-data.csv")
write.csv(trees, "data/raw_data/trees.csv", row.names = F)
