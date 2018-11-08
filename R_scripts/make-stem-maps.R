# Making STEM MAPS for tree surveys
#
# Adapted from code provided by Margaret Metz

library(plyr); library(dplyr); library(readr); library(ggplot2)
library(plotrix)
library(stringr)

tree_data <- read_csv("data/processed_data/trees.csv")# load tree data
plotids <- sort(unique(tree_data$plot_id))# vector of unique plot ids

plot_data <- read_csv("data/processed_data/plot_locations.csv")
plot_data <- plot_data %>%
      mutate(plot_id = str_to_title(plot_id)) %>%
      select(`Plot ID`= plot_id, Longitude = lon, Latitude = lat, installation_full_name)

stem_map_data <- tree_data %>%
      filter(!duplicated(stem_id)) %>%
      select(stem_id, plot_id, tag, dbh, health, distance, azimuth, scientific_name, Genus) %>%
      mutate(
            Genus = if_else(Genus %in% c("Acer","Carya", "Pinus", "Quercus"), Genus, "other"),
             symbol.pch = case_when(
                   Genus == "Acer" ~ 17,
                   Genus == "Carya" ~ 18,
                   Genus == "Pinus" ~ 19,
                   Genus == "Quercus" ~ 15,
                   Genus == "other" ~ 3,
                   ),
             symbol.colors = case_when(
                   Genus == "Acer" ~ "darkorange",
                   Genus == "Carya" ~ "brown",
                   Genus == "Pinus" ~ "red",
                   Genus == "Quercus" ~ "blue",
                   Genus == "other" ~ "black",
                   )
             )


# unique(stem_map_data$Genus)

# Function to iterate over the vector of plot ids and make a map
for(i in plotids){
      tmp = filter(tree_data, plot_id==i)
}

tmp <- filter(stem_map_data, plot_id=="blanding n1")

# tmp.genus = tmp$Genus

# unique(tree_data$species)
# unique(tmp$Genus)

# tree_genus <- sort(unique(tree_data$Genus))

## Make species list and matching to different symbols
# symbol.sp = c("Acer","Carya", "Pinus", "Quercus", "other")
# symbol.pch = c(22, 24, 21, 23, 3)[match(tmp$Genus, symbol.sp)]
# symbol.pch[is.na(symbol.pch)]=3
# symbol.colors=c("darkorange", "brown", "red", "blue", "black")[match(tmp$Genus, symbol.sp)]

expansion=.4*cut(tmp$dbh, c(0,10,20,40,80, max(tree_data$dbh, na.rm=T)), labels=F)

dead = tmp$health=="dead" & !is.na(tmp$health)
dead[dead==TRUE] = "white"
dead[dead==FALSE] = tmp$symbol.colors[dead==FALSE]

## Set page/map size
# quartz(width = 8.5, height = 11, pointsize = 10)
# png(width = 8.5, height = 11, units = "in", pointsize = 10, res = 150)
# par(mai=c(0.5, 0.5, 0.5, 0.5))

polar.plot(tmp$distance, tmp$azimuth,
           start=90, clockwise=T, rp.type="n",
           labels=paste(c("N","NE","E","SE","S","SW","W","NW"), seq(0,315,45), sep="\n"),
           label.pos=c(0, 45, 90, 135, 180, 225, 270, 315),
           radial.lim=c(0,12.6), mar=c(5, 5, 15, 5),
           label.prop=1.05, font=2, family="serif"
           )

xpos <- cos(pi*((-1)*tmp$azimuth+90)/180) * tmp$distance
ypos <- sin(pi*((-1)*tmp$azimuth+90)/180) * tmp$distance

points(xpos,ypos, pch=tmp$symbol.pch, cex=expansion, col=tmp$symbol.colors, bg=dead)

offset=c(-.75,-1)
text(xpos,ypos, tmp$tag, cex=.5, pos=4, offset=offset)

legend("bottomleft",
       legend=unique(tmp$Genus),
       col=tmp$symbol.colors,
       pch=tmp$symbol.pch,
       # pt.bg=c("darkorange", "brown", "red", "blue", "black"),
       cex=0.8,
       inset=c(-0.035,.1))

legend("bottomright",
       legend=c("alive", "dead", NA,"0-10cm", "10-20cm", "20-40cm", "40-80cm", ">100cm"),
       col="black",
       pch=c(21,21,NA,rep(21,5)),
       pt.bg=c("black", "white", NA, "black", "black", "black", "black", "black"),
       pt.cex=c(1,1,NA, c(.4*(1:5))), cex=.85, inset=c(-0.035,.1))

