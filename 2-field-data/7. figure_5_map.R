#FIGURE: Map of islands visited on Bijagos survey

#install spatial packages if not already available
install.packages(c("sf", "sp"))
remotes::install_github("r-tmap/tmap")

require(sf)
require(tidyverse)
require(sp)
require(tmap)

GNB <- st_read("2-field-data/data_in/gnb-spatial/vector/bijagos.shp") #shapefiles made by Hugo Soubrier of Bijagos archipelago
villages_gps <- read.csv("2-field-data/data_in/field-villages-gps.csv") #GPS coordinates of villages(sentinel sites)

villages_gps <- villages_gps %>%
  mutate(Village = case_when(Village == "Anecuene" ~ "Anequem", #correct village name spelling
                             TRUE ~ Village))

#identify coordinates
villages.ly <- villages_gps %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

#colours for islands
island_pals <- c('#66c2a5','#fc8d62','#8da0cb',
                 '#e78ac3','#a6d854')
#set order
island_pals2 <- c(island_pals[5], island_pals[3], island_pals[1], 
                  island_pals[4], island_pals[2])

GNB_map <- tmap::tm_shape(GNB)+
  tm_borders(col = "grey")+
  tm_fill(fill = "white")+
  tm_shape(villages.ly)+
  tm_dots(fill = "Village", size = 0.5, 
          fill.scale = tm_scale(values = island_pals2)) +
  tm_text("Island", size = 1, ymod = -1)+
  tm_layout(bg.color = "light blue", 
            legend.position = c("left", "bottom"))+
  tm_compass()


tmap_save(GNB_map, file = "2-field-data/plots/fig_5_bijagos_map.pdf")
