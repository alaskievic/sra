# Load packages
source("00_load_packages.R")

# Install and Load package with Murdock's Map
devtools::install_github("sboysel/murdock")
library(murdock)

############### 1. Murdock's Map ##############

# Murdock's Map
map_murdock <- murdock

map_murdock <- st_as_sf(map_murdock)

# Extract the data
map_data <- murdock@data

# Map of Africa
map_africa <- st_read(dsn = "/Users/alaskievic/Desktop/PhD/SRA/analysis/data/spatial/africa_shp/afr_g2014_2013_0.shp")

merged_map <- ggplot(data = map_africa) + 
  geom_sf(data = map_africa, color = "blue", size = 0.6, fill = NA) +
  geom_sf(data = map_murdock, color = "black", size = 0.2, fill = NA)+
  coord_sf(datum = NA) + 
  theme_bw() + 
  theme(panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank())

merged_map

ggsave("merged_map.png", path = "/Users/alaskievic/Desktop/PhD/SRA/analysis/data/", dpi = 300)


ggplot(data = map_africa) + 
  geom_sf(data = map_murdock, color = "black", size = 0.2, fill = NA)


############### 2. Murdock's Atlas ##############

