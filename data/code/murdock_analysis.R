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
map_africa <- st_read(dsn = here("data", "raw", "spatial", "africa_shp", 
                                 "afr_g2014_2013_0.shp"))

merged_map <- ggplot(data = map_africa) + 
  geom_sf(data = map_murdock, color = "black", size = 0.2, fill = "orange")+
  geom_sf(data = map_africa, color = "red", size = 0.6, fill = NA) +
  coord_sf(datum = NA) + 
  theme_bw() + 
  theme(panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank())

merged_map

ggsave("merged_map.png", path = here("data", "output"), dpi = 300)




############### 2. Cleaning Ethnographic Atlas ##############


ea_loc <- read_csv(file = here("data", "raw", "murdock_ea", "societies.csv")) %>%
  mutate(type = "ea")

sccs_loc <- read_csv(file = here("data", "raw", "sccs", "societies.csv")) %>%
  mutate(type = "sccs")

ea_sccs <- bind_rows(ea_loc, sccs_loc)

map_world <- st_read(dsn = here("data", "raw", "spatial", "world_shp", 
                                 "World_Countries__Generalized_.shp"))


full_map <- ggplot(data = map_world) +
  geom_sf(fill = NA) +
  geom_point(data = ea_sccs, aes(x = Long, y = Lat, colour = type), size = 1) +
  scale_color_manual(name = "", values = c("blue", "red"), labels = c("Only EA", "EA + SCCS")) +
  coord_sf(datum = NA) +
  theme_bw() + 
  theme(panel.border = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           panel.grid = element_blank(),
           axis.ticks = element_blank(), 
           axis.title.y=element_blank(),
           axis.title.x=element_blank())

full_map
  
ggsave("full_map.png", path = here("data", "output",
                                           "ea_sccs_centroid.png"), dpi = 300)





