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

murdock_africa <- tm_shape(map_murdock) +
  tm_borders(col = "black", alpha = 0.5, lwd = 1.25) +
  tm_shape(map_africa) + 
  tm_borders(col = "red", lwd = 2) + 
  tm_add_legend(type = "line", labels = c("Country Boundaries", "EA Boundaries"), 
                col = c("red", "black")) +
  tm_layout(legend.position = c("right", "bottom"),
            legend.text.size = 1,
            frame = FALSE)

tmap_save(murdock_africa, here("data", "output", "figures", 
                               "murdock_africa.png"))


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


ea_loc <- st_as_sf(ea_loc, coords = c("Long", "Lat"))
sccs_loc <- st_as_sf(sccs_loc, coords = c("Long", "Lat"))


ea_sccs_dotmap <- tm_shape(map_world) +
  tm_borders() +
  tm_shape(ea_loc) +
  tm_dots(col = "blue") + 
  tm_shape(sccs_loc) + 
  tm_dots(col = "red") +
  tm_add_legend(type = "symbol", labels = c("Only EA", "EA + SCCS"), 
                col = c("blue", "red")) +
  tm_layout(legend.position = c("right", "bottom"),
            legend.text.size = 1,
            frame = FALSE)

ea_sccs_dotmap

tmap_save(ea_sccs_dotmap, here("data", "output", "figures", 
                               "ea_sccs_dotmap.png"))

  
ggsave("full_map.png", path = here("data", "output",
                                           "ea_sccs_centroid.png"), dpi = 300)





