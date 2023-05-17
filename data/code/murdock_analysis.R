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






ea_data_nolang <- read_csv(file = here("data", "raw", "murdock_ea", "data.csv"))

ea_lang <- read_csv(file = here("data", "raw", "murdock_ea", "societies.csv")) %>%
  rename(soc_id = id)


# Merging both
ea_data <- full_join(ea_data_nolang, ea_lang, by = "soc_id")


# Saving


############### 3. Checking Ethnologue ##############

ethno <- st_read(dsn = here("data", "raw", "nunn_ancestral",
                            "Ethnologue_16_shapefile", 
                            "langa_no_overlap_biggest_clean.shp"))


plot(ethno$geometry)

ethno_data <- ethno %>% st_drop_geometry()

tm_shape(ethno) + 
  tm_borders() + 
  tmap_options(check.and.fix = TRUE)



############### 4. Glottolog ##############

glotto_geo <- read_csv(file = here("data", "raw", "glottolog", "languages_and_dialects_geo.csv"))

lang_geo <- glotto_geo %>% filter(level == "language") %>%
  filter(latitude != is.na(latitude))

map_world <- st_read(dsn = here("data", "raw", "spatial", "world_shp", 
                                "World_Countries__Generalized_.shp"))

lang_geo <- st_as_sf(lang_geo, coords = c("longitude", "latitude"))



glotto_lang_dotmap <- tm_shape(map_world) +
  tm_borders() +
  tm_shape(lang_geo) +
  tm_dots(col = "blue") + 
  tm_layout(legend.position = c("right", "bottom"),
            legend.text.size = 1,
            frame = FALSE)

glotto_lang_dotmap

tmap_save(ea_sccs_dotmap, here("data", "output", "figures", 
                               "ea_sccs_dotmap.png"))


glotto_languoid <- read_csv(file = here("data", "raw", "glottolog", "languoid.csv"))
  
  
  

