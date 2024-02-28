# Load packages
source("00_load_packages.R")

# Install and Load package with Murdock's Map
devtools::install_github("sboysel/murdock")
library(murdock)

############### 1. Murdock's Map ##############

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

# plot(map_murdock$geometry)

# Extract the data
map_data <- murdock@data
map_data %>% dplyr::count(CodeType)

# Load Lowes(2023) crosswalk
ea_map_match <- read_csv(here("data", "raw", "murdock_map",
                              "murdock_ea_join_indexmatch_toshare_revised.csv")) %>%
  dplyr::select(c("NAME", "EA_match", "exact_match", "noclustermatch", "ea_name", 
                  "index_id", "LAT", "LON")) %>%
  mutate(match_name = case_when(exact_match == 0 ~ ea_name,
                                exact_match == 1 ~ EA_match)) %>%
  mutate(match_name = str_to_lower(match_name))%>%
  mutate(match_name = str_replace(match_name, " \\s*\\([^\\)]+\\)", "")) %>%
  rename("map_name" = NAME) %>%
  dplyr::select(c("map_name", "ea_name", "match_name", "exact_match", "index_id", 
                  "LAT", "LON"))

# Load cleaned D-Place EA
load(here("data", "output", "ea_dplace.Rda"))

# Construct a concordance between EA-Dplace and Murdock Map
ea_dplace %<>% filter(var_id == "EA005") %>%
  dplyr::select(c("pref_name", "orig_name", "soc_id")) %>%
  mutate(match_name = str_to_lower(orig_name)) %>%
  mutate(match_name = str_replace(match_name, " \\s*\\([^\\)]+\\)", ""))


# Match with murdock map
crosswalk_ea_map <- full_join(ea_map_match, ea_dplace, by = "match_name") %>%
  filter(is.na(index_id) == FALSE)
save(crosswalk_ea_map, file = here("data", "output", "crosswalk_ea_map.Rda"))


######################## 2 - Some Maps on EA Variables #########################
rm(list=ls())

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

# Extract the data
map_data <- murdock@data

# Load revised Lowes(2023) crosswalk
load(here("data", "output", "crosswalk_ea_map.Rda"))

# Match to data to Murdock Map
map_murdock %<>% rename("map_name" = NAME) %>%
  dplyr::select(c("map_name", "CodeType"))

map_murdock <- inner_join(map_murdock, crosswalk_ea_map, by = "map_name")
  
## Join with D-Place data
# Load cleaned D-Place EA
load(here("data", "output", "ea_dplace.Rda"))

ea_dplace %<>% filter(var_id == "EA005") %>%
  mutate(code = as.factor(code))

map_murdock <- left_join(map_murdock, ea_dplace, by = "soc_id")



# Map of Africa
map_africa <- st_read(dsn = here("data", "raw", "spatial", "africa_shp", 
                                 "afr_g2014_2013_0.shp"))

v5_map <- tm_shape(map_murdock) +
  tm_borders(col = "darkgrey", alpha = 0.5, lwd = 1.5) +
  tm_fill(col = "code", style = "cat", palette = "RdYlGn", title = "Dependence on Agriculture") + 
  tm_shape(map_africa) + 
  tm_borders(col = "black", lwd = 2) +
  tm_layout(legend.position = c("left", "bottom"),
            legend.text.size = 0.8,
            frame = FALSE)

v5_map

tmap_save(v5_map, here("data", "output", "figures", 
                               "murdock_map_v05.png"))

  
# Using the original match from Revised Murdock Map
  

