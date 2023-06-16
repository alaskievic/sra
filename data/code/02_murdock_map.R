# Load packages
source("00_load_packages.R")

# Install and Load package with Murdock's Map
devtools::install_github("sboysel/murdock")
library(murdock)

############### 1. Murdock's Map ##############

# Murdock's Map
map_murdock <- murdock

map_murdock <- st_as_sf(map_murdock)

plot(map_murdock$geometry)

# Extract the data
map_data <- murdock@data

map_data %>% dplyr::count(CodeType)


# Load Lowes(2023) crosswalk
ea_map_match <- read_csv(here("data", "raw", "murdock_map",
                              "murdock_ea_join_indexmatch_toshare.csv")) %>%
  dplyr::select(c("NAME", "EA_match", "exact_match", "noclustermatch", "ea_name")) %>%
  mutate(match_name = case_when(exact_match == 0 ~ ea_name,
                                exact_match == 1 ~ EA_match)) %>%
  mutate(match_name = str_to_lower(match_name))%>%
  rename("map_name" = NAME) %>%
  dplyr::select(c("map_name", "ea_name", "match_name", "exact_match",
                  "noclustermatch",))

# Load cleaned D-Place EA
load(here("data", "output", "ea_dplace.Rda"))

# Construct a concordance between EA-Dplace and Murdock Map
ea_dplace %<>% filter(var_id == "EA005") %>%
  dplyr::select(c("pref_name", "orig_name", "alt_name")) %>%
  mutate(match_name = str_to_lower(orig_name)) %>%
  mutate(match_name = str_replace(match_name, " \\s*\\([^\\)]+\\)", ""))


# Match with murdock map
teste <- full_join(ea_dplace, ea_map_match, by = "match_name")

# create measure based on Michalopoulos(2018)





######################## 2 - Some Maps on EA Variables #########################
# Murdock's Map
map_murdock <- murdock

map_murdock <- st_as_sf(map_murdock)

plot(map_murdock$geometry)

# Extract the data
map_data <- murdock@data
map_data %>% dplyr::count(CodeType)

# Load Lowes(2023) crosswalk
lowes_data <- read_csv(here("data", "raw", "murdock_map",
                              "murdock_ea_join_indexmatch_toshare.csv")) %>%
  dplyr::select(c("NAME", "EA_match", "exact_match", "noclustermatch",
                  "ea_name", "v5")) %>%
  rename("map_name" = NAME) %>%
  mutate(map_name = str_to_lower(map_name))

# Match to data to Murdock Map
map_murdock %<>% rename("map_name" = NAME) %>%
  mutate(map_name = str_to_lower(map_name)) %>%
  dplyr::select(c("map_name", "CodeType"))

map_murdock <- full_join(map_murdock, lowes_data, by = "map_name") %>%
  mutate(v5 = as.factor(v5))


# Map of Africa
map_africa <- st_read(dsn = here("data", "raw", "spatial", "africa_shp", 
                                 "afr_g2014_2013_0.shp"))

v5_map <- tm_shape(map_murdock) +
  tm_borders(col = "darkgrey", alpha = 0.5, lwd = 1.5) +
  tm_fill(col = "v5", style = "cat", palette = "RdYlGn") + 
  tm_shape(map_africa) + 
  tm_borders(col = "black", lwd = 2) +
  tm_layout(legend.position = c("left", "bottom"),
            legend.text.size = 0.8,
            frame = FALSE)

v5_map

teste <- st_drop_geometry(map_murdock)
  
tmap_save(murdock_africa, here("data", "output", "figures", 
                               "murdock_africa.png"))

  
# Using the original match from Revised Murdock Map
  

