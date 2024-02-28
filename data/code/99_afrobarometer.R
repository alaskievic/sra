# Load packages
source("00_load_packages.R")

# Install LEDA
library(devtools)
install_github(repo = "carl-mc/LEDA")

############### 1. Afrobarometer Dataset ##############

# Read .sva file
afro_7 <- read_sav(file = here("data", "raw", "afrobarometer",
                               "r7_merged_data_34ctry.release.sav"))

afro_8 <- read_sav(file = here("data", "raw", "afrobarometer",
             "afrobarometer_release-dataset_merge-34ctry_r8_en_2023-03-01.sav"))

# Select variables
afro_7 %<>% dplyr::select(c("RESPNO", "COUNTRY", "REGION", "Q1", "Q2A", "Q2B",
                            "Q84", "Q94", "Q95A", "Q95B", "Q96A", "Q96B", "Q97",
                            "Q98", "Q101", "Q102", "Q84", "URBRUR_COND"))

afro_8 %<>% dplyr::select(c("RESPNO", "COUNTRY", "REGION", "Q1", "Q2",
                            "Q82B", "Q95C", "Q96B", "Q96C", "Q97", "Q98A",
                            "Q101", "Q102"))

# Link Afrobarometer 7 to Murdock Map
library(LEDA)
leda <- LEDA$new()

list.dict <- leda$get_list_dict()


map_afro_set <- leda$link_set(lists.a = list(type = "Afrobarometer", round = 6),
                         lists.b = list(type = "Murdock_Map"), 
                         link.level = "dialect",  
                         by.country = TRUE, 
                         drop.a.threshold = 0, 
                         drop.b.threshold = 0, 
                         drop.ethno.id = TRUE, 
                         add_listmetadata = TRUE)

map_afro_set %<>% dplyr::select(c("a.type", "a.groupvar", "a.marker", "b.type",
                                   "a.iso3c", "b.iso3c", "a.group", "b.group",
                                   "iso3c"))

# Keep the unmatched
map_unmatched <- filter(map_afro_set, is.na(b.group) == TRUE)

map_unmatched %<>% filter(a.group != "French",
                           a.group != "English",
                           a.group != "Portuguese",
                           a.group != "German",
                           a.group != "Indian",
                           a.group != "German", 
                           a.group != "Portugues",
                           a.group != "White/European")

# Link by language distance
map_afro_dist <- leda$link_minlingdist(
  lists.a = list(type = c("Afrobarometer"),  round = 6), 
  lists.b = list(type = c("Murdock_Map")),
  level = "dialect",
  by.country = TRUE,
  delta = .5, expand = FALSE, 
  agg_fun.a = min, agg_fun.b = min,
  add_listmetadata = T)

join_unmatched <- inner_join(map_unmatched, map_afro_dist,
                             by = c("a.group", "a.iso3c"), 
                             relationship = "many-to-many") %>%
  dplyr::select(c("a.type.x", "a.marker.x", "a.group", "b.group.x", "a.iso3c",
                  "b.type.x", "b.type.y", "b.group.y",  "b.iso3c.x", "b.iso3c.y",
                  "distance"))

join_unmatched %<>% dplyr::select(c("a.type", "a.groupvar", "a.marker", "b.type",
                                   "a.iso3c", "b.iso3c", "a.group", "b.group", 
                                   "distance"))


# Filter by greatest distance form each a.group
teste <- map_afro_dist
teste %<>% filter(distance != 1)
teste %<>% group_by(a.iso3c, b.group) %>%
  mutate(max_distance=min(distance))

  


# Inner join with previously unmatched
join_unmatched <- inner_join(map_unmatched, map_afro_dist, by = "a.group", 
                             relationship = "many-to-many") %>%
  dplyr::select(c("a.type.x", "a.type.y", "a.marker.x", "a.group", "b.group.x",
                  "b.type.x", "b.type.y", "b.group.y", "distance"))


# Use distance to match the unmatched entries in link_set



