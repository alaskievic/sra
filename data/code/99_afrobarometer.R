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


map_afro_link <- leda$link_set(lists.a = list(type = "Afrobarometer", round = 6),
                         lists.b = list(type = "Murdock_Map"), 
                         link.level = "dialect",  
                         by.country = TRUE, 
                         drop.a.threshold = 0, 
                         drop.b.threshold = 0, 
                         drop.ethno.id = TRUE, 
                         add_listmetadata = TRUE)

map_afro_link %<>% dplyr::select(c("a.type", "a.groupvar", "a.marker", "b.type",
                                   "a.iso3c", "b.iso3c", "a.group", "b.group",
                                   "iso3c"))
map_afro_link %>%
  dplyr::select(b.type) %>%
  summarise_all(funs(sum(is.na(.))))  
  
teste <- leda$link_withinlingdist(
  lists.a = list(type = c("Afrobarometer"),  round = 6), 
  lists.b = list(type = c("Murdock_Map")),
  level = "dialect",
  max.distance = .1, by.country = TRUE,
  delta = .5, expand = FALSE, 
  agg_fun.a = mean, agg_fun.b = min)


teste %>%
  dplyr::select(b.type) %>%
  summarise_all(funs(sum(is.na(.))))


# Use distance to match the unmatched entries in link_set



