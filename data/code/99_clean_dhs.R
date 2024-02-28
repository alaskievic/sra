# Load packages
source("00_load_packages.R")

# Install and Load package with Murdock's Map
devtools::install_github("sboysel/murdock")
library(murdock)

########### 1. Read and Merge Men Sample with Cluster Identifiers ##############

men_dhs <- read_dta(file = here("data", "raw", "dhs", "men_sample_2.dta"))

load(here("data", "output", "dhs", "cluster_crosswalk.RData"))

shp_append %<>% rename("dhsid" = "DHSID")

men_cluster <- left_join(men_dhs, shp_append, by = "dhsid")
# merge(x, y, all.x = TRUE, by = "Id")


### Murdock's Map ###
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

### Assign coordinates ###
men_cluster %<>% filter(is.na(LONGNUM) == FALSE)
  
men_cluster %<>% st_as_sf(., coords = c("LONGNUM", "LATNUM"))

# Assign same projection
st_crs(men_cluster) <- st_crs(map_murdock) 

## Transform into terra ##
men_cluster <- terra::vect(men_cluster)
map_murdock <- terra::vect(map_murdock)

# Intersect both datasets #
men_intersect <- terra::intersect(men_cluster, map_murdock)

men_intersect <- as.data.frame(men_intersect)

### Merge with Green Revolution Data ###
caloric_murdock <- read_csv(file = here("data", "output",  "caloric_murdock.csv")) %>%
  rename("map_name" = "NAME")

men_dhs_murdock <- left_join(men_intersect, caloric_murdock, by = "map_name")

save(men_dhs_murdock, file = here("data", "output", "dhs", "men_dhs_murdock.RData"))



########### 2. Read and Merge Women Sample with Cluster Identifiers ##############
women_dhs <- read_dta(file = here("data", "raw", "dhs", "women_sample_1.dta")) %>%
  dplyr::select(sample, idhspid, idhshid, dhsid)

women_cluster <- left_join(women_dhs, shp_append, by = "dhsid")

### Assign coordinates ###
women_cluster %<>% filter(is.na(LONGNUM) == FALSE)

women_cluster %<>% st_as_sf(., coords = c("LONGNUM", "LATNUM"))

# Assign same projection
st_crs(women_cluster) <- st_crs(map_murdock) 

## Transform into terra ##
women_cluster <- terra::vect(women_cluster)
map_murdock <- terra::vect(map_murdock)

# Intersect both datasets #
women_intersect <- terra::intersect(women_cluster, map_murdock)

women_intersect <- as.data.frame(women_intersect)

### Merge with Green Revolution Data ###
caloric_murdock <- read_csv(file = here("data", "output",  "caloric_murdock.csv")) %>%
  rename("map_name" = "NAME")

women_dhs_murdock <- left_join(women_intersect, caloric_murdock, by = "map_name")

save(women_dhs_murdock, file = here("data", "output", "dhs", "women_dhs_murdock.RData"))


