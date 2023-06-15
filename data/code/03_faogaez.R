# Load packages
source("00_load_packages.R")

# Install and Load package with Murdock's Map
devtools::install_github("sboysel/murdock")
library(murdock)


########### 1. Load Caloric Yield FAO-GAEZ Data from the LR Project ############

################################# 1.1 Low Inputs ###############################

load(here("data", "raw", "faogaez", "caloric_potential.Rdata"))
rm(test2)
rm(test20)

# Convert lon and lat to numeric
test1$lon <- as.numeric(test1$lon)
test1$lat <- as.numeric(test1$lat)

# Convert the dataframe to a spatial dataframe
test1_sf <- st_as_sf(test1, coords = c("lon", "lat"), crs = 4326)
rm(test1)

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

# Make sure that geometries are valid
map_murdock <- st_make_valid(map_murdock)
test1_sf <- st_make_valid(test1_sf)

map_murdock %<>% st_set_crs(st_crs(test1_sf))

# Spatial join
test1_sf <- st_join(test1_sf, map_murdock, join = st_intersects)

# Create a vector of column names to calculate averages for
crop_columns <- c("bean200b_yld", "casv200b_yld", 
                  "wpot200b_yld", "spot200b_yld", "grnd200b_yld", 
                  "chck200b_yld", "cowp200b_yld", 
                  "barl200b_yld", "sbrl200b_yld", "wbrl200b_yld", 
                  "maiz200b_yld", "hmze200b_yld", "lmze200b_yld", 
                  "tmze200b_yld", "mllt200b_yld", "fmlt200b_yld", 
                  "pmlt200b_yld", "ricd200b_yld", "ricw200b_yld", 
                  "sorg200b_yld", "hsrg200b_yld", 
                  "lsrg200b_yld", "tsrg200b_yld",  
                  "whea200b_yld", "swhe200b_yld", "wwhe200b_yld", 
                  "soyb200b_yld", "yams200b_yld", "gyam200b_yld", 
                  "wyam200b_yld", "yyam200b_yld")

test1_sf %<>% filter(!is.na(NAME))

# Calculate the average for each country and exclude the geometry column
averages <- test1_sf %>% st_drop_geometry() %>% group_by(NAME) %>% 
  summarise(across(all_of(crop_columns), mean, na.rm = TRUE), .groups = "drop")

# Write the output to a CSV file
write.csv(averages, here("data", "output",  "averages_low_input_murdock.csv"),
          row.names = FALSE)


############################ 1.2 High Inputs ###################################
rm(test1_sf)
rm(averages)

load(here("data", "raw", "faogaez", "caloric_potential.Rdata"))
rm(test1)
rm(test20)

# Convert lon and lat to numeric
test2$lon <- as.numeric(test2$lon)
test2$lat <- as.numeric(test2$lat)

# Convert the dataframe to a spatial dataframe
test2_sf <- st_as_sf(test2, coords = c("lon", "lat"), crs = 4326)

# Make sure that geometries are valid
map_murdock <- st_make_valid(map_murdock)
test2_sf <- st_make_valid(test2_sf)
map_murdock %<>% st_set_crs(st_crs(test2_sf))

# Spatial join

test2_sf <- st_join(test2_sf, map_murdock, join = st_intersects)


# Create a vector of column names to calculate averages for
crop_columns <- c("bean200b_yld", "casv200b_yld", 
                  "wpot200b_yld", "spot200b_yld", "grnd200b_yld", 
                  "chck200b_yld", "cowp200b_yld", 
                  "barl200b_yld", "sbrl200b_yld", "wbrl200b_yld", 
                  "maiz200b_yld", "hmze200b_yld", "lmze200b_yld", 
                  "tmze200b_yld", "mllt200b_yld", "fmlt200b_yld", 
                  "pmlt200b_yld", "ricd200b_yld", "ricw200b_yld", 
                  "sorg200b_yld", "hsrg200b_yld", 
                  "lsrg200b_yld", "tsrg200b_yld",  
                  "whea200b_yld", "swhe200b_yld", "wwhe200b_yld", 
                  "soyb200b_yld", "yams200b_yld", "gyam200b_yld", 
                  "wyam200b_yld", "yyam200b_yld")

# Calculate the average for each country and exclude the geometry column
averages <- test1_sf %>% st_drop_geometry() %>% group_by(name) %>% 
  summarise(across(all_of(crop_columns), mean, na.rm = TRUE), .groups = "drop")

# Write the output to a CSV file
write.csv(averages, file.path(data_folder, "averages_low_input.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------------------

## Goal2: Calculate the high-input average caloric potential at a country x crop level

# Convert lon and lat to numeric
test2$lon <- as.numeric(test2$lon)
test2$lat <- as.numeric(test2$lat)

# Convert the dataframe to a spatial dataframe
test2_sf <- st_as_sf(test2, coords = c("lon", "lat"), crs = 4326)

# Load the shapefile
shapefile_path <- file.path(data_folder, "Shapefiles", "world_data_administrative_boundaries_country_level",
                            "world_data_administrative_boundaries_country_level.shp")
world <- st_read(shapefile_path)

# Make sure that geometries are valid
world <- st_make_valid(world)
test2_sf <- st_make_valid(test2_sf)


# Spatial join
test2_sf <- st_join(test2_sf, world, join = st_intersects)


# Create a vector of column names to calculate averages for
crop_columns <- c("bean200b_yld", "casv200b_yld", 
                  "wpot200b_yld", "spot200b_yld", "grnd200b_yld", 
                  "chck200b_yld", "cowp200b_yld", 
                  "barl200b_yld", "sbrl200b_yld", "wbrl200b_yld", 
                  "maiz200b_yld", "hmze200b_yld", "lmze200b_yld", 
                  "tmze200b_yld", "mllt200b_yld", "fmlt200b_yld", 
                  "pmlt200b_yld", "ricd200b_yld", "ricw200b_yld", 
                  "sorg200b_yld", "hsrg200b_yld", 
                  "lsrg200b_yld", "tsrg200b_yld",  
                  "whea200b_yld", "swhe200b_yld", "wwhe200b_yld", 
                  "soyb200b_yld", "yams200b_yld", "gyam200b_yld", 
                  "wyam200b_yld", "yyam200b_yld")

test2_sf %<>% filter(!is.na(NAME))

# Calculate the average for each country and exclude the geometry column
averages <- test2_sf %>% st_drop_geometry() %>% group_by(NAME) %>% 
  summarise(across(all_of(crop_columns), mean, na.rm = TRUE), .groups = "drop")


# Write the output to a CSV file
write.csv(averages, here("data", "output",  "averages_high_input_murdock.csv"),
          row.names = FALSE)


