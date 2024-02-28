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
averages <- test2_sf %>% st_drop_geometry() %>% group_by(NAME) %>% 
  summarise(across(all_of(crop_columns), mean, na.rm = TRUE), .groups = "drop")

# Write the output to a CSV file
write.csv(averages, here("data", "output",  "averages_high_input_murdock.csv"),
          row.names = FALSE)

########### 2. Calculate difference between high and low caloric yield #########
rm(list = ls())

low_murdock <- read_csv(file = here("data", "output",
                                    "averages_low_input_murdock.csv"))

high_murdock <- read_csv(file = here("data", "output",
                                     "averages_high_input_murdock.csv"))

low_murdock <- pivot_longer(cols = bean200b_yld:yyam200b_yld, low_murdock,
                      names_to = "crop", values_to = "caloric_low") %>%
  group_by(NAME) %>%
  summarise(max_caloric_low = max(caloric_low, na.rm=TRUE)) %>%
  ungroup()

high_murdock <- pivot_longer(cols = bean200b_yld:yyam200b_yld, high_murdock,
                            names_to = "crop", values_to = "caloric_high") %>%
  group_by(NAME) %>%
  summarise(max_caloric_high = max(caloric_high, na.rm=TRUE)) %>%
  ungroup()

caloric_murdock <- inner_join(low_murdock, high_murdock, by = c("NAME"))

caloric_murdock %<>% mutate(caloric_diff = max_caloric_high - max_caloric_low)

write.csv(caloric_murdock, here("data", "output",  "caloric_murdock.csv"),
          row.names = FALSE)

###################### 3. Plot Change in Max Caloric Yield #####################
rm(list = ls())

# Load Caloric Differentials and join with shapefile
caloric_murdock <- read_csv(file = here("data", "output",  "caloric_murdock.csv"))

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

map_caloric <- full_join(map_murdock, caloric_murdock, by = "NAME") %>%
  mutate(caloric_diff = caloric_diff/1000000)


# Map of Africa
map_africa <- st_read(dsn = here("data", "raw", "spatial", "africa_shp", 
                                 "afr_g2014_2013_0.shp"))

# Map caloric differential
caloric_diff_map <- tm_shape(map_caloric) +
  tm_borders(col = "darkgrey", alpha = 0.5, lwd = 1.5) +
  tm_fill(col = "caloric_diff", palette = "RdYlGn", midpoint = NA) + 
  tm_shape(map_africa) + 
  tm_borders(col = "black", lwd = 2) +
  tm_layout(legend.position = c("left", "bottom"),
            legend.text.size = 0.8,
            frame = FALSE)

caloric_diff_map

tmap_save(caloric_diff_map, here("data", "output", "figures", 
                       "caloric_diff_map.png"))
