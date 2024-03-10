#Load packages
source("00_load_packages.R")

# Install and Load package with Murdock's Map
devtools::install_github("sboysel/murdock")
library(murdock)

install.packages("rnaturalearth")
library(rnaturalearth)


##################### 1. Read TRI and Slope Files ##############################

### Read Terrain Ruggedness Index for the whole planet
tri <- terra::rast(here("data", "raw", "geo_data", "slope", "tri.txt"))

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

# Reprojects raster
terra::crs(tri) <- terra::crs(map_murdock)

# Restrict to map's borders
tri_crop <- terra::crop(tri, map_murdock)

tri_mask <- terra::mask(tri_crop, map_murdock)

# plot(tri)
# plot(tri_crop)
plot(tri_mask)


# Saving
terra::writeRaster(tri_mask, filename = here("data", "raw", "geo_data",
                                             "slope", "tri_murdock_raw.tif"))

### Read Slope for the whole planet
slope <- terra::rast(here("data", "raw", "geo_data", "slope", "slope.txt"))

# Reprojects raster
terra::crs(slope) <- terra::crs(map_murdock)

# Restrict to Map borders
slope_crop <- terra::crop(slope, map_murdock)

slope_mask <- terra::mask(slope_crop, map_murdock)

# plot(slope)
# plot(slope_crop)
plot(slope_mask)

# Saving
terra::writeRaster(slope_mask, filename = here("data", "raw", "geo_data",
                                               "slope", "slope_murdock_raw.tif"))


### Read Cell Area for the whole planet
cellarea <- terra::rast(here("data", "raw", "geo_data", "slope", "cellarea.txt"))

# Reprojects raster
terra::crs(cellarea) <- terra::crs(map_murdock)

# Restrict to Brazilian borders
cellarea_crop <- terra::crop(cellarea, map_murdock)

cellarea_mask <- terra::mask(cellarea_crop, map_murdock)

# Saving
terra::writeRaster(cellarea_mask, filename = here("data", "raw", "geo_data",
                                                  "slope", "cellarea_murdock_raw.tif"))



####### 2. Calculating average TRI and slope #########

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

# Load TRI
tri_murdock <- terra::rast(here("data", "raw", "geo_data", "slope", "tri_murdock_raw.tif"))

# Load Slope
slope_murdock <- terra::rast(here("data", "raw", "geo_data", "slope", "slope_murdock_raw.tif"))

# Load Cell Area
cellarea_murdock <- terra::rast(here("data", "raw", "geo_data", "slope", "cellarea_murdock_raw.tif"))

# Restrict Borders
tri_murdock_mask <- terra::mask(tri_murdock, map_murdock)

cellarea_murdock_mask <- terra::mask(cellarea_murdock, map_murdock)

slope_murdock_mask <- terra::mask(slope_murdock, map_murdock)

# Extracting Values
tri_murdock_values      <- terra::extract(x = tri_murdock_mask, y = map_murdock, df = TRUE)
slope_murdock_values    <- terra::extract(x = slope_murdock_mask, y = map_murdock, df = TRUE)
cellarea_murdock_values <- terra::extract(x = cellarea_murdock_mask, y = map_murdock, df = TRUE)


# Transform millimeters to meters
tri_murdock_values %<>% mutate(tri = tri/1000)

# Transform slope magnitudes
slope_murdock_values %<>% mutate(slope = slope/1000)

# Join and calculate weighted average
tri_slope_murdock <- tri_murdock_values

tri_slope_murdock$cellarea <- cellarea_murdock_values$cellarea
tri_slope_murdock$slope    <- slope_murdock_values$slope


tri_slope_murdock %<>% group_by(ID) %>% mutate(tri_mean = mean(tri, na.rm = TRUE)) %>%
  mutate(tri_weighted = weighted.mean(tri, cellarea, na.rm = TRUE)) %>%
  mutate(slope_mean = mean(slope, na.rm = TRUE)) %>%
  mutate(slope_weighted = weighted.mean(slope, cellarea, na.rm = TRUE)) %>% 
  ungroup()

tri_slope_murdock %<>% distinct(ID, .keep_all = TRUE)

tri_slope_murdock %<>% dplyr::select(ID, tri_weighted, slope_weighted)

tri_slope_murdock %<>% as_tibble(geom = NULL)

# Assign codes
tri_slope_murdock$CODE  <- map_murdock$CODE
tri_slope_murdock$NAME   <- map_murdock$NAME


tri_slope_murdock %<>% mutate(d_tri = ifelse(tri_weighted >=0 & tri_weighted < 80, "Level", 0)) %>%
  mutate(d_tri = ifelse(tri_weighted >=80  & tri_weighted < 116, "Nearly Level", d_tri)) %>%
  mutate(d_tri = ifelse(tri_weighted >=116 & tri_weighted < 161, "Slightly Rugged", d_tri)) %>%
  mutate(d_tri = ifelse(tri_weighted >=161 & tri_weighted < 239, "Intermediately Rugged", d_tri)) %>%
  mutate(d_tri = ifelse(tri_weighted >=239 & tri_weighted < 497, "Moderately Rugged", d_tri)) %>%
  mutate(d_tri = ifelse(tri_weighted >=497 & tri_weighted < 958, "Highly Rugged", d_tri)) %>%
  mutate(d_tri = ifelse(tri_weighted >=958 & tri_weighted <=4367, "Extremely Rugged", d_tri)) %>%
  mutate(d_tri = as.factor(d_tri)) %>%
  arrange(tri_weighted)  
  

# Saving
save(tri_slope_murdock, file = here("data", "output", "tri_slope_peru.RData"))

tri_slope_murdock %<>% rename(map_name = "NAME")

stata_march4 <- read_dta(file = here("data", "output", "dhs", "stata_march4.dta"))

stata_march4 %<>% inner_join(., tri_slope_murdock, by = "map_name")

write_dta(stata_march4, path = here("data", "output", "dhs", "stata_march4.dta"))


################# 3. Calculates distance to the cost ###########################

# Murdock's Map
map_murdock <- murdock
map_murdock <- st_as_sf(map_murdock)

# st_point_on_surface guarantees that the centroid fall inside the mun borders
centroid_murdock <- st_point_on_surface(map_murdock)

# Plotting
plot(map_murdock$geometry)
plot(centroid_murdock, add = TRUE)

# Loading Brazilian coast shapefile
coast<- ne_coastline()

coast <- st_read(here("data", "raw", "geo_data", "gshhg-shp-2.3.7", "GSHHS_shp", 
                      "f", "GSHHS_f_L1.shp"))

# Plotting
plot(coast$geometry)

# Assigning same coordinate
coast <- st_transform(coast, projection(map_murdock))
coast %<>% st_set_crs(st_crs(map_murdock))
centroid_murdock %<>% st_set_crs(st_crs(map_murdock))

# Distance Centroids
coast <- st_cast(coast, "MULTILINESTRING")

dist_centroid <- sf::st_distance(centroid_murdock, st_union(coast))

# Create a data.frame with the distance and the coordinates of the points
dist_centroid <- tibble(dist_centroid = as.vector(dist_centroid)/1000,
                        centroid_murdock$NAME)

colnames(dist_centroid) <- c("dist_coast_centroid", "NAME")


# Saving
save(dist_coast_final, file = here("output", "distance_coast", "dist_coast.RData"))

dist_centroid %<>% rename(map_name = "NAME")

stata_march4 <- read_dta(file = here("data", "output", "dhs", "stata_march4.dta"))

stata_march4 %<>% inner_join(., dist_centroid, by = "map_name")

write_dta(stata_march4, path = here("data", "output", "dhs", "stata_march4.dta"))




###################### 4. Rainfall and Temperature #############################

# Test for one of the files #
rain_test <- terra::rast(here("data", "raw", "geo_data", "wc2.1_30s_prec",
                        "wc2.1_30s_prec_01.tif"))

# Murdock's Map #
map_murdock <- murdock
map_murdock <- terra::vect(map_murdock)

# Reprojects raster #
terra::crs(rain_test) <- terra::crs(map_murdock)

# Restrict to map's borders
rain_test_crop <- terra::crop(rain_test, map_murdock)

rain_test_mask <- terra::mask(rain_test_crop, map_murdock)

# plot(tri)
# plot(tri_crop)
plot(rain_test_mask)

# Extracting Values
rain_test_value <- terra::extract(x = rain_test_mask, y = map_murdock, fun = mean, na.rm = TRUE)

# Assign codes
rain_test_value$CODE   <- map_murdock$CODE
rain_test_value$NAME   <- map_murdock$NAME


# Use map to extract from file list #
# Read all files as a list #
rain_list <- list.files(path = here("data", "raw", "geo_data", "wc2.1_30s_prec"),
                      pattern='.tif$', all.files=TRUE, full.names=TRUE) %>%
  map(., terra::rast)


# teste <- do.call(merge, rain_list)

# Murdock's Map #
map_murdock <- murdock
map_murdock <- terra::vect(map_murdock)

# Reprojects Raster #
rain_list_crop <- purrr::map(rain_list, terra::crop, map_murdock)
rain_list_mask <- purrr::map(rain_list_crop, terra::mask, map_murdock)

# Extracting Values
rain_value_list <- purrr::map(rain_list_mask, terra::extract, map_murdock, fun = mean,
                       na.rm = TRUE)

murdock_rain <- purrr::reduce(rain_value_list, full_join, by = "ID") %>%
  mutate(tot_rain = rowSums(across(starts_with("wc2")))) %>%
  mutate(avg_rain = rowMeans(across(starts_with("wc2")), na.rm = TRUE)) %>%
  dplyr::select(avg_rain, tot_rain)

# Assign Codes and Names
murdock_rain$NAME   <- map_murdock$NAME

murdock_rain %<>% rename(map_name = "NAME")



# Quick Plot #

temp_list <- list.files(path = here("data", "raw", "geo_data", "wc2.1_30s_tavg"),
                        pattern='.tif$', all.files=TRUE, full.names=TRUE) %>%
  map(., terra::rast)


# teste <- do.call(merge, temp_list)

# Murdock's Map #
map_murdock <- murdock
map_murdock <- terra::vect(map_murdock)

# Reprojects Raster #
temp_list_crop <- purrr::map(temp_list, terra::crop, map_murdock)
temp_list_mask <- purrr::map(temp_list_crop, terra::mask, map_murdock)

# Extracting Values
temp_value_list <- purrr::map(temp_list_mask, terra::extract, map_murdock, fun = mean,
                              na.rm = TRUE)

murdock_temp <- purrr::reduce(temp_value_list, full_join, by = "ID") %>%
  mutate(avg_temp = rowMeans(across(starts_with("wc2")), na.rm = TRUE)) %>%
  dplyr::select(avg_temp)

# Assign Codes and Names
murdock_temp$NAME   <- map_murdock$NAME

murdock_temp%<>% rename(map_name = "NAME")


stata_march4 <- read_dta(file = here("data", "output", "dhs", "stata_march4.dta"))

stata_march4 %<>% inner_join(., murdock_rain, by = "map_name") %>%
  inner_join(., murdock_temp, by = "map_name")

write_dta(stata_march4, path = here("data", "output", "dhs", "stata_march4.dta"))

