# Load packages
source("00_load_packages.R")

############### 1. Append all shapefiles of DHS ##############

file_list <- list.files(here("data", "raw", "dhs", "cluster_shp", "all_files"), pattern = "shp$", full.names = TRUE)

shapefile_list <- lapply(file_list, read_sf)

shapefile_list <- lapply(shapefile_list, st_drop_geometry)

shapefile_list <- lapply(shapefile_list, as_tibble)

shp_append <- rbindlist(shapefile_list, fill = TRUE)

shp_append %<>% .[, list(DHSID, DHSCC, DHSYEAR, DHSCLUST, SOURCE, URBAN_RURA, LATNUM, 
                  LONGNUM, ALT_DEM)]

save(shp_append, file = here("data", "output", "dhs", "cluster_crosswalk.RData"))

