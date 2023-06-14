# Load packages
source("00_load_packages.R")

############### 1. Read GREG Shapefile ##############


greg_map <- st_read(dsn = here("data", "raw", "GREG", "shapefile", 
                                 "GREG.shp"))

greg_data <- st_drop_geometry(greg_map)
  
map_world <- st_read(dsn = here("data", "raw", "spatial", "world_shp", 
                                "World_Countries__Generalized_.shp"))

greg_world <- tm_shape(greg_map) +
  tm_borders(col = "black", alpha = 0.5, lwd = 1.25) +
  tm_shape(greg_map) + 
  tm_borders(col = "red", lwd = 2) + 
  tm_add_legend(type = "line", labels = c("Country Boundaries", "GREG Boundaries"), 
                col = c("black", "red")) +
  tm_layout(legend.position = c("right", "bottom"),
            legend.text.size = 1,
            frame = FALSE)


tmap_save(murdock_africa, here("data", "output", "figures", 
                               "murdock_africa.png"))

