# Merge Caloric Potential with Evenson dataset
# ==============================================================================

# To dos:
# -----------------------------------------------------------------------------

# - 1. Collapse pixel level data (by crop) to country level data.
#   - Merge the current pixel level data with information on country. (Use the shapefiles)
#   - Collapse at the country level (average)

# - 2. Select which is the relevant input level by date
#   - Table 1 in Two Blades of Grass...

# - 3. Merge with Evenson.

# Setup
# -----------------------------------------------------------------------------

# Libraries
library(maptools)
library(lwgeom)
library(raster)
library(sf)
library(readr)
library(rgdal)
library(jtools) # Load jtools
library(googlesheets4)
library(dplyr)
library(plm)
library(tidyr)
library(tidyfst)
library(rasterVis)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(stargazer)
library(RColorBrewer)
library(sandwich)  # Adjust standard errors
library(lmtest)    # Recalculate model errors with sandwich functions with coeftest()
library(broom) 
library(ggforce) # draw circles
library(here) #Use the here() function to create a relative path to the directory containing your data
library(terra)
library(glmnet)
library(magrittr) # import another library
library(readxl)
library(lfe)

# Define a name for the data folder:
if  (Sys.getenv("USERNAME")=="dazav") {
  data_folder <- here('C:/Users/dazav/Dropbox (Personal)/Research Projects/Green Revolution/Data')
}

if (Sys.getenv("USER")=="liruoran"){
  data_folder <- "/Users/liruoran/Dropbox (University of Michigan)/Green Revolution/Data"
}

# ------------------------------------------------------------------------------------------

# Load the caloric potential data
load(file.path(data_folder, "Processed","caloric_potential.Rdata"))

# ------------------------------------------------------------------------------------------

## Goal1: Calculate the low-input average caloric potential at a country x crop level

# Convert lon and lat to numeric
test1$lon <- as.numeric(test1$lon)
test1$lat <- as.numeric(test1$lat)

# Convert the dataframe to a spatial dataframe
test1_sf <- st_as_sf(test1, coords = c("lon", "lat"), crs = 4326)

# Load the shapefile
shapefile_path <- file.path(data_folder, "Shapefiles", "world_data_administrative_boundaries_country_level", 
                            "world_data_administrative_boundaries_country_level.shp")
world <- st_read(shapefile_path)

# Make sure that geometries are valid
world <- st_make_valid(world)
test1_sf <- st_make_valid(test1_sf)


# Spatial join
test1_sf <- st_join(test1_sf, world, join = st_intersects)

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

# Calculate the average for each country and exclude the geometry column
averages <- test2_sf %>% st_drop_geometry() %>% group_by(name) %>% 
  summarise(across(all_of(crop_columns), mean, na.rm = TRUE), .groups = "drop")

# Write the output to a CSV file
write.csv(averages, file.path(data_folder, "averages_high_input.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------------------
# Crops and their first released date

# Rice: ricd200b_yld and ricw200b_yld, 1966
# Wheat: whea200b_yld, swhe200b_yld and wwhe200b_yld, 1965
# Maize: maiz200b_yld and tmze200b_yld, 1966
# Barley: barl200b_yld and sbrl200b_yld, 1979
# Pearl Millet: mllt200b_yld and pmlt200b_yld, 1982
# Sorghum: sorg200b_yld, hsrg200b_yld, lsrg200b_yld and tsrg200b_yld, 1983
# Cassava: casv200b_yld,1984
# Potato: spot200b_yld and wpot200b_yld, 1990
# Groundnut: grnd200b_yld, 1985
# Beans: bean200b_yld, 1979
# Lentils: gram200b_yld, 1980


# Importing the data

low_input <- read.csv(file.path(data_folder,"averages_low_input.csv"))
high_input <- read.csv(file.path(data_folder,"averages_high_input.csv"))


# Initialize modified dataframes
low_input_modified <- data.frame(matrix(ncol = ncol(low_input), nrow = 0))
names(low_input_modified) <- names(low_input)

high_input_modified <- data.frame(matrix(ncol = ncol(high_input), nrow = 0))
names(high_input_modified) <- names(high_input)

# years vector
years <- seq(1960, 2000, 5)

# Get unique countries from low_input or high_input
unique_countries <- unique(low_input$name)

# Loop over unique countries
for (country in unique_countries) {
  # Subset the data for this country
  low_input_country <- subset(low_input, name == country)
  high_input_country <- subset(high_input, name == country)
  
  # Repeat the rows for each year
  low_input_country <- low_input_country[rep(seq_len(nrow(low_input_country)), each = length(years)),]
  high_input_country <- high_input_country[rep(seq_len(nrow(high_input_country)), each = length(years)),]
  
  # Assign the years to this subset
  low_input_country$year <- rep(years, times = nrow(low_input_country) / length(years))
  high_input_country$year <- rep(years, times = nrow(high_input_country) / length(years))
  
  # Add this country's data to the overall data frame
  low_input_modified <- rbind(low_input_modified, low_input_country)
  high_input_modified <- rbind(high_input_modified, high_input_country)
}

# Create a vector of the countries that we want to keep
# country_list <- c("Algeria", "Angola", "Argentina", 
#           "Bangladesh", "Benin", "Bolivia", "Botswana", "Brazil", "Burkina Faso", "Burundi", 
#           "Cambodia", "Cameroon", "Central African Republic", "Chad", "Chile", "China", "Colombia", 
#                 "Democratic Republic of the Congo", "Republic of the Congo", 
#                 "Costa Rica", "Côte d'Ivoire", "Cuba", "Dominican Republic", 
#                 "Ecuador", "Egypt", "El Salvador", "Ethiopia", "```Ethiopia PDR```", 
#                 "Gabon", "Gambia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
#                 "Haiti", "Honduras", 
#                 "India", "Indonesia", "Iran", "Iraq", 
#                 "Jamaica", "Jordan", "Kenya", "Laos", "Lebanon", "Liberia", "Libya", 
#                 "Madagascar", "Malawi", "Malaysia", "Mali", "Mauritania", "Mauritius", 
#                 "Mexico", "Mongolia", "Morocco", "Mozambique", "Myanmar", "```Northern Mariana Islands```", 
#                 "Namibia", "Nepal", "New Caledonia", "Nicaragua", "Niger", "Nigeria", 
#                 "Pakistan", "Panama", "Paraguay", "Peru", "Philippines", "Rwanda", 
#                 "Saudi Arabia", "Senegal", "Sierra Leone", "South Africa", 
#                 "Sri Lanka", "Sudan", "```Eswatini```", "Syria", "Tanzania", "Thailand", 
#                 "Togo", "Tunisia", "Turkey", "Uganda", "Uruguay", "Venezuela", "Vietnam", 
#                 "Yemen", "Zambia", "Zimbabwe") # using the Evenson name


#  country list
country_list <- c( "Algeria", "Angola", "Argentina", 
                   "Bangladesh", "Benin", "Bolivia", "Botswana", "Brazil", "Burkina Faso", "Burundi", 
                   "Cambodia", "Cameroon", "Central African Republic", "Chad", "Chile", "China", "Colombia", 
                   "Democratic Republic of the Congo", "Republic of the Congo", 
                   "Costa Rica", "Côte d'Ivoire", "Cuba", "Dominican Republic",
                   "Ecuador", "Egypt", "El Salvador", "Ethiopia", 
                   "Gabon", "Gambia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", 
                   "Haiti", "Honduras", 
                   "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq",
                   "Jamaica", "Jordan", "Kenya", "Lao People's Democratic Republic", "Lebanon", "Liberia", "Libyan Arab Jamahiriya",
                   "Madagascar", "Malawi", "Malaysia", "Mali", "Mauritania", "Mauritius", 
                   "Mexico", "Mongolia", "Morocco", "Mozambique", "Myanmar", 
                   "Namibia", "Nepal", "New Caledonia", "Nicaragua", "Niger", "Nigeria", 
                   "Pakistan", "Panama", "Paraguay", "Peru", "Philippines", "Rwanda", 
                   "Saudi Arabia", "Swaziland","Senegal", "Sierra Leone", "South Africa", "Sri Lanka",
                   "Sudan", "Syrian Arab Republic", "Tanzania", "Thailand", 
                   "Togo", "Tunisia", "Turkey", "Uganda", "Uruguay", "Venezuela (Bolivarian Republic of)",
                   "Viet Nam", "Yemen", "Zambia", "Zimbabwe")

# filter modified dataframes
low_input_modified <- low_input_modified[low_input_modified$name %in% country_list,]
high_input_modified <- high_input_modified[high_input_modified$name %in% country_list,]

# Write low_input data frame to a csv file
write.csv(low_input_modified, file.path(data_folder, "low_input_modified.csv"), row.names = FALSE)

# Write high_input data frame to a csv file
write.csv(high_input_modified, file.path(data_folder, "high_input_modified.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------------------
# Importing the dataset:
Evenson_Data <- read_excel(file.path(data_folder,"Evenson Data on Modern Varieties and Development, 5-05.xls"))

# Keeping the columns from 1 to 16
Evenson_Data <- Evenson_Data[1:820, c(1:41)]

# Drop the rows whose fao_abbr has value "Ethiopia PDR", "Northern Mariana Islands" and "Eswatini"
Evenson_Data <- subset(Evenson_Data, !(fao_abbr %in% c("Ethiopia PDR", "Northern Mariana Islands", "Eswatini")))

# Rename the processed dataframe
evenson <- Evenson_Data

# Rename fao_abbr to country in evenson
names(evenson)[names(evenson) == "fao_abbr"] <- "country"

# Load modified data sets
low_input_modified <- read.csv(file.path(data_folder, "low_input_modified.csv"))
high_input_modified <- read.csv(file.path(data_folder, "high_input_modified.csv"))

# Rename to country in modified dataset
names(low_input_modified)[names(low_input_modified) == "name"] <- "country"
names(high_input_modified)[names(high_input_modified) == "name"] <- "country"


# Mapping of country names in evenson to corresponding names in high/low_input datasets
# country_name_mapping <- c("Cent Afr Rep"= "Central African Republic",
#                          "Dominican Rp"= "Dominican Republic",
#                          "Congo, Dem R"= "Democratic Republic of the Congo",
#                          "GuineaBissau"= "Guinea-Bissau",
#                          "NewCaledonia"= "New Caledonia",
#                          "Iran" = "Iran (Islamic Republic of)",
#                          "Laos" = "Lao People's Democratic Republic",
#                          "Libya" = "Libyan Arab Jamahiriya",
#                          "Sudan" = "Sudan (former)",
#                          "Syria" = "Syrian Arab Republic",
#                          "Venezuela" = "Venezuela (Bolivarian Republic of)",
#                          "Vietnam" = "Viet Nam")  

# Replace country names in evenson using the mapping
# evenson$country <- as.character(evenson$country)
# evenson$country[evenson$country %in% names(country_name_mapping)] <- country_name_mapping[evenson$country] 

# Brian note to Ruoran: I don't know why the lines commented above didn't work.
# Instead of fixing it, I changed the approach using the lines below.
# If you agree with this procedure, please delete these comments. :)

# Names to replace in Evenson:
old_country_names <- c("Cent Afr Rep",
                       "Dominican Rp",
                       "Congo, Dem R",
                       "GuineaBissau",
                       "NewCaledonia",
                       "Iran",
                       "Laos",
                       "Libya",
                       "Sudan",
                       "Syria",
                       "Venezuela",
                       "Vietnam")
new_country_names <- c("Central African Republic",
                       "Dominican Republic",
                       "Democratic Republic of the Congo",
                       "Guinea-Bissau",
                       "New Caledonia",
                       "Iran (Islamic Republic of)",
                       "Lao People's Democratic Republic",
                       "Libyan Arab Jamahiriya",
                       "Sudan (former)",
                       "Syrian Arab Republic",
                       "Venezuela (Bolivarian Republic of)",
                       "Viet Nam")  

evenson$country <- replace(evenson$country, evenson$country %in% old_country_names, new_country_names)

# Merge evenson with low_input_modified and then merge the resulting dataframe with high_input_modified
merged_df <- merge(evenson, low_input_modified, by = c('country', 'year'), all = TRUE)
merged_df <- merge(merged_df, high_input_modified, by = c('country', 'year'), all = TRUE)

# Write merged data frame to a csv file
write.csv(merged_df, file.path(data_folder, "merged_dataframe.csv"), row.names = FALSE)



# ------------------------------------------------------------------------------------------
# Importing the data

# high and low input transition for Rice

#df <- read_excel(file.path(data_folder,"detailed_merged_dataframe.xls"))
df <- read_csv(file.path(data_folder,"merged_dataframe.csv"))
df <- df %>%
  mutate(across(c(ricd200b_yld.x, ricd200b_yld.y), as.numeric)) %>%
  mutate(
    ricd200new = case_when(
      year <= 1965 ~ ricd200b_yld.x,
      year >= 1970 ~ ricd200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(ricw200b_yld.x, ricw200b_yld.y), as.numeric)) %>%
  mutate(
    ricw200new = case_when(
      year <= 1965 ~ ricw200b_yld.x,
      year >= 1970 ~ ricw200b_yld.y,
      TRUE ~ NA_real_
    )
  )

# high and low input transition for Barley
df <- df %>%
  mutate(across(c(barl200b_yld.x, barl200b_yld.y), as.numeric)) %>%
  mutate(
    barl200new = case_when(
      year <= 1975 ~ barl200b_yld.x,
      year >= 1980 ~ barl200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(sbrl200b_yld.x, sbrl200b_yld.y), as.numeric)) %>%
  mutate(
    sbrl200new = case_when(
      year <= 1975 ~ sbrl200b_yld.x,
      year >= 1980 ~ sbrl200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(wbrl200b_yld.x, wbrl200b_yld.y), as.numeric)) %>%
  mutate(
    wbrl200new = case_when(
      year <= 1975 ~ wbrl200b_yld.x,
      year >= 1980 ~ wbrl200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Maize
df <- df %>%
  mutate(across(c(maiz200b_yld.x, maiz200b_yld.y), as.numeric)) %>%
  mutate(
    maiz200new = case_when(
      year <= 1965 ~ maiz200b_yld.x,
      year >= 1970 ~ maiz200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(hmze200b_yld.x, hmze200b_yld.y), as.numeric)) %>%
  mutate(
    hmze200new = case_when(
      year <= 1965 ~ hmze200b_yld.x,
      year >= 1970 ~ hmze200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(lmze200b_yld.x, lmze200b_yld.y), as.numeric)) %>%
  mutate(
    lmze200new = case_when(
      year <= 1965 ~ lmze200b_yld.x,
      year >= 1970 ~ lmze200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(tmze200b_yld.x, tmze200b_yld.y), as.numeric)) %>%
  mutate(
    tmze200new = case_when(
      year <= 1965 ~ tmze200b_yld.x,
      year >= 1970 ~ tmze200b_yld.y,
      TRUE ~ NA_real_
    )
  )

# high and low input transition for Millet
df <- df %>%
  mutate(across(c(mllt200b_yld.x, mllt200b_yld.y), as.numeric)) %>%
  mutate(
    mllt200new = case_when(
      year <= 1980 ~ mllt200b_yld.x,
      year >= 1985 ~ mllt200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(fmlt200b_yld.x, fmlt200b_yld.y), as.numeric)) %>%
  mutate(
    fmlt200new = case_when(
      year <= 1980 ~ fmlt200b_yld.x,
      year >= 1985 ~ fmlt200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(pmlt200b_yld.x, pmlt200b_yld.y), as.numeric)) %>%
  mutate(
    pmlt200new = case_when(
      year <= 1980 ~ pmlt200b_yld.x,
      year >= 1985 ~ pmlt200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Sorghum
df <- df %>%
  mutate(across(c(sorg200b_yld.x, sorg200b_yld.y), as.numeric)) %>%
  mutate(
    sorg200new = case_when(
      year <= 1980 ~ sorg200b_yld.x,
      year >= 1985 ~ sorg200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(hsrg200b_yld.x, hsrg200b_yld.y), as.numeric)) %>%
  mutate(
    hsrg200new = case_when(
      year <= 1980 ~ hsrg200b_yld.x,
      year >= 1985 ~ hsrg200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(lsrg200b_yld.x, lsrg200b_yld.y), as.numeric)) %>%
  mutate(
    lsrg200new = case_when(
      year <= 1980 ~ lsrg200b_yld.x,
      year >= 1985 ~ lsrg200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(tsrg200b_yld.x, tsrg200b_yld.y), as.numeric)) %>%
  mutate(
    tsrg200new = case_when(
      year <= 1980 ~ tsrg200b_yld.x,
      year >= 1985 ~ tsrg200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Wheat
df <- df %>%
  mutate(across(c(whea200b_yld.x, whea200b_yld.y), as.numeric)) %>%
  mutate(
    whea200new = case_when(
      year <= 1960 ~ whea200b_yld.x,
      year >= 1965 ~ whea200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(swhe200b_yld.x, swhe200b_yld.y), as.numeric)) %>%
  mutate(
    swhe200new = case_when(
      year <= 1960 ~ swhe200b_yld.x,
      year >= 1965 ~ swhe200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(wwhe200b_yld.x, wwhe200b_yld.y), as.numeric)) %>%
  mutate(
    wwhe200new = case_when(
      year <= 1960 ~ wwhe200b_yld.x,
      year >= 1965 ~ wwhe200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Soybean
df <- df %>%
  mutate(across(c(soyb200b_yld.x, soyb200b_yld.y), as.numeric)) %>%
  mutate(
    soyb200new = case_when(
      year <= 1975 ~ soyb200b_yld.x,
      year >= 1980 ~ soyb200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Yam
df <- df %>%
  mutate(across(c(yams200b_yld.x, yams200b_yld.y), as.numeric)) %>%
  mutate(
    yams200new = case_when(
      year <= 1985 ~ yams200b_yld.x,
      year >= 1990 ~ yams200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(gyam200b_yld.x, gyam200b_yld.y), as.numeric)) %>%
  mutate(
    gyam200new = case_when(
      year <= 1985 ~ gyam200b_yld.x,
      year >= 1990 ~ gyam200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(wyam200b_yld.x, wyam200b_yld.y), as.numeric)) %>%
  mutate(
    wyam200new = case_when(
      year <= 1985 ~ wyam200b_yld.x,
      year >= 1990 ~ wyam200b_yld.y,
      TRUE ~ NA_real_
    )
  )
df <- df %>%
  mutate(across(c(yyam200b_yld.x, yyam200b_yld.y), as.numeric)) %>%
  mutate(
    yyam200new = case_when(
      year <= 1985 ~ yyam200b_yld.x,
      year >= 1990 ~ yyam200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Dry Beans
df <- df %>%
  mutate(across(c(bean200b_yld.x, bean200b_yld.y), as.numeric)) %>%
  mutate(
    bean200new = case_when(
      year <= 1975 ~ bean200b_yld.x,
      year >= 1980 ~ bean200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Cassava
df <- df %>%
  mutate(across(c(casv200b_yld.x, casv200b_yld.y), as.numeric)) %>%
  mutate(
    casv200new = case_when(
      year <= 1980 ~ casv200b_yld.x,
      year >= 1985 ~ casv200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for White Potato
df <- df %>%
  mutate(across(c(wpot200b_yld.x, wpot200b_yld.y), as.numeric)) %>%
  mutate(
    wpot200new = case_when(
      year <= 1985 ~ wpot200b_yld.x,
      year >= 1990 ~ wpot200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Sweet Potato
df <- df %>%
  mutate(across(c(spot200b_yld.x, spot200b_yld.y), as.numeric)) %>%
  mutate(
    spot200new = case_when(
      year <= 1980 ~ spot200b_yld.x,
      year >= 1985 ~ spot200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Groundnut
df <- df %>%
  mutate(across(c(grnd200b_yld.x, grnd200b_yld.y), as.numeric)) %>%
  mutate(
    grnd200new = case_when(
      year <= 1980 ~ grnd200b_yld.x,
      year >= 1985 ~ grnd200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Chickpea
df <- df %>%
  mutate(across(c(chck200b_yld.x, chck200b_yld.y), as.numeric)) %>%
  mutate(
    chck200new = case_when(
      year <= 1980 ~ chck200b_yld.x,
      year >= 1985 ~ chck200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# high and low input transition for Cowpea
df <- df %>%
  mutate(across(c(cowp200b_yld.x, cowp200b_yld.y), as.numeric)) %>%
  mutate(
    cowp200new = case_when(
      year <= 1970 ~ cowp200b_yld.x,
      year >= 1975 ~ cowp200b_yld.y,
      TRUE ~ NA_real_
    )
  )
# Write df data frame to a csv file
write.csv(df, file.path(data_folder, "full_merged_dataframe.csv"), row.names = FALSE)


# Keeping the columns that are useful to first analysis
df_Data <- df[1:820, c(1:2,7:17, 31:41,104:134)]



# Read the land size data
df_landsize <- read.csv(file.path(data_folder,"Landcsv.csv"))

# Adjust country names
df_landsize <- df_landsize %>%
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    country == "Congo, Rep." ~ "Congo, Rep",
    country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    country == "Egypt, Arab Rep." ~ "Egypt",
    country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    country == "Lao PDR" ~ "Lao People's Democratic Republic",
    country == "Libya" ~ "Libyan Arab Jamahiriya",
    country == "Myanmar" ~ "Myanmar",
    country == "Syrian Arab Republic" ~ "Syrian Arab Republic",
    country == "Vietnam" ~ "Viet Nam",
    country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ country
  ))

# Define list of countries
countries <- c('Algeria', 'Angola', 'Argentina', 'Bangladesh', 'Benin', 'Bolivia', 'Botswana', 
               'Brazil', 'Burkina Faso', 'Burundi', 'Cambodia', 'Cameroon', 
               'Central African Republic', 'Chad', 'Chile', 'China', 'Colombia', 
               'Congo, Rep', 'Costa Rica', 'Côte d\'Ivoire', 'Cuba', 
               'Democratic Republic of the Congo', 'Dominican Republic', 'Ecuador', 
               'Egypt', 'El Salvador', 'Ethiopia', 'Gabon', 'Gambia', 'Ghana', 
               'Guatemala', 'Guinea', 'Guinea-Bissau', 'Haiti', 'Honduras', 
               'India', 'Indonesia', 'Iran (Islamic Republic of)', 'Iraq', 
               'Jamaica', 'Jordan', 'Kenya', 'Lao People\'s Democratic Republic', 
               'Lebanon', 'Liberia', 'Libyan Arab Jamahiriya', 'Madagascar', 
               'Malawi', 'Malaysia', 'Mali', 'Mauritania', 'Mauritius', 'Mexico', 
               'Mongolia', 'Morocco', 'Mozambique', 'Myanmar', 'N Marianas', 
               'Namibia', 'Nepal', 'New Caledonia', 'Nicaragua', 'Niger', 'Nigeria', 
               'Pakistan', 'Panama', 'Paraguay', 'Peru', 'Philippines', 'Rwanda', 
               'Saudi Arabia', 'Senegal', 'Sierra Leone', 'South Africa', 'Sri Lanka', 
               'Sudan', 'Swaziland', 'Syrian Arab Republic', 'Tanzania', 'Thailand', 
               'Togo', 'Tunisia', 'Turkey', 'Uganda', 'Uruguay', 'Viet Nam', 
               'Yemen', 'Zambia', 'Zimbabwe')

# Filter land size data
df_landsize_filtered <- df_landsize %>% 
  filter(country %in% countries)

# Check the column names
colnames(df_landsize_filtered)

# If column names do start with 'X', remove it
colnames(df_landsize_filtered) <- gsub("^X", "", colnames(df_landsize_filtered))

# Pivot the data to long format
df_landsize_melted <- df_landsize_filtered %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "land_size(km^2)")

# Convert year to numeric
df_landsize_melted$year <- as.numeric(df_landsize_melted$year)

# Merge the dataframes
df_final <- df_Data %>% left_join(df_landsize_melted, by = c("country", "year"))


# Conversion factor from square kilometers to acres
conversion_factor <- 247.105

# Add new column with converted unit
# df_landsize_melted <- df_landsize_melted %>%
#  mutate(land_size_acre = `land_size(km^2)` * conversion_factor)

df_final <- df_final %>%
    mutate(land_size_acre = `land_size(km^2)` * conversion_factor)
  
# List of crop names
crops <- c("Rice", "Wheat", "Maize", "Barley", "Millet", "Sorghum", "Cassava", 
           "Potatoes", "Groundnut", "Dry Beans", "Lentils")

# Create a second list for the MV crop names
mv_crops <- c("Rice", "Wheat", "Maize", "Barley", "Pearl Millet", "Sorghum", "Cassava", 
              "Potato", "Groundnut", "Beans", "Lentils")

# Calculate crop area as a share of total land area
for (crop in crops) {
  df_final <- df_final %>%
    mutate(!!paste0(crop, " Area_share") := !!sym(paste0(crop, " Area")) / land_size_acre)
}

# Calculate MV area as a share of total crop area, and normalize by total land area
for (i in 1:length(mv_crops)) {
  df_final <- df_final %>%
    mutate(!!paste0("MV_", crops[i], " Area_share") := 
             (!!sym(paste0("MV ", mv_crops[i], " Area (%)")) / 100) * 
             !!sym(paste0(crops[i], " Area")) / land_size_acre)
}


# Merge the dataframes
# df_final <- df_Data %>% left_join(df_landsize_melted, by = c("country", "year"))


# Write df data frame to a csv file
write.csv(df_final, file.path(data_folder, "land_merged_dataframe.csv"), row.names = FALSE)

# Save data as Rdata to use in the presentations Rmd file.
save(file = file.path(data_folder, "land_merged_dataframe.Rdata"), 
     list = c("df_final") )


#
#                   ,mMm.,------.,mMm.
#                    (GNP'        `?ND)
#                     P  dMm.  ,mMb  ?
#                     (  ?X_O  O_XP  )
#                     (      qp      )   NOTES:
#                      \  `--'`--'  /    The stuff below must be a separate R script.  
#                   _____         ,__)   Above is data cleaning still.
#                  (--|__) _.._  _| _,   But the stuff below is analysis already
#                    _|   (_|| |(_|(_|
#                   (                     
# Additional little things to clean up:
# - The dataframe used below should be df_final, not df. For now I'll just do the
# following:
df <- df_final 
# - Also, it is enought to make year as a factor only once:
# To interact fixed effects, as in delta_kt, we need to convert first at least
# one of the variables to factor:
df <- mutate(df, year = as.factor(year))
# - Since all the regressions are similar, it might be easier to write an R
# function to automatize stuff. Read about this for later.
# - Is 'Region' in the final data set?

# ------------------------------------------------------------------------------------------

# Let's think later if plm would better than felm. I suspect felm is more flexible
# Set panel data structure
# pdata <- pdata.frame(df, index = c("country","year"))
# Panel regression
# modelrice <- plm("MV Rice Area (%)" ~ ricd200new, data = pdata, model = "random")
# Print summary
# summary(modelrice)

#------ Rice

# I will do it with felm, and a regular OLS as benchmark:
modelricd_linear <- lm(`MV Rice Area (%)` ~ `ricd200new`, data = df)
modelricd_fe <- felm(`MV Rice Area (%)` ~ `ricd200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelricd_fe_int<-felm(`MV Rice Area (%)` ~ `ricd200new` | `Region`:`year` , data = df) 

# Let's see the results:
stargazer(modelricd_linear, modelricd_fe, modelricd_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


#
modelricw_linear <- lm(`MV Rice Area (%)` ~ `ricw200new`, data = df)
modelricw_fe <- felm(`MV Rice Area (%)` ~ `ricw200new` | `Country ID` + `year` , data = df)

# To interact fixed effects, as in delta_kt, we need to convert first at least
# one of the variables to factor:
df <- mutate(df, year = as.factor(year))

# Then we can do the interactions with ":"
modelricw_fe_int<-felm(`MV Rice Area (%)` ~ `ricw200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelricw_linear, modelricw_fe, modelricw_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#----------------Barley
modelbarl_linear <- lm(`MV Barley Area (%)` ~ `barl200new`, data = df)
modelbarl_fe <- felm(`MV Barley Area (%)` ~ `barl200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelbarl_fe_int<-felm(`MV Barley Area (%)` ~ `barl200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelbarl_linear, modelbarl_fe, modelbarl_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelsbrl_linear <- lm(`MV Barley Area (%)` ~ `sbrl200new`, data = df)
modelsbrl_fe <- felm(`MV Barley Area (%)` ~ `sbrl200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelsbrl_fe_int<-felm(`MV Barley Area (%)` ~ `sbrl200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelsbrl_linear, modelsbrl_fe, modelsbrl_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelwbrl_linear <- lm(`MV Barley Area (%)` ~ `wbrl200new`, data = df)
modelwbrl_fe <- felm(`MV Barley Area (%)` ~ `wbrl200new` | `Country ID` + `year` , data = df)


# Then we can do the interactions with ":"
modelwbrl_fe_int<-felm(`MV Barley Area (%)` ~ `wbrl200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelwbrl_linear, modelwbrl_fe, modelwbrl_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#----- Maize
modelmaiz_linear <- lm(`MV Maize Area (%)` ~ `maiz200new`, data = df)
modelmaiz_fe <- felm(`MV Maize Area (%)` ~ `maiz200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelmaiz_fe_int<-felm(`MV Maize Area (%)` ~ `maiz200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelmaiz_linear, modelmaiz_fe, modelmaiz_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelhmze_linear <- lm(`MV Maize Area (%)` ~ `hmze200new`, data = df)
modelhmze_fe <- felm(`MV Maize Area (%)` ~ `hmze200new` | `Country ID` + `year` , data = df)


# Then we can do the interactions with ":"
modelhmze_fe_int<-felm(`MV Maize Area (%)` ~ `hmze200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelhmze_linear, modelhmze_fe, modelhmze_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modellmze_linear <- lm(`MV Maize Area (%)` ~ `lmze200new`, data = df)
modellmze_fe <- felm(`MV Maize Area (%)` ~ `lmze200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modellmze_fe_int<-felm(`MV Maize Area (%)` ~ `lmze200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modellmze_linear, modellmze_fe, modellmze_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modeltmze_linear <- lm(`MV Maize Area (%)` ~ `tmze200new`, data = df)
modeltmze_fe <- felm(`MV Maize Area (%)` ~ `tmze200new` | `Country ID` + `year` , data = df)


# Then we can do the interactions with ":"
modeltmze_fe_int<-felm(`MV Maize Area (%)` ~ `tmze200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modeltmze_linear, modeltmze_fe, modeltmze_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#-------Millet

modelmllt_linear <- lm(`MV Pearl Millet Area (%)` ~ `mllt200new`, data = df)
modelmllt_fe <- felm(`MV Pearl Millet Area (%)` ~ `mllt200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelmllt_fe_int<-felm(`MV Pearl Millet Area (%)` ~ `mllt200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelmllt_linear, modelmllt_fe, modelmllt_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelfmlt_linear <- lm(`MV Pearl Millet Area (%)` ~ `fmlt200new`, data = df)
modelfmlt_fe <- felm(`MV Pearl Millet Area (%)` ~ `fmlt200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelfmlt_fe_int<-felm(`MV Pearl Millet Area (%)` ~ `fmlt200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelfmlt_linear, modelfmlt_fe, modelfmlt_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelpmlt_linear <- lm(`MV Pearl Millet Area (%)` ~ `pmlt200new`, data = df)
modelpmlt_fe <- felm(`MV Pearl Millet Area (%)` ~ `pmlt200new` | `Country ID` + `year` , data = df)


# Then we can do the interactions with ":"
modelpmlt_fe_int<-felm(`MV Pearl Millet Area (%)` ~ `pmlt200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelpmlt_linear, modelpmlt_fe, modelpmlt_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


#-----------Sorghum

modelsorg_linear <- lm(`MV Sorghum Area (%)` ~ `sorg200new`, data = df)
modelsorg_fe <- felm(`MV Sorghum Area (%)` ~ `sorg200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelsorg_fe_int<-felm(`MV Sorghum Area (%)` ~ `sorg200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelsorg_linear, modelsorg_fe, modelsorg_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelhsrg_linear <- lm(`MV Sorghum Area (%)` ~ `hsrg200new`, data = df)
modelhsrg_fe <- felm(`MV Sorghum Area (%)` ~ `hsrg200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelhsrg_fe_int<-felm(`MV Sorghum Area (%)` ~ `hsrg200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelhsrg_linear, modelhsrg_fe, modelhsrg_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modellsrg_linear <- lm(`MV Sorghum Area (%)` ~ `lsrg200new`, data = df)
modellsrg_fe <- felm(`MV Sorghum Area (%)` ~ `lsrg200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modellsrg_fe_int<-felm(`MV Sorghum Area (%)` ~ `lsrg200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modellsrg_linear, modellsrg_fe, modellsrg_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modeltsrg_linear <- lm(`MV Sorghum Area (%)` ~ `tsrg200new`, data = df)
modeltsrg_fe <- felm(`MV Sorghum Area (%)` ~ `tsrg200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modeltsrg_fe_int<-felm(`MV Sorghum Area (%)` ~ `tsrg200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modeltsrg_linear, modeltsrg_fe, modeltsrg_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#------Wheat
modelwhea_linear <- lm(`MV Wheat Area (%)` ~ `whea200new`, data = df)
modelwhea_fe <- felm(`MV Wheat Area (%)` ~ `whea200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelwhea_fe_int<-felm(`MV Wheat Area (%)` ~ `whea200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelwhea_linear, modelwhea_fe, modelwhea_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelswhe_linear <- lm(`MV Wheat Area (%)` ~ `swhe200new`, data = df)
modelswhe_fe <- felm(`MV Wheat Area (%)` ~ `swhe200new` | `Country ID` + `year` , data = df)


# Then we can do the interactions with ":"
modelswhe_fe_int<-felm(`MV Wheat Area (%)` ~ `swhe200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelswhe_linear, modelswhe_fe, modelswhe_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


modelwwhe_linear <- lm(`MV Wheat Area (%)` ~ `wwhe200new`, data = df)
modelwwhe_fe <- felm(`MV Wheat Area (%)` ~ `wwhe200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelwwhe_fe_int<-felm(`MV Wheat Area (%)` ~ `wwhe200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelwwhe_linear, modelwwhe_fe, modelwwhe_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#-----Soybean
modelsoyb_linear <- lm(`MV Beans Area (%)` ~ `soyb200new`, data = df)
modelsoyb_fe <- felm(`MV Beans Area (%)` ~ `soyb200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelsoyb_fe_int<-felm(`MV Beans Area (%)` ~ `soyb200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelsoyb_linear, modelsoyb_fe, modelsoyb_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#-----Cassava
modelcasv_linear <- lm(`MV Cassava Area (%)` ~ `casv200new`, data = df)
modelcasv_fe <- felm(`MV Cassava Area (%)` ~ `casv200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelcasv_fe_int<-felm(`MV Cassava Area (%)` ~ `casv200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelcasv_linear, modelcasv_fe, modelcasv_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#-----Potato
modelwpot_linear <- lm(`MV Potato Area (%)` ~ `wpot200new`, data = df)
modelwpot_fe <- felm(`MV Potato Area (%)` ~ `wpot200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelwpot_fe_int<-felm(`MV Potato Area (%)` ~ `wpot200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelwpot_linear, modelwpot_fe, modelwpot_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#-----Groundnuts
modelgrnd_linear <- lm(`MV Groundnut Area (%)` ~ `grnd200new`, data = df)
modelgrnd_fe <- felm(`MV Groundnut Area (%)` ~ `grnd200new` | `Country ID` + `year` , data = df)

# Then we can do the interactions with ":"
modelgrnd_fe_int<-felm(`MV Groundnut Area (%)` ~ `grnd200new` | `Region`:`year` , data = df)

# Let's see the results:
stargazer(modelgrnd_linear, modelgrnd_fe, modelgrnd_fe_int,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")


#------------------------------------------------------------------------------------------------
#The Second Analysis(sample): Land area by crop [*]; [*] == [Normalized by the share of country land area]
#------------------------------------------------------------------------------------------------

#------ Rice


modelricd_linear_nor <- lm(`Rice Area_share` ~ `ricd200new`, data = df_final)
modelricd_fe_nor <- felm(`Rice Area_share` ~ `ricd200new` | `Country ID` + `year` , data = df_final)

# Then we can do the interactions with ":"
modelricd_fe_int_nor<-felm(`Rice Area_share` ~ `ricd200new` | `Region`:`year` , data = df_final)

# Let's see the results:
stargazer(modelricd_linear_nor, modelricd_fe_nor, modelricd_fe_int_nor,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")

#------------------------------------------------------------------------------------------------
#The Third Analysis(sample): Land area in MV by crop = MV share \times Rice area. [*]; 
# [*] == [Normalized by the share of country land area]
#------------------------------------------------------------------------------------------------

#------ Rice

modelricd_linear_nor <- lm(`MV_Rice Area_share` ~ `ricd200new`, data = df_final)
modelricd_fe_nor <- felm(`MV_Rice Area_share` ~ `ricd200new` | `Country ID` + `year` , data = df_final)

# To interact fixed effects, as in delta_kt, we need to convert first at least
# one of the variables to factor:
df_final <- mutate(df_final, year = as.factor(year))

# Then we can do the interactions with ":"
modelricd_fe_int_nor<-felm(`MV_Rice Area_share` ~ `ricd200new` | `Region`:`year` , data = df_final)

# Let's see the results:
stargazer(modelricd_linear_nor, modelricd_fe_nor, modelricd_fe_int_nor,
          add.lines = list(c("Fixed Effects:","No","Year + Country", "Year*Region")),
          type="text")
