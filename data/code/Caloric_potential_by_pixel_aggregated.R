# Merge Caloric Potential with Evenson dataset 
# ==============================================================================

# To dos:
# -----------------------------------------------------------------------------

# - RHS: max caloric yield across all crops in Table 1 of GHW
#   Calculate max caloric yield at pixel level
#   Then aggregate to country level (take mean across pixels)
# - Outcomes to examine (from Evenson-Gollin dataset):
#   Child mortality rate
#   Birth rate
#   Death rate
#   Rural population density
#   GDP per capita

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

## Goal 1: Calculate the maximum yield by pixel:

# 1. Low-input:

# Convert lon and lat to numeric
test1$lon <- as.numeric(test1$lon)
test1$lat <- as.numeric(test1$lat)

# Verify if they uniquely identify the pixels:

test1$id <- paste(test1$lon, test1$lat, sep = "_")
is_duplicate <- duplicated(test1$id)
any(is_duplicate)

# 2. High input:

# Convert lon and lat to numeric
test2$lon <- as.numeric(test2$lon)
test2$lat <- as.numeric(test2$lat)

# Verify if they uniquely identify the pixels:

test2$id <- paste(test2$lon, test2$lat, sep = "_")
is_duplicate <- duplicated(test2$id)
any(is_duplicate)

# 3. Merge both datasets:
yields_by_pixel <- merge(test1,test2, by = c('lon','lat'))

rm(test1)
rm(test2)

# 4. Calculate high and low input transitions:

# Select all the variable names that include yields:
yield_columns <- grep("yld", names(yields_by_pixel), value = TRUE)

# - Need all the corresponding variables as numeric:
yields_by_pixel <- yields_by_pixel %>% 
  mutate(across(all_of(yield_columns),
                as.numeric))

# - Expand the list of pixels through the period of analysis:

yields_by_pixel <- yields_by_pixel[c('lon','lat',yield_columns)]
expanded_data <- rbind(yields_by_pixel, yields_by_pixel, yields_by_pixel,
                       yields_by_pixel, yields_by_pixel, yields_by_pixel,
                       yields_by_pixel, yields_by_pixel, yields_by_pixel)
expanded_data$year <- rep(1:9, each = nrow(yields_by_pixel))
expanded_data$year <- expanded_data$year*5 + 1955

# - Generating new variables with the corresponding transitions: 
expanded_data <- expanded_data %>%
  mutate(
    ricd200new = case_when(
      year <= 1965 ~ ricd200b_yld.x,
      year >= 1970 ~ ricd200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    ricw200new = case_when(
      year <= 1965 ~ ricw200b_yld.x,
      year >= 1970 ~ ricw200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    barl200new = case_when(
      year <= 1975 ~ barl200b_yld.x,
      year >= 1980 ~ barl200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    sbrl200new = case_when(
      year <= 1975 ~ sbrl200b_yld.x,
      year >= 1980 ~ sbrl200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    wbrl200new = case_when(
      year <= 1975 ~ wbrl200b_yld.x,
      year >= 1980 ~ wbrl200b_yld.y,
      TRUE ~ NA_real_
    )
    ,  maiz200new = case_when(
      year <= 1965 ~ maiz200b_yld.x,
      year >= 1970 ~ maiz200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    hmze200new = case_when(
      year <= 1965 ~ hmze200b_yld.x,
      year >= 1970 ~ hmze200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    lmze200new = case_when(
      year <= 1965 ~ lmze200b_yld.x,
      year >= 1970 ~ lmze200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    tmze200new = case_when(
      year <= 1965 ~ tmze200b_yld.x,
      year >= 1970 ~ tmze200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    mllt200new = case_when(
      year <= 1980 ~ mllt200b_yld.x,
      year >= 1985 ~ mllt200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    fmlt200new = case_when(
      year <= 1980 ~ fmlt200b_yld.x,
      year >= 1985 ~ fmlt200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    pmlt200new = case_when(
      year <= 1980 ~ pmlt200b_yld.x,
      year >= 1985 ~ pmlt200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    sorg200new = case_when(
      year <= 1980 ~ sorg200b_yld.x,
      year >= 1985 ~ sorg200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    hsrg200new = case_when(
      year <= 1980 ~ hsrg200b_yld.x,
      year >= 1985 ~ hsrg200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    lsrg200new = case_when(
      year <= 1980 ~ lsrg200b_yld.x,
      year >= 1985 ~ lsrg200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    tsrg200new = case_when(
      year <= 1980 ~ tsrg200b_yld.x,
      year >= 1985 ~ tsrg200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    whea200new = case_when(
      year <= 1960 ~ whea200b_yld.x,
      year >= 1965 ~ whea200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    swhe200new = case_when(
      year <= 1960 ~ swhe200b_yld.x,
      year >= 1965 ~ swhe200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    wwhe200new = case_when(
      year <= 1960 ~ wwhe200b_yld.x,
      year >= 1965 ~ wwhe200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    soyb200new = case_when(
      year <= 1975 ~ soyb200b_yld.x,
      year >= 1980 ~ soyb200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    yams200new = case_when(
      year <= 1985 ~ yams200b_yld.x,
      year >= 1990 ~ yams200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    gyam200new = case_when(
      year <= 1985 ~ gyam200b_yld.x,
      year >= 1990 ~ gyam200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    wyam200new = case_when(
      year <= 1985 ~ wyam200b_yld.x,
      year >= 1990 ~ wyam200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    yyam200new = case_when(
      year <= 1985 ~ yyam200b_yld.x,
      year >= 1990 ~ yyam200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    bean200new = case_when(
      year <= 1975 ~ bean200b_yld.x,
      year >= 1980 ~ bean200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    casv200new = case_when(
      year <= 1980 ~ casv200b_yld.x,
      year >= 1985 ~ casv200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    wpot200new = case_when(
      year <= 1985 ~ wpot200b_yld.x,
      year >= 1990 ~ wpot200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    spot200new = case_when(
      year <= 1980 ~ spot200b_yld.x,
      year >= 1985 ~ spot200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    grnd200new = case_when(
      year <= 1980 ~ grnd200b_yld.x,
      year >= 1985 ~ grnd200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    chck200new = case_when(
      year <= 1980 ~ chck200b_yld.x,
      year >= 1985 ~ chck200b_yld.y,
      TRUE ~ NA_real_
    )
    ,
    cowp200new = case_when(
      year <= 1970 ~ cowp200b_yld.x,
      year >= 1975 ~ cowp200b_yld.y,
      TRUE ~ NA_real_
    )
  )

# - Let's filter only these variables:

expanded_data <- expanded_data %>%
  select(lon,lat,year,ends_with('new')) # Why are some zeroes?

# - Let's select the max yield by pixel:
expanded_data <- expanded_data %>%
  mutate(max_yield = max(c_across(ends_with("new")), na.rm = TRUE)) 

expanded_data <- expanded_data %>% 
  select(lon,lat,year,max_yield)

# -----------------------------------------------------------------------------

## Goal 2: Collapse at the country level:
  
# Convert the dataframe to a spatial dataframe
expanded_data_sf <- st_as_sf(expanded_data, coords = c("lon", "lat"), crs = 4326)

# Load the shapefile
shapefile_path <- file.path(data_folder, "Shapefiles", "world_data_administrative_boundaries_country_level", 
                            "world_data_administrative_boundaries_country_level.shp")
world <- st_read(shapefile_path)

# Make sure that geometries are valid
world <- st_make_valid(world)
expanded_data_sf <- st_make_valid(expanded_data_sf)

# Spatial join:

expanded_data_sf <- st_join(expanded_data_sf, world, join = st_intersects)

# Average by country:

averages <- expanded_data_sf %>% st_drop_geometry() %>% group_by(name,year) %>% 
  summarize(avg_max_yield = mean(max_yield, na.rm = TRUE), .groups = "drop")
  

# Saving these intermediate datasets:
save(file = file.path(data_folder, "max_yield_merged_dataframe.Rdata"), 
     list = c("expanded_data_sf","world","averages") )


# ------------------------------------------------------------------------------------------

## Goal 3: Merge with the Evanson dataset:

# Load the dataset previously saved as the final dataset:

load(file.path(data_folder, "land_merged_dataframe.Rdata"))
df_final <- mutate(df_final, year = as.factor(year))

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

# Filter averages:
averages <- averages[averages$name %in% country_list,]
colnames(averages) <- c('country','year','avg_max_yield')

# Merge the previous final dataset with these averages:
df_final <- merge(df_final,averages, by = c('country','year'))

# Importing the other variables from the Evenson dataset:
Evenson_Data <- read_excel(file.path(data_folder,"Evenson Data on Modern Varieties and Development, 5-05.xls"))

# Keeping the columns we are going to use for the analysis
Evenson_Data <- Evenson_Data[c("fao_abbr","year","Child Mortality Rate",
                    "Birth Rate",
                    "Death Rate",
                    "Rural population density",
                    "Total population",
                    "GDP per capita")]


# Drop the rows whose fao_abbr has value "Ethiopia PDR", "Northern Mariana Islands" and "Eswatini"
Evenson_Data <- subset(Evenson_Data, !(fao_abbr %in% c("Ethiopia PDR", "Northern Mariana Islands", "Eswatini")))

# Rename the processed dataframe
evenson <- Evenson_Data
names(evenson)[names(evenson) == "fao_abbr"] <- "country"

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

# Note: The new names appear there, but we get a warning. Revisit this.

# Merge this and the previous final dataset:
df_final <- merge(df_final,evenson, by = c('country','year'))

# Saving these intermediate datasets:
save(file = file.path(data_folder, "final_comp_dataframe.Rdata"), 
     list = c("df_final") )

