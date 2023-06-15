# Caloric Potential
# ==============================================================================

# To dos:
# -----------------------------------------------------------------------------

# - Move the plots to a section at the end or to the Data Cleaning Master File.Rmd
# - Don't load packages that are not being used.
# - Think better about the selection of the high intensity caloric potential.
# - Change the name of the dataframes: "test1" is not informative.


# Outcomes:
# -----------------------------------------------------------------------------

# Saves the dataframes test1 and test2, which produce the low input and hi 
# caloric potentials respectively, in Caloric_potential.Rdata

# Setup
# -----------------------------------------------------------------------------

# Libraries
library(maptools)
library(raster)
library(sf)
library(rgdal)
library(jtools) # Load jtools
library(googlesheets4)
library(dplyr)
library(tidyr)
library(tidyfst)
library(rasterVis)
library(ggplot2)
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

# Define a name for the data folder:
if  (Sys.getenv("USERNAME")=="dazav") {
  data_folder <- here('C:/Users/dazav/Dropbox (Personal)/Research Projects/Green Revolution/Data')
} 
if (Sys.getenv("USER")=="liruoran"){
  data_folder <- here("/Users/liruoran/Dropbox (University of Michigan)/Green Revolution/Data")
}


# Load USDA SR28 Calories Table
# ------------------------------------------------------------------------------

# Import Calories
calories_raw = read.csv(file.path(data_folder, "GAEZ DATA", "Data and Documentation","GAEZ Calories.csv")) # Q: Was this dataset manually created elsewhere?
# Keep observations with information
calories_raw=calories_raw[complete.cases(calories_raw), ]
# Reshape the data:
calories=as.data.frame(t(calories_raw[,7])) 
colnames(calories)=c(t(calories_raw[,2]))


# Calories by crop
# ------------------------------------------------------------------------------

# Load GAEZ yields Rdata:
load(file.path(data_folder, "Processed","gaez_df.Rdata"))

# - Low input case:
# -----------------

# Select the crops for which the caloric potential is known:
test1 = low_input_df %>% select('lon','lat', colnames(calories))
# Create the caloric yield:
for(crop in colnames(calories)){
  test1[,crop] = calories[,crop]*low_input_df[,crop]*10
}
# Select the maximum caloric yield:
test1$productive_calories = apply(test1[,-(1:2)], 1, max)   # '-(1:2)' excludes the first two columns which are the coords.
# Indicate which crop provides the maximum caloric yield:
test1$productive_crop = ifelse(test1$productive_calories==0,'zero',
                               colnames(calories)[apply(test1[,3:54],1,which.max)])
# Rename this column to indicate if the crop that provides the maximum caloric yield 
# is Maize, Rice, Wheat, Other, or Non-Productive.
test1 = test1 %>% mutate(index_low = 
                           ifelse(productive_crop == "hmze200b_yld"|productive_crop == "lmze200b_yld"|
                                    productive_crop == "maiz200b_yld"|productive_crop == "tmze200b_yld", "Maize",
                                  ifelse(productive_crop == "ricd200b_yld"|productive_crop == "ricw200b_yld","Rice",
                                         ifelse(productive_crop == "swhe200b_yld"|productive_crop == "whea200b_yld"|
                                                  productive_crop == "wwhe200b_yld","Wheat",
                                                ifelse(productive_crop=="zero","Non-Productive","Others")))))



# Plot the distribution of crops:
ggplot() +
  geom_raster(data = test1 , aes(x = lon, y = lat, fill = index_low)) +
  scale_fill_manual(values = c("Others"="#999999",
                               "Rice"= "#99FF66", 
                               "Wheat"="#FF6600",  
                               "Non-Productive"="#FFFFFF",
                               'Maize' = 'yellow')) + 
  ggtitle("Crop distribution under low input")

# Plot low-input caloric yield at each pixel:    
ggplot() +
  geom_raster(data = test1 , aes(x = lon, y = lat, fill = productive_calories)) + scale_fill_distiller(palette = "Spectral")


# - High input case:
# ------------------

# Initializing the dataset selecting the crops for which the caloric potential is known:
test2 = hi_input_df %>% select(lon,lat,colnames(calories))
# Calculate the caloric yield:
for(crop in colnames(calories)){
  test2[,crop] = calories[,crop]*hi_input_df[,crop]*10
}

# a) Assuming the crop stays the same.

# Joins test2 with the caloric yield data from test1 and computes 
# the high-input caloric yield for the crop that was determined to be 
# the most productive under the low-input scenario:
test2 = left_join(test2, test1 %>% select(lon,lat,productive_crop, productive_calories), by = c("lon","lat"))
test2$productive_calories_hi = apply(test2,1,function(x){x[x["productive_crop"]]})
test2[,57][is.na(test2[,57])]=0
test2[,57] = as.numeric(test2[,57])

# Plot high-input caloric yield at each pixel:
ggplot() +
  geom_raster(data = test2 , aes(x = lon, y = lat, fill = productive_calories_hi)) + scale_fill_distiller(palette = "Spectral")

# Difference in caloric yield between the high-input and low-input scenarios

test2$diff1 = test2$productive_calories_hi - test2$productive_calories

# Plot this difference:
ggplot() +
  geom_raster(data = test2 , aes(x = lon, y = lat, fill = diff1)) + scale_fill_distiller(palette = "Spectral")


# b0) Assuming the crop will change:

test20 = hi_input_df %>% select(lon,lat,colnames(calories))
# Calculate the caloric yield:
for(crop in colnames(calories)){
  test20[,crop] = calories[,crop]*hi_input_df[,crop]*10
}


# Select the maximum caloric yield:
test20$productive_calories = apply(test20[,-(1:2)], 1, max) 
# Indicate which crop provides the maximum caloric yield:
test20$productive_crop = ifelse(test20$productive_calories==0,'zero',
                               colnames(calories)[apply(test20[,3:54],1,which.max)])
# Rename this column to indicate if the crop that provides the maximum caloric yield 
# is Maize, Rice, Wheat, Other, or Non-Productive.
test20 = test20 %>% mutate(index_low = 
                           ifelse(productive_crop == "hmze200b_yld"|productive_crop == "lmze200b_yld"|
                                    productive_crop == "maiz200b_yld"|productive_crop == "tmze200b_yld", "Maize",
                                  ifelse(productive_crop == "ricd200b_yld"|productive_crop == "ricw200b_yld","Rice",
                                         ifelse(productive_crop == "swhe200b_yld"|productive_crop == "whea200b_yld"|
                                                  productive_crop == "wwhe200b_yld","Wheat",
                                                ifelse(productive_crop=="zero","Non-Productive","Others")))))



# Plot the distribution of crops:
ggplot() +
  geom_raster(data = test20 , aes(x = lon, y = lat, fill = index_low)) +
  scale_fill_manual(values = c("Others"="#999999",
                               "Rice"= "#99FF66", 
                               "Wheat"="#FF6600",  
                               "Non-Productive"="#FFFFFF",
                               'Maize' = 'yellow')) + 
  ggtitle("Crop distribution under high input")



# b) Assuming the crop can change:

# Select the maximum caloric yield:
test2$tentative_max = apply(test2[,3:54],1,max)
test2$max = ifelse(test2$tentative_max<test2$productive_calories, test2$productive_calories,test2$tentative_max)
test2$diff2 = test2$max-test2$productive_calories
test2$crop = ifelse(test2$max==0,'zero',
                    ifelse(test2$tentative_max==0, test2$productive_crop,colnames(calories)[apply(test2[,3:54],1,which.max)]))
test2 = test2 %>% mutate(index_hi = 
                           ifelse(crop == "hmze200b_yld"|crop == "lmze200b_yld"|
                                    crop == "maiz200b_yld"|crop == "tmze200b_yld","Maize",
                                  ifelse(crop == "ricd200b_yld"|crop == "ricw200b_yld","Rice",
                                         ifelse(crop == "swhe200b_yld"|crop == "whea200b_yld"|
                                                  crop == "wwhe200b_yld","Wheat",
                                                ifelse(crop=="zero","Non-Productive","Others")))))

# Plot the distribution of crops:

ggplot() +
  geom_raster(data = test2 , aes(x = lon, y = lat, fill = index_hi)) +
  scale_fill_manual(values = c("Others"="#999999", 
                               "Rice"= "#99FF66", 
                               "Wheat"="#FF6600",  
                               "Non-Productive"="#FFFFFF",
                               'Maize' = 'yellow')) + 
  ggtitle("Crop distribution under high input (switching)")


# I'm going to copy here all the next steps Wei's code did to process the data
# Think later is those were optimum:

test2 = test2 %>% mutate(interval_low = ifelse(productive_calories<56000, "0",
                                               ifelse(productive_calories<3400000, "56,001-3,400,000",
                                                      ifelse(productive_calories<6900000, "3,400,001-6,800,000",
                                                             ifelse(productive_calories<9600000, "6,800,001-9,600,000", "above 9,600,000")))))

test2 = test2 %>% mutate(interval_hi = ifelse(productive_calories_hi<56000, "0",
                                              ifelse(productive_calories_hi<3400000, "56,001-3,400,000",
                                                     ifelse(productive_calories_hi<6900000, "3,400,001-6,800,000",
                                                            ifelse(productive_calories_hi<9600000, "6,800,001-9,600,000", "above 9,600,000")))))

test2 = test2 %>% mutate(interval_hi_unfixed = ifelse(max<56000, "0-56,000",
                                                      ifelse(max<3400000, "56,001-3,400,000",
                                                             ifelse(max<6900000, "3,400,001-6,800,000",
                                                                    ifelse(max<9600000, "6,800,001-9,600,000", "above 9,600,000")))))

test2 = test2 %>% mutate(interval_diff_fix = ifelse(diff1<=0, "0 and below", 
                                                    ifelse(diff1<=1500000, "1-1,500,000",
                                                           ifelse(diff1<=14000000, "1,500,001-14,000,000", "above 14,000,000"))))

test2 = test2 %>% mutate(interval_diff_unfixed = ifelse(diff2<=0, "0 and below", 
                                                        ifelse(diff2<=1500000, "1-1,500,000",
                                                               ifelse(diff2<=14000000, "1,500,001-14,000,000", "above 14,000,000"))))

# Saving the objects we are going to need for later analysis:
# -----------------------------------------------------------------------------
save(file = file.path(data_folder, "Processed","caloric_potential.Rdata"), 
     list = c("test1","test2","test20"))
