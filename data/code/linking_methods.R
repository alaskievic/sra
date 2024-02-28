# Load packages
source("00_load_packages.R")



library(devtools)
install_github(repo = "carl-mc/LEDA")


library(LEDA)
leda <- LEDA$new()

# Retrieve dataset dictionary
list.dict <- leda$get_list_dict()


data <- read.table(file = here("data", "raw", "ethnologue", "LanguageCodes.tab"),
                   header = F, sep = "\t", fill = TRUE)

country <- read.table(file = here("data", "raw", "ethnologue", "CountryCodes.tab"),
                      header = F, sep = "\t", fill = TRUE)

index <- read.table(file = here("data", "raw", "ethnologue", "LanguageIndex.tab"),
                    header = F, sep = "\t", fill = TRUE)

