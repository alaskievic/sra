# Load packages
source("00_load_packages.R")


####################### 1. Prepare DHS Sample for Stata ########################

# Women #
load(file = here("data", "output", "dhs", "women_dhs_murdock.RData"))

# Keep only relevant variables #
women_dhs_murdock %<>% dplyr::select(idhspid,DHSYEAR, URBAN_RURA, map_name, ea_name, match_name,
                                     LAT, LON, max_caloric_low, max_caloric_high,
                                     caloric_diff)


# Read original DHS and select only relevant variables #
women_dhs <- read_dta(file = here("data", "raw", "dhs", "women_sample_1.dta")) %>%
  dplyr::select(sample, idhspid, idhshid, dhsid, country, year, age, religion, homelangao,
                marstat, currwork, wkcurrjob, wealths, edyrtotal)

# Keep only latest year #
women_dhs %<>% group_by(country) %>% slice_max(year)


women_dhs %<>% inner_join(women_dhs_murdock, ., by = "idhspid")

stata_women <- filter(women_dhs, wkcurrjob == 30)

# Number of women in agriculture
stata_women %<>% group_by(map_name) %>% add_tally(name = "num_wom_agri") %>%
  distinct(map_name, .keep_all = TRUE) %>%
  dplyr::select(map_name, num_wom_agri, LAT, LON, max_caloric_high,
                max_caloric_low, caloric_diff)


# Men #
load(file = here("data", "output", "dhs", "men_dhs_murdock.RData"))

# Keep only relevant variables #
men_dhs_murdock %<>% dplyr::select(idhspid,DHSYEAR, URBAN_RURA, map_name, ea_name, match_name,
                                     LAT, LON, max_caloric_low, max_caloric_high,
                                     caloric_diff)


# Read original DHS and select only relevant variables #
men_dhs <- read_dta(file = here("data", "raw", "dhs", "men_sample_2.dta")) %>%
  dplyr::select(sample, idhspid, idhshid, dhsid, country, year, wkcurrjobmn)

# Keep only latest year #
men_dhs %<>% group_by(country) %>% slice_max(year)

men_dhs %<>% inner_join(men_dhs_murdock, ., by = "idhspid")

stata_men <- filter(men_dhs, wkcurrjobmn == 30)

# Number of women in agriculture
stata_men %<>% group_by(map_name) %>% add_tally(name = "num_men_agri") %>%
  distinct(map_name, .keep_all = TRUE) %>%
  dplyr::select(map_name, num_men_agri)


## Append both ##
stata_march4 <- inner_join(stata_women, stata_men, by = "map_name") %>%
  mutate(num_agri = num_men_agri + num_wom_agri,
         share_wom_agri = num_wom_agri/num_agri)


## Add Relevant EA variables ##
load(file = here("data", "output", "ea_dplace.Rda"))
load(file = here("data", "output", "crosswalk_ea_map.Rda"))

ea_dplace %<>% filter(var_id == "EA005" | var_id == "EA009" | var_id == "EA054") %>%
               dplyr::select(var_id, soc_id,  code) %>%
  pivot_wider(., id_cols = "soc_id", names_from = "var_id", values_from = "code")

ea_dplace %<>% inner_join(., crosswalk_ea_map, by = "soc_id") %>%
  dplyr::select(soc_id, EA005, EA009, EA054, map_name, ea_name)

stata_march4 %<>% inner_join(., ea_dplace, by = "map_name")

write_dta(stata_march4, path = here("data", "output", "dhs", "stata_march4.dta"))


