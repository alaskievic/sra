# Load packages
source("00_load_packages.R")

############### 1. Clean and merge data from D-Place ##############
ea_var <- read_csv(here("data", "raw", "murdock_ea", "variables.csv")) %>%
  rename("var_id" = "id")
ea_coding <- read_csv(here("data", "raw", "murdock_ea", "codes.csv"))

ea_societ <- read_csv(here("data", "raw", "murdock_ea", "societies.csv")) %>%
  rename("soc_id" = "id", "orig_name" = "ORIG_name_and_ID_in_this_dataset", 
         "alt_name" = "alt_names_by_society", 
         "pref_name" = "pref_name_for_society") %>% 
  dplyr::select(-c("xd_id", "HRAF_name_ID", "HRAF_link", "Comment", 
                   "glottocode_comment"))


ea_data <- read_csv(here("data", "raw", "murdock_ea", "data.csv")) %>%
  dplyr::select(-c("comment", "sub_case", "references", "source_coded_data", 
                    "admin_comment"))


# Merging
ea_def <- full_join(ea_coding, ea_var, by = "var_id")
ea_def_tomerge <- dplyr::select(ea_def, c("var_id", "code", "description", 
                                          "name", "title")) %>%
  rename("var_name" = "name")

ea_dplace <- full_join(ea_data, ea_societ, by = "soc_id")
ea_dplace %<>% full_join(., ea_def_tomerge, by = c("var_id", "code")) %>%
  dplyr::select(-c("year"))
  
# Saving
save(ea_def, file = here("data", "output", "ea_def.Rda"))
save(ea_dplace, file = here("data", "output", "ea_dplace.Rda"))

