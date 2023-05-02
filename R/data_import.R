# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Data Import and Cleaning

employee_data_tbl <- read_delim(file = "../data/dataset.csv", delim = "+") %>% 
  mutate(employee_id = row_number())

satisfaction_tbl <- read_delim(file = "../data/satisfaction_reviews.csv", delim = "@!?", col_names = "satisfaction") %>% 
  mutate(employee_id = as.integer(str_extract(string = satisfaction, pattern = "(\\d+)$"))) %>% 
  mutate(satisfaction = str_remove(string = satisfaction, pattern = "(\\d+)$")) %>% 
  mutate(satisfaction = str_remove_all(string = satisfaction, pattern = "NA\\.")) %>% 
  mutate(satisfaction = na_if(satisfaction, ""))

finalproj_tbl <- left_join(employee_data_tbl, satisfaction_tbl, join_by(employee_id))

saveRDS(finalproj_tbl, "../data/finalproj_data.rds")

