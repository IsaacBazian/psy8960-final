# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Data Import and Cleaning

employee_data_tbl <- read_delim(file = "../data/dataset.csv", delim = "+") %>% 
  mutate(employee_id = row_number())

satisfaction_tbl <- read_delim(file = "../data/satisfaction_reviews.csv", delim = "@!?", col_names = "satisfaction") %>% 
  mutate(employee_id = as.integer(str_extract(string = satisfaction, pattern = "(\\d+)$"))) %>% 
  mutate(satisfaction = str_remove(string = satisfaction, pattern = "(\\d+)$"))



