# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning

# This code reads in the data from dataset.csv. Looking at that file made it clear
# that values were separated by '+', so we set that value. We were also told that
# the rows were in order of employee IDs, from 1 to 1470, so we added a column
# with each row number to act as the IDs.
employee_data_tbl <- read_delim(file = "../data/dataset.csv", delim = "+") %>% 
  mutate(employee_id = row_number())

# This code reads in the data from satisfaction_reviews.csv. The initial file had
# some very strange formatting, with data on different numbers of columns across
# different rows, so I thought it would be best to just get each row into one
# column for consistent, so I set the delimiter to something very specific found
# nowhere in the data to get all values into one column. I also noticed that
# the employee IDs were always at the very end of each row, so I wrote some regex
# to grab those numbers from each string and put them in their own column, and remove
# that pattern from the character strings as they were now better stored somewhere else.
# I also noticed that some string had "NA." entered instead of blank fields, so
# I wrote some more regex to delete them and converted empty strings to NAs.
satisfaction_tbl <- read_delim(file = "../data/satisfaction_reviews.csv", delim = "@!?", col_names = "satisfaction") %>% 
  mutate(employee_id = as.integer(str_extract(string = satisfaction, pattern = "(\\d+)$"))) %>% 
  mutate(satisfaction = str_remove(string = satisfaction, pattern = "(\\d+)$")) %>% 
  mutate(satisfaction = str_remove_all(string = satisfaction, pattern = "NA\\.")) %>% 
  mutate(satisfaction = na_if(satisfaction, ""))

# This code joins the two previous tibbles on their shared employee_id columns,
# then saves the object as an RDS file so it can be used in future parts.
finalproj_tbl <- left_join(employee_data_tbl, satisfaction_tbl, join_by(employee_id))

saveRDS(finalproj_tbl, "../data/finalproj_data.rds")

