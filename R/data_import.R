# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning

# This code reads in the data from dataset.csv. Looking at that file made it clear
# that values were separated by '+', so we set that value. We were also told that
# the rows were in order of employee IDs, from 1 to 1470, so we added a column
# with each row number to act as the IDs.
employee_data_tbl <- read_delim(file = "../data/dataset.csv", delim = "+", col_types = "iffifiifiifiiififiiiffiiiiiiiiiiii") %>% #Things that seem reasonably represented as continuous are integers, everything else is a factor
  mutate(employee_id = row_number())

# This code reads in the data from satisfaction_reviews.csv. The initial file had
# some very strange formatting, but it turns out setting "." as the delimiter works
# just fine for separating into the three variables.
satisfaction_tbl <- read_delim(file = "../data/satisfaction_reviews.csv", delim = ".", col_names = c("good_here", "bad_here", "employee_id"))

# This code joins the two previous tibbles on their shared employee_id columns,
# then saves the object as an RDS file so it can be used in future parts.
finalproj_tbl <- left_join(employee_data_tbl, satisfaction_tbl, join_by(employee_id))

saveRDS(finalproj_tbl, "../data/finalproj_data.rds")

