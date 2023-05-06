# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
finalproj_stats_tbl <- readRDS("../data/finalproj_data.rds")


# Analysis

H1_tbl <- finalproj_stats_tbl %>% 
  cor_test("MonthlyIncome", "PerformanceRating", alternative = "two.sided", method = "pearson", conf.level = 0.95) #All defaults, but it informs reader













# Visualization

ggplot(finalproj_stats_tbl, aes(x = PerformanceRating, y = MonthlyIncome)) + 
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm", se = FALSE)



