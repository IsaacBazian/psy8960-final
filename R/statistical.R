# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
finalproj_stats_tbl <- readRDS("../data/finalproj_data.rds")


# Analysis

# Hypothesis 1
H1_tbl <- finalproj_stats_tbl %>% 
  cor_test("MonthlyIncome", "PerformanceRating", alternative = "two.sided", method = "pearson", conf.level = 0.95) #All defaults, but it informs reader



# Hypothesis 2

H2_anova_tbl <- finalproj_stats_tbl %>%
  anova_test(MonthlyIncome ~ Department, detailed = T)

H2_tukeyhsd_tbl <- finalproj_stats_tbl %>% 
  tukey_hsd(MonthlyIncome ~ Department)


# Hypothesis 3

H3_model <- lm(YearsAtCompany ~ RelationshipSatisfaction*Gender, data = finalproj_stats_tbl)
summary(H3_model)







# Visualization

# Visualization of H1
H1_plot <- ggplot(finalproj_stats_tbl, aes(x = PerformanceRating, y = MonthlyIncome)) + 
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Performance Rating", y = "Monthly Income")

H1_plot



# Visualization of H2
ggplot(finalproj_stats_tbl, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(y = "Monthly Income")



# Visualization of H3
ggplot(finalproj_stats_tbl, aes(x = predict(H3_model), y = RelationshipSatisfaction, color = Gender, group = Gender)) +
  geom_point(position = position_jitter()) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Predicted Years at Company", y = "Relationship Satisfaction")







