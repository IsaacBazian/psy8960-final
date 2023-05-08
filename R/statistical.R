# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
finalproj_stats_tbl <- readRDS("../data/finalproj_data.rds")


# Analysis

# Test of Hypothesis 1
# This code runs a correlation test to see if there is a significant relationship
# between MonthlyIncome and PerformanceRating, and saves the results to an object
# for later use in the Publication section
H1_tbl <- finalproj_stats_tbl %>% 
  cor_test("MonthlyIncome", "PerformanceRating", alternative = "two.sided", method = "pearson", conf.level = 0.95) #All defaults, but it informs reader
H1_tbl


# Test of Hypothesis 2
# This code runs an ANOVA to see if pay varies significantly between departments,
# and saves the results to an object for later use in the Publication section
H2_anova_tbl <- finalproj_stats_tbl %>%
  anova_test(MonthlyIncome ~ Department, detailed = T)
H2_anova_tbl

# Since the ANOVA is significant, we also run a Tukey HSD post-hoc tests to see
# which departments significantly differ from each other in pay. The results are
# saved to an object for later use in the Publication section.
H2_tukeyhsd_tbl <- finalproj_stats_tbl %>% 
  tukey_hsd(MonthlyIncome ~ Department)
H2_tukeyhsd_tbl


# Test of Hypothesis 3
# This code creates a liner regression model predicting tenure from relationship
# satisfaction, with gender as a moderator. The summary of results are saved for 
# later use in the Publication section.
H3_model <- lm(YearsAtCompany ~ RelationshipSatisfaction*Gender, data = finalproj_stats_tbl)
H3_list <- summary(H3_model)
H3_list



# Visualization

# Visualization of H1
# This code makes a scatter plot of the relationship between monthly pay and performance
# rating, including a line of best fit. Points are jittered to make density more visible.
# Plot is displayed and saved to the figs folder in publication-ready quality.
H1_plot <- ggplot(finalproj_stats_tbl, aes(x = PerformanceRating, y = MonthlyIncome)) + 
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Performance Rating", y = "Monthly Income", title = "Relationship Between Monthly Pay and Performance Rating")

H1_plot

ggsave("../figs/H1.png", plot = H1_plot, width = 8, height = 4.5, units = "in", dpi = 300)

# Visualization of H2
# This code makes a boxplot displaying monthly pay in each department.
# Plot is displayed and saved to the figs folder in publication-ready quality.
H2_plot <- ggplot(finalproj_stats_tbl, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(y = "Monthly Income", title = "Monthly Pay Between Departments")

H2_plot

ggsave("../figs/H2.png", plot = H2_plot, width = 8, height = 4.5, units = "in", dpi = 300)

# Visualization of H3
# This code makes a scatter plot of the relationship between model-predicted tenure
# and relationship satisfaction, moderated by gender, with a line of best fit for
# for gender group. Points are jittered to make density more visible.
# Plot is displayed and saved to the figs folder in publication-ready quality.
H3_plot <- ggplot(finalproj_stats_tbl, aes(y = predict(H3_model), x = RelationshipSatisfaction, color = Gender, group = Gender)) +
  geom_point(size = 0.5, position = position_jitter()) +
  geom_smooth(method = "lm", se = F) +
  labs(y = "Predicted Years at Company", x = "Relationship Satisfaction", title = "Relationship Between Predicted Tenure and Relationship Satisfaction \nas Moderated by Gender")

H3_plot

ggsave("../figs/H3.png", plot = H3_plot, width = 8, height = 4.5, units = "in", dpi = 300)


# Publication

# Publication Results for H1






