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
H3_list <- summary(H3_model)
H3_list






# Visualization

# Visualization of H1
H1_plot <- ggplot(finalproj_stats_tbl, aes(x = PerformanceRating, y = MonthlyIncome)) + 
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Performance Rating", y = "Monthly Income")

H1_plot

ggsave("../figs/H1.png", plot = H1_plot, width = 8, height = 4.5, units = "in", dpi = 300)

# Visualization of H2
H2_plot <- ggplot(finalproj_stats_tbl, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(y = "Monthly Income")

H2_plot

ggsave("../figs/H2.png", plot = H2_plot, width = 8, height = 4.5, units = "in", dpi = 300)

# Visualization of H3
H3_plot <- ggplot(finalproj_stats_tbl, aes(x = predict(H3_model), y = RelationshipSatisfaction, color = Gender, group = Gender)) +
  geom_point(size = 0.5, position = position_jitter()) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Predicted Years at Company", y = "Relationship Satisfaction")

H3_plot

ggsave("../figs/H3.png", plot = H3_plot, width = 8, height = 4.5, units = "in", dpi = 300)




# Publication

# Publication Results for H1






