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

#This tidy tibble of the results is used later in the Publication section
H3_tbl <- tidy(H3_list)

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

# This code makes a publication tibble by pulling results out of the analysis tibble
# and formatting it for publication, with descriptive labels and appropriate trailing
# and leading zeros.
H1_publication_tbl <- tibble(
  "Correlation" = str_remove(format(round(H1_tbl$cor, 2), nsmall = 2), pattern = "0"),
  "Test Statistic" = format(round(H1_tbl$statistic, 2), nsmall = 2),
  "p-Value" = str_remove(format(round(H1_tbl$p, 2), nsmall = 2), pattern = "0")
)
H1_publication_tbl

# This code dynamically pulls numbers from the publication table
# in interpreting the results.
paste0("For Hypothesis 1, the correlation between monthly pay and performance rating was ", H1_publication_tbl$Correlation,
       ", with a test statistic of ", H1_publication_tbl$`Test Statistic`, " corresponding to a p-value of ", H1_publication_tbl$`p-Value`,
       ". As this value is above the alpha level, we would fail to reject the null hypothesis that there is no relationship between monthly pay and performance rating.")

# This code write the publication table to a csv and saves it in the out folder.
write_csv(H1_publication_tbl, "../out/H1.csv")


# Publication results for H2

# This code makes a publication tibble by pulling results out of the analysis tibble
# and formatting it for publication, with descriptive labels and appropriate trailing
# and leading zeros.
H2_publication_tbl <- tibble(
  "Source of Variation" = c("Within", "Between", "Total"),
  "Sum of Squares" = c(
    H2_anova_tbl$SSn,
    H2_anova_tbl$SSd,
    (H2_anova_tbl$SSn + H2_anova_tbl$SSd)),
  "Degrees of Freedom" = c(
    H2_anova_tbl$DFn,
    H2_anova_tbl$DFd,
    nrow(finalproj_stats_tbl) - 1),
  "Mean Squares" = c(
    H2_anova_tbl$SSn / H2_anova_tbl$DFn,
    H2_anova_tbl$SSd / H2_anova_tbl$DFd,
    NA),
  "F-Statistic" = c(
    format(round(H2_anova_tbl$F, 2), nsmall = 2),
    NA,
    NA),
  "p-Value" = c(
    str_remove(format(round(H2_anova_tbl$p, 2), nsmall = 2), pattern = "0"),
    NA,
    NA)
  )
H2_publication_tbl

# This code dynamically pulls numbers from the publication table and supplementary analysis table
# in interpreting the results.
paste0("For Hypothesis 2, the ANOVA for pay differing between departments produced an F test statistic of ", H2_publication_tbl$`F-Statistic`[1],
       " corresponding to a p-Value of ", H2_publication_tbl$`p-Value`[1], 
       ". As this result was above the alpha level, we would reject the null hypothesis and conclude that there is a significant difference in pay between departments. A post-hoc TukeyHSD test revealed that the only significant difference in pay was between the Sales and Research & Development departments, with a p-Value of ", 
       str_remove(format(round(H2_tukeyhsd_tbl$p.adj[1], 2), nsmall = 2), pattern = "0"), ".")

# This code write the publication table to a csv and saves it in the out folder.
write_csv(H2_publication_tbl, "../out/H2.csv")


# Publication Results for H3

# This code makes a publication tibble by pulling results out of the analysis tibble
# and formatting it for publication, with descriptive labels and appropriate trailing
# and leading zeros.
H3_publication_tbl <- tibble(
  "Coefficient" = c("(Intercept)", "Relationship Satisfaction (RS)", "Gender", "RS X Gender"),
  "t-Statistic" = format(round(H3_tbl$statistic, 2), nsmall = 2),
  "p-Value" = str_remove(format(round(H3_tbl$p.value, 2), nsmall = 2), pattern = "0")
)
H3_publication_tbl

# This code dynamically pulls numbers from the publication table
# in interpreting the results.
paste0("For Hypothesis 3, the linear model predicting tenure from relationship satisfaction with gender as a moderator results in a significant result only for the intercept coefficient, with a t-statistics of ",
       H3_publication_tbl$`t-Statistic`[1], " corresponding to a p-value of ", H3_publication_tbl$`p-Value`[1], 
       ", which is below the alpha level. As the other coefficients do not have significant results, we would fail to reject the null hypothesis that relationship satisfaction moderated by gender can predict tenure.")

# This code write the publication table to a csv and saves it in the out folder.
write_csv(H3_publication_tbl, "../out/H3.csv")



