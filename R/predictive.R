# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)




# Data Import and Cleaning
finalproj_tbl <- readRDS("../data/finalproj_data.rds")




# Analysis

# This code randomizes the order of the data, then splits the data into training 
# and testing sets with a 50/50 split, and creates training folds
finalproj_shuffled_tbl <- finalproj_tbl[sample(nrow(finalproj_tbl)),]
split50 <- round(nrow(finalproj_shuffled_tbl) * .50)
finalproj_train_tbl <- finalproj_shuffled_tbl[1:split50,]
finalproj_test_tbl <- finalproj_shuffled_tbl[(split50 + 1):nrow(finalproj_shuffled_tbl),]
training_folds <- createFolds(finalproj_train_tbl$Attrition, 10)


# The following code sets up parallelization, runs the same models again, and times them
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)











#Stop parallelization
stopCluster(local_cluster)
registerDoSEQ()











