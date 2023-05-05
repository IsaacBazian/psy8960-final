# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)




# Data Import and Cleaning
finalproj_tbl <- readRDS("../data/finalproj_data.rds") %>% 
  select(- good_here, - bad_here, - Over18) #MUST ADD SATISFACTION NLP LATER, TESTING ML FIRST




# Analysis

# This code randomizes the order of the data, then splits the data into training 
# and testing sets with a 50/50 split, and creates training folds
finalproj_shuffled_tbl <- finalproj_tbl[sample(nrow(finalproj_tbl)),]
split50 <- round(nrow(finalproj_shuffled_tbl) * .50)
finalproj_train_tbl <- finalproj_shuffled_tbl[1:split50,]
finalproj_test_tbl <- finalproj_shuffled_tbl[(split50 + 1):nrow(finalproj_shuffled_tbl),]
training_folds <- createFolds(finalproj_train_tbl$Attrition, 10)


# The following code sets up parallelization
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)


modelElasticNet <- train(
  Attrition ~ .,
  finalproj_train_tbl,
  method = "glmnet",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T),
  tuneLength = 3
)



modelRandomForest <- train(
  Attrition ~ .,
  finalproj_train_tbl,
  method = "ranger",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T),
  tuneLength = 3
)



modelXGB <- train(
  Attrition ~ .,
  finalproj_train_tbl,
  method = "xgbLinear",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = training_folds, number = 10, search = "grid", verboseIter=T),
  tuneLength = 3
)


#Stop parallelization
stopCluster(local_cluster)
registerDoSEQ()



# Publication

# modelElasticNet$results$Accuracy
# modelRandomForest$results$Accuracy
# modelXGB$results$Accuracy
# 
# mean(predict(modelElasticNet, finalproj_test_tbl) == finalproj_test_tbl$Attrition)
# mean(predict(modelRandomForest, finalproj_test_tbl) == finalproj_test_tbl$Attrition)
# mean(predict(modelXGB, finalproj_test_tbl) == finalproj_test_tbl$Attrition)
# 
# table(predict(modelElasticNet, finalproj_test_tbl), finalproj_test_tbl$Attrition)
# table(predict(modelRandomForest, finalproj_test_tbl), finalproj_test_tbl$Attrition)
# table(predict(modelXGB, finalproj_test_tbl), finalproj_test_tbl$Attrition)

modelcomparison_tbl <- tibble(
  algo = c("Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_accuracy = c(
    max(modelElasticNet$results$Accuracy),
    max(modelRandomForest$results$Accuracy),
    max(modelXGB$results$Accuracy)
  ),
  ho_accuracy = c(
    mean(predict(modelElasticNet, finalproj_test_tbl) == finalproj_test_tbl$Attrition),
    mean(predict(modelRandomForest, finalproj_test_tbl) == finalproj_test_tbl$Attrition),
    mean(predict(modelXGB, finalproj_test_tbl) == finalproj_test_tbl$Attrition)
  )
)









