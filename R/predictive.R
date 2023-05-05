# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)
library(tm)
library(qdap)
library(textstem)


# Data Import and Cleaning
finalproj_without_satisfaction_tbl <- readRDS("../data/finalproj_data.rds") %>% 
  select(- good_here, - bad_here, - Over18) #Over18 has no variance, and is therefore a constant, not a true variable. Provides no information for prediction.


satisfaction_tbl <- readRDS("../data/finalproj_data.rds") %>% 
  select(good_here, bad_here) %>% 
  unite("satisfaction", good_here:bad_here, sep = " ", na.rm = T) %>% 
  mutate(across("satisfaction", str_replace_all, "-|/", " "))

satisfaction_corpus_original <- VCorpus(VectorSource(satisfaction_tbl$satisfaction))

satisfaction_corpus_with_empty <- satisfaction_corpus_original %>% 
  tm_map(content_transformer(replace_abbreviation)) %>% 
  tm_map(content_transformer(replace_contraction)) %>% 
  tm_map(content_transformer(replace_symbol)) %>%
  tm_map(content_transformer(str_to_lower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, c(stopwords("en"))) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(lemmatize_strings))


compare_them <- function(corp1, corp2) {
  rows <- 1:length(corp1)
  pick <- sample(rows, 1)
  corp_list <- list("Row" = pick, Original = content(corp1[[pick]]), "Preprocessed" = content(corp2[[pick]]))
  return(corp_list)
}

compare_them(satisfaction_corpus_original, satisfaction_corpus_with_empty)

satisfaction_dtm <- DocumentTermMatrix(satisfaction_corpus_with_empty)
satisfaction_slim_dtm <- removeSparseTerms(satisfaction_dtm, .996) # n/k ratio is 1470/638, in desired range
# satisfaction_dtm
# satisfaction_slim_dtm

satisfaction_token_tbl <- as_tibble(as.matrix(satisfaction_slim_dtm)) %>% 
  mutate(employee_id = row_number())

finalproj_tbl <- left_join(finalproj_without_satisfaction_tbl, satisfaction_token_tbl, join_by(employee_id))


# Analysis

# This code randomizes the order of the data, then splits the data into training 
# and testing sets with a 50/50 split, and creates training folds
finalproj_shuffled_tbl <- finalproj_tbl[sample(nrow(finalproj_tbl)),]
split <- round(nrow(finalproj_shuffled_tbl) * .50)
finalproj_train_tbl <- finalproj_shuffled_tbl[1:split,]
finalproj_test_tbl <- finalproj_shuffled_tbl[(split + 1):nrow(finalproj_shuffled_tbl),]
training_folds_full <- createFolds(finalproj_train_tbl$Attrition, 10)

finalproj_without_satisfaction_shuffled_tbl <- finalproj_without_satisfaction_tbl[sample(nrow(finalproj_without_satisfaction_tbl)),]
#split75 <- round(nrow(finalproj_without_satisfaction_shuffled_tbl) * .75)
finalproj_without_satisfaction_train_tbl <- finalproj_without_satisfaction_shuffled_tbl[1:split,]
finalproj_without_satisfaction_test_tbl <- finalproj_without_satisfaction_shuffled_tbl[(split + 1):nrow(finalproj_without_satisfaction_shuffled_tbl),]
training_folds_without_satisfaction <- createFolds(finalproj_without_satisfaction_train_tbl$Attrition, 10)

presettuneLength <- 4


# The following code sets up parallelization
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

#Tested models with satisfaction words

tic()
modelElasticNet <- train(
  Attrition ~ .,
  finalproj_train_tbl,
  method = "glmnet",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = c("nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = training_folds_full, number = 10, search = "grid", verboseIter=T),
  tuneLength = presettuneLength
)
toc()

tic()
modelRandomForest <- train(
  Attrition ~ .,
  finalproj_train_tbl,
  method = "ranger",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = c("nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = training_folds_full, number = 10, search = "grid", verboseIter=T),
  tuneLength = presettuneLength
)
toc()

tic()
modelXGB <- train(
  Attrition ~ .,
  finalproj_train_tbl,
  method = "xgbLinear",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = c("nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = training_folds_full, number = 10, search = "grid", verboseIter=T),
  tuneLength = presettuneLength
)
toc()

# Models without satisfaction words

tic()
modelElasticNetWithoutSatisfaction <- train(
  Attrition ~ .,
  finalproj_without_satisfaction_train_tbl,
  method = "glmnet",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = c("nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = training_folds_without_satisfaction, number = 10, search = "grid", verboseIter=T),
  tuneLength = presettuneLength
)
toc()

tic()
modelRandomForestWithoutSatisfaction <- train(
  Attrition ~ .,
  finalproj_without_satisfaction_train_tbl,
  method = "ranger",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = c("nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = training_folds_without_satisfaction, number = 10, search = "grid", verboseIter=T),
  tuneLength = presettuneLength
)
toc()

tic()
modelXGBWithoutSatisfaction <- train(
  Attrition ~ .,
  finalproj_without_satisfaction_train_tbl,
  method = "xgbLinear",
  metric = "Accuracy",
  na.action = na.pass,
  preProcess = c("nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = training_folds_without_satisfaction, number = 10, search = "grid", verboseIter=T),
  tuneLength = presettuneLength
)
toc()

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

modelcomparison_without_satisfaction_tbl <- tibble(
  algo = c("Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_accuracy = c(
    max(modelElasticNetWithoutSatisfaction$results$Accuracy),
    max(modelRandomForestWithoutSatisfaction$results$Accuracy),
    max(modelXGBWithoutSatisfaction$results$Accuracy)
  ),
  ho_accuracy = c(
    mean(predict(modelElasticNetWithoutSatisfaction, finalproj_without_satisfaction_test_tbl) == finalproj_without_satisfaction_test_tbl$Attrition),
    mean(predict(modelRandomForestWithoutSatisfaction, finalproj_without_satisfaction_test_tbl) == finalproj_without_satisfaction_test_tbl$Attrition),
    mean(predict(modelXGBWithoutSatisfaction, finalproj_without_satisfaction_test_tbl) == finalproj_without_satisfaction_test_tbl$Attrition)
  )
)



# max(modelXGBWithoutSatisfaction$results$Accuracy)
# mean(predict(modelXGBWithoutSatisfaction, finalproj_without_satisfaction_test_tbl) == finalproj_without_satisfaction_test_tbl$Attrition)

# Combine tables, add run time, probably will choose Elastic Net or XGB




