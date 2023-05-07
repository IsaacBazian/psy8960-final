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

# This code reads in the data from the rds we saved in data_import.R. 
# We get rid of the satisfaction text data because we'll be treating that separately first.
# We can remove Over18 because it has no variance, and is therefore a constant, 
# not a true variable. It provides no information for prediction. There are also
# two other variables, EmployeeCount and StandardHours, also have no variance
# and so could be removed in this way, but they're numeric rather than factors,
# so the model training still runs and they're just not considered due to telling
# the train function to get rid of zero and near zero variance variables.
finalproj_without_satisfaction_tbl <- readRDS("../data/finalproj_data.rds") %>% 
  select(- good_here, - bad_here, - Over18)

# This code reads in the satisfaction test data and unites it into one variable,
# which makes it easier to turn it into a 'bag of words' for natural language
# processing. We also remove - and / here because otherwise words separated by them
# can end up looking like a single word, which can mess with NLP.
satisfaction_tbl <- readRDS("../data/finalproj_data.rds") %>% 
  select(good_here, bad_here) %>% 
  unite("satisfaction", good_here:bad_here, sep = " ", na.rm = T) %>% 
  mutate(across("satisfaction", str_replace_all, "-|/", " "))

# We turn the satisfaction text into a corpus
satisfaction_corpus_original <- VCorpus(VectorSource(satisfaction_tbl$satisfaction))

# We apply preprocessing steps to that corpus to clean it up. We replace abbreviations
# and contractions into distinct words for consistency across the corpus, and replace
# symbols with the words they represent (saw some pluses and percents). 
# We turn all letters to lowercase and
# remove punctuation for consistency. Now the corpus is at its highest number of words
# and we start to trim it down - we remove standard english stopwords and strip excess
# whitespace. Finally, we lemmatize the strings so that words with the same essential
# meaning will be considered as one word.
satisfaction_corpus_with_empty <- satisfaction_corpus_original %>% 
  tm_map(content_transformer(replace_abbreviation)) %>% 
  tm_map(content_transformer(replace_contraction)) %>% 
  tm_map(content_transformer(replace_symbol)) %>%
  tm_map(content_transformer(str_to_lower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, c(stopwords("en"))) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(lemmatize_strings))


# This code was used to make sure the preprocessing went as intended by comparing
# the statements before and after preprocessing. Everything looked good. This code
# has now been commented out.
# compare_them <- function(corp1, corp2) {
#   rows <- 1:length(corp1)
#   pick <- sample(rows, 1)
#   corp_list <- list("Row" = pick, Original = content(corp1[[pick]]), "Preprocessed" = content(corp2[[pick]]))
#   return(corp_list)
# }
# 
# compare_them(satisfaction_corpus_original, satisfaction_corpus_with_empty)

# This code turned the corpus into a DTM with unigrams. Unigrams were used to save on
# processing time. This DTM was then trimmed down to get the n/k ratio to the desired
# range of between 2/1 and 3/1
satisfaction_dtm <- DocumentTermMatrix(satisfaction_corpus_with_empty)
satisfaction_slim_dtm <- removeSparseTerms(satisfaction_dtm, .996) # n/k ratio is 1470/638, in desired range
# satisfaction_dtm
# satisfaction_slim_dtm

# This code turns the DTM of satisfaction text into a tibble and adds employee_id for joining
satisfaction_token_tbl <- as_tibble(as.matrix(satisfaction_slim_dtm)) %>% 
  mutate(employee_id = row_number())

# This code merges the original employee data with the now-processed satisfaction
# text by the shared employee_id
finalproj_tbl <- left_join(finalproj_without_satisfaction_tbl, satisfaction_token_tbl, join_by(employee_id))


# Analysis

# This code randomizes the order of the data, then splits the data into training 
# and testing sets with a 50/50 split, and creates training folds
finalproj_shuffled_tbl <- finalproj_tbl[sample(nrow(finalproj_tbl)),]
split <- round(nrow(finalproj_shuffled_tbl) * .50)
finalproj_train_tbl <- finalproj_shuffled_tbl[1:split,]
finalproj_test_tbl <- finalproj_shuffled_tbl[(split + 1):nrow(finalproj_shuffled_tbl),]
training_folds_full <- createFolds(finalproj_train_tbl$Attrition, 10)

# This code does the same, but for the dataset without satisfaction text
finalproj_without_satisfaction_shuffled_tbl <- finalproj_without_satisfaction_tbl[sample(nrow(finalproj_without_satisfaction_tbl)),]
finalproj_without_satisfaction_train_tbl <- finalproj_without_satisfaction_shuffled_tbl[1:split,]
finalproj_without_satisfaction_test_tbl <- finalproj_without_satisfaction_shuffled_tbl[(split + 1):nrow(finalproj_without_satisfaction_shuffled_tbl),]
training_folds_without_satisfaction <- createFolds(finalproj_without_satisfaction_train_tbl$Attrition, 10)

# We set tuneLength here to make it easier to change for all models at once
presettuneLength <- 4


# The following code sets up parallelization
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

#Tested models with satisfaction words

# This code fits a model predicting whether workers will turnover or not using all other variables with elastic net
tic() #These tics and tocs keep track of how long the code takes - helpful in testing
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

# This code fits a model predicting whether workers will turnover or not using all other variables with a random forest
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

# This code fits a model predicting whether workers will turnover or not using all other variables with extreme gradient boosting
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

# Code for these models is all the same as above, just without satisfaction text data 

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

# This code creates a tibble for the models including satisfaction text data.
# I hardcode in the model names, then get the accuracy of each model for both
# the training data (we can take the max accuracy of the results because final
# models are chosen based on accuracy) and the test data (we have the models
# predict whether employees turnover or not, compare that to the real data, and
# take the mean to tell us what percentage of the time the model made the right
# classification).
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

# This code does the same as above, but using the models trained without the
# satisfaction text data.
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

# The instructions called only for one model comparison table, and that only
# the selected model needed to compare with and without satisfaction text data.
# However, since results seemed quite close regardless, I elected to consider
# with and without text data for all three models, and incorporate that into
# my model selection. I still provide a final table of just the selected model
# accuracy with and without text data.

# Looking at the results across these two tables, it seems like the models are
# pretty similar in terms of how accurate they are at classifying new data into
# employees turning over or not. The exact order of which model makes the most
# accurate predictions seems to change between runs - we don't set a seed because
# the parallel processing we use to bring processing time down makes the standard
# set.seed not prevent randomization across all cores. The model that seems to emerge
# as making the most accurate predictions on new data across runs seems to be the
# Elastic Net. In fact, sometimes the Elastic Net actually makes better predictions
# without the satisfaction text data than with it - I suspect in these cases
# that the satisfaction text data leads to more overfitting of the model on training
# data, leading to worse accuracy on test data.

# The most influential decisions in fitting models seems to have been changing
# The algorithm methods. On the training data, the ELastic Net seems to predict
# less accurately than Random Forest and XGB, sitting at around .90-.92 between runs
# compared to RF and XGB sitting around .98-.99. This implies that the different models
# are making different choices in how they make predictions. Still, it's the models'
# performance on test data that we really care about, where all models drop to an
# accuracy of about .84-.87 between runs. Since the Elastic Net consistently has
# the smallest drop in accuracy in going from training to test data of the three
# tested models, and because it often has the highest accuracy as well (though
# these differences are always pretty small), we select the Elastic Net as our
# final model, as it seems to suffer from overfitting the least and makes classifications
# of which employees will turnover and which won't with at least as much accuracy
# (and possibly a bit more) as that obtained from Random Forest and XGB.


# This code makes a tibble displaying the predictive accuracy of the selected model,
# the Elastic Net, for training and test data when satisfaction text data is and
# is not included.
final_model_text_increment_tbl <- tibble(
  algo = c("Elastic Net", "Elastic Net"),
  text_data = c("With Text Data", "Without Text Data"),
  cv_accuracy = c(
    max(modelElasticNet$results$Accuracy),
    max(modelElasticNetWithoutSatisfaction$results$Accuracy)
  ),
  ho_accuracy = c(
    mean(predict(modelElasticNet, finalproj_test_tbl) == finalproj_test_tbl$Attrition),
    mean(predict(modelElasticNetWithoutSatisfaction, finalproj_without_satisfaction_test_tbl) == finalproj_without_satisfaction_test_tbl$Attrition)
  )
)

# The exact numbers change between runs, but in general, it seems that the training
# accuracy increases with the inclusion of text data. This may be due to more data
# leading to more chance differences for the model to capitalize on. Accuracy goes
# down a bit for the test data regardless, and whether the accuracy on test data
# is better with or without text seems to change between runs (if anything, it
# seems like without text data may be better on training data slightly more often).
# Overall, the predictive accuracy differences between including and not 
# including test data do not seem to be very large - often the difference is 
# within roughly .04 or less on both training and test data.
# Therefore, I do not think the inclusion of the satisfaction text data (at least
# in the way it was used here) results in consistent, considerable positive increments
# in predictive accuracy. I suspect that this may be because this data already
# includes other variables that relate to satisfaction.






