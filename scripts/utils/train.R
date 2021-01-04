library(tidyverse)
library(caret)

source('scripts/utils/dataset.R')
train.smote <- list(name = "my_smote", func = function(x, y) { return(dataset.smote(x, y, 1)) }, first = TRUE)

five_stats <- function (...) {
  # More metrics
  c(twoClassSummary(...), defaultSummary(...))
}

get_LOOCV <- function() {
  # Train control using "leave-one-out cross-validation"
  ctrl <- trainControl(method = "LOOCV") # , sampling = train.smote) #, summaryFunction = five_stats)
  return(ctrl)
}

get_repeated_cv <- function(k = 10, repeats = 10) {
  # Train control using "repeated cross-validation"
  ctrl <- trainControl(method = "repeatedcv", number = k, repeats = repeats, savePredictions = "all") #, sampling = train.smote) #, summaryFunction = five_stats)
  return(ctrl)
}

get_cv <- function(k = 10) {
  # Train control using "repeated cross-validation"
  ctrl <- trainControl(method = "cv", number = k, savePredictions = "all") # , sampling = train.smote) #, summaryFunction = five_stats)
  return(ctrl)
}

train_model <- function(x, y, trControl, method) {
  model <- train(x, y, method = method, trControl = trControl)
  return(model)
}

test_model <- function(model, x) {
  y_pred <- predict(model, newdata = x)
  return(y_pred)
}