library(tidyverse)
library(caret)

getLOOCV <- function() {
  # Train control using "leave-one-out cross-validation"
  ctrl <- trainControl(method = "LOOCV");
  return(ctrl);
}

getRepeatedCV <- function(k = 10, repeats = 10) {
  # Train control using "repeated cross-validation"
  ctrl <- trainControl(method = "repeatedcv", number = k, repeats = repeats);
  return(ctrl);
}

train <- function(x, y, trControl, method = "svmRadial") {
  model <- train(x, y, method = method, trControl = trControl);
  return(model);
}