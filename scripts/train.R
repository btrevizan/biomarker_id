library(tidyverse)
library(caret)

bootstrap <- function(k, n) {
  # Select k elements in a range of 1 to n with replacement.
  boot <- sample.int(n, k, replace=TRUE);
  return(boot);
}

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

train <- function(x, y, trControl, method = "svmLinear3") {
  model <- train(x, y, method = method, trControl = trControl);
  return(model);
}