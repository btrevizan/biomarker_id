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

train <- function(x, y, trControl, method = "svmLinear3") {
  model <- train(x, y, method = method, trControl = trControl);
  return(model);
}

# Load data ===================================================================
source('scripts/utils/data.R')
remove(x, y)

# Get selected features =======================================================
finalRanking <- readRDS(savedFinalRankingPath)
features <- as.vector(finalRanking$Name)

trainX <- trainX[features]
testX <- testX[features]

# Evaluation ==================================================================
trControl <- getLOOCV()
model <- train(testX, testY, trControl)

saveRDS(model, file = savedModelPath)




