# Functions ===================================================================
source('scripts/utils/train.R')

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




