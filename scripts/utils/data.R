filepath <- file.choose()
data <- readRDS(filepath)  # data.x and data.y

# Data dimensions
dataDim <- dim(data)
nRows <- dataDim[1]
nCols <- dataDim[2] - 1

x <- data[, 1:nCols]
y <- as.factor(data[, nCols + 1])

remove(data, dataDim)

# Get metadata ================================================================
# File name
filenameParts <- strsplit(filepath, "/")[[1]]
filenameWithExtension <- tail(filenameParts, n = 1)
filenameWithExtensionParts <- strsplit(filenameWithExtension, "\\.")[[1]]
filename <- filenameWithExtensionParts[1]

remove(filenameParts, filenameWithExtension, filenameWithExtensionParts)

# Data path
datapath <- paste("data/", filename, "/", sep = "")
savedPartitionPath <- paste(datapath, "trainI.rds", sep = "")
savedBagPath <- paste(datapath, "bag.rds", sep = "")
savedFeatureRankingsPath <- paste(datapath, "featureRanks.rds", sep = "")
savedSelectedFeaturesPath <- paste(datapath, "selectedFeatures.rds", sep = "")
savedConsistencies <- paste(datapath, "consistencies.rds", sep = "")
savedSimilarities <- paste(datapath, "similarities.rds", sep = "")
savedParamsPath <- paste(datapath, "params.rda", sep = "")
savedFinalRankingPath <- paste(datapath, "finalRanking.rds", sep = "")
savedModelPath <- paste(datapath, "model.rds", sep = "")

dir.create(datapath, showWarnings = FALSE)

# Split in training and testing partitions ====================================
trainI <- NULL
trainI <- readRDS(savedPartitionPath)
if(is.null(trainI)) {
  # Partition was not created
  trainI <- createDataPartition(y, times = 1, p = trainPorportion, list = TRUE)[[1]]  # create
  saveRDS(trainI, file = savedPartitionPath)                                          # save
}

testI <- setdiff(1:nRows, trainI)

trainX <- x[trainI, ]
trainY <- y[trainI]

testX <- x[testI, ]
testY <- y[testI]