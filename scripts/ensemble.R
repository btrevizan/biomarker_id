# Input: dataset
# Output: selected features

# Libraries ===================================================================
library(caret)

source("scripts/utils/featureSelection.R")
source("scripts/utils/rankingAggregation.R")

# Parameters ==================================================================
trainPorportion <- 0.7
nBags <- 2
fsAlgorithm <- minRedundancyMaxRelevance # According to B. Seijo-Pardo, mRMR presented best result for microarray datasets
threshold <- 500
aggrAlgorithm <- aggrMean  # According to B. Seijo-Pardo, mean presented best result for microarray datasets

# Load data ===================================================================
source('scripts/utils/data.R')

# Feature selection ensemble ==================================================
bag <- NULL
bag <- readRDS(savedBagPath)
if(is.null(bag)) {
  bag <- createResample(trainY, times = nBags, list = TRUE)
  saveRDS(bag, file = savedBagPath)
}

## Selection
rankIndex <- 1
featureRankings<- list()
selectedFeatures <- list()

for(indices in bag) {
  bagX <- trainX[indices, ]
  bagY <- trainY[indices]
  
  # Feature selection according to bag
  featureRanks[[rankIndex]] <- fsAlgorithm(bagX, bagY)
  selectedFeatures[[rankIndex]] <- featureRanks[[rankIndex]]$feature[nCols - threshold:nCols]
  
  rankIndex <- rankIndex + 1
}

saveRDS(featureRanks, file = savedFeatureRankingsPath)
saveRDS(selectedFeatures, file = savedSelectedFeaturesPath)

# Stability index =============================================================
selectedFeatures <- readRDS(savedSelectedFeaturesPath)

consistencyIndex <- function(a, b, k, n) {
  # Compute the consistency index (similarity degree) between 2 subsets of features
  # A Stability Index for Feature Selection, Ludmila I. Kucheva
  # a: subset A
  # b: subset B
  # k: cut-off used (i.e. 100 (cut-off) features selected on a ranking of 500 features)
  # n: number of overall features
  intersection <- intersect(a, b)
  r <- length(intersection)
  kTerm <- (k ^ 2) / n
  
  consistency <- (r - kTerm) / (k - kTerm)
  return(consistency)
}

similarityIndex <- function(a, b) {
  # Similarity index presented at Kuncheva
  # a: subset A
  # b: subset B
  intersection <- intersect(a, b)
  union_ab <- union(a, b)
  
  similarity <- length(intersection) / length(union_ab)
  return(similarity)
}

stabilityIndex <- function(f, k, n) {
  # According to Kucheva, stability is the average of all pairwise consistency indices
  # f: a set of set of features
  # k: cut-off used (i.e. 100 (cut-off) features selected on a ranking of 500 features)
  # n: number of overall features
  t <- length(f)
  consistencies <- list()
  similarities <- list()
  
  for(i in 1:t-1) {
    for(j in i+1:t) {
      index <- paste("bag", i, "bag", j, sep = "")
      
      consistencies[[index]] <- consistencyIndex(f[[i]], f[[j]], k, n)
      similarities[[index]] <- similarityIndex(f[[i]], f[[j]])
    }
  }
  
  saveRDS(consistencies, file = savedConsistencies)
  saveRDS(similarities, file = savedSimilarities)
  
  consistenciesSum <- sum(unlist(consistencies))
  stability <- (2 / (t * (t - 1))) * consistenciesSum
  
  return(stability)
}

stability <- stabilityIndex(selectedFeatures, threshold, nCols)

save(stability, 
     trainPorportion, 
     nBags, 
     fsAlgorithm, 
     threshold, 
     aggrAlgorithm, 
     file = savedParamsPath)

# Aggregate rankings ==========================================================
selectedFeatures <- readRDS(savedSelectedFeaturesPath)

finalRanking <- aggrAlgorithm(selectedFeatures)

saveRDS(finalRanking, file = savedFinalRankingPath)









