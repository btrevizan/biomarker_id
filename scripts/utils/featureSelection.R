library(mRMRe)

minRedundancyMaxRelevance <- function(x, y) {
  # Return a data.frame with features names and respective ordered scores
  # the last element have the highest score
  dataDim <- dim(x)
  nCols <- dataDim[2]
  
  orderedY <- ordered(y)
  yOrder <- order(y)
  
  orderedX <- x[yOrder, ]
  df <- data.frame(orderedX, orderedY)
  
  data <- mRMR.data(df)
  result <- mRMR.classic(data = data, target_indices = c(nCols + 1), feature_count = nCols)
  
  scores <- result@scores[[1]]
  features <- result@feature_names[1:nCols]
  
  ranking <- data.frame(feature = features, score = scores)
  rankingOrder <- order(ranking$score)
  
  return(ranking[rankingOrder, ])
}