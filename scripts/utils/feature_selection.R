library(mlr3)
library(mlr3filters)
library(mRMRe)
library(FSelector)
library(FSelectorRcpp)

sym_uncertainty <- function(x, y) {
  # Return a data.frame with features names and respective ordered scores
  # the last element have the highest score
  data <- set_data(x, y)
  result <- information_gain(orderedY~., data, type = "symuncert", threads = 6)
  
  scores <- result$importance
  features <- as.factor(result$attributes)
  
  ranking <- set_ranking(scores, features)
  return(ranking)
}

gain_ratio <- function(x, y) {
  # Return a data.frame with features names and respective ordered scores
  # the last element have the highest score
  data <- set_data(x, y)
  result <- information_gain(orderedY~., data, type = "gainratio", threads = 6)
  
  scores <- result$importance
  features <- as.factor(result$attributes)
  
  ranking <- set_ranking(scores, features)
  return(ranking)
}

reliefF <- function(x, y) {
  # Return a data.frame with features names and respective ordered scores
  # the last element have the highest score
  data <- set_data(x, y)
  result <- relief(orderedY~., data, neighbours.count = 5, sample.size = nrow(data))
  
  scores <- result$attr_importance
  features <- as.factor(rownames(result))
  
  ranking <- set_ranking(scores, features)
  return(ranking)
}

chi_squared <- function(x, y) {
  # Return a data.frame with features names and respective ordered scores
  # the last element have the highest score
  data <- set_data(x, y)
  result <- chi.squared(orderedY~., data)
  
  scores <- result$attr_importance
  features <- as.factor(rownames(result))
  
  ranking <- set_ranking(scores, features)
  return(ranking)
}

oneR_FS <- function(x, y) {
  # Return a data.frame with features names and respective ordered scores
  # the last element have the highest score
  data <- set_data(x, y)
  result <- oneR(orderedY~., data)
  
  scores <- result$attr_importance
  features <- as.factor(rownames(result))
  
  ranking <- set_ranking(scores, features)
  return(ranking)
}

set_data <- function(x, y) {
  dataDim <- dim(x)
  
  yOrder <- order(y)
  
  orderedX <- x[yOrder, ]
  orderedY <- y[yOrder]
  
  df <- cbind(orderedX, orderedY)
  
  return(df)
}

set_ranking <- function(scores, features) {
  ranking <- data.frame(row.names = as.factor(features), feature = as.factor(features), score = scores)
  rankingOrder <- order(ranking$score, decreasing = TRUE)
  
  return(ranking[rankingOrder, ])
}

mlrFS <- function(method) {
  return (function(x, y) {
    data <- set_data(x, y)
    task <- TaskClassif$new(id = "data", backend = data, target = "orderedY")
    filter <- flt(method)
    
    results <- as.data.table(filter$calculate(task))
    ranking <- set_ranking(results$score, as.factor(results$feature))
    
    return(ranking)
  })
}
