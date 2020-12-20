library(caret)

consistency_index <- function(a, b, k, n) {
  # Compute the consistency index (similarity degree) between 2 subsets of features
  # A Stability Index for Feature Selection, Ludmila I. Kucheva
  # a: subset A
  # b: subset B
  # k: cut-off used (i.e. 100 (cut-off) features selected on a ranking of 500 features)
  # n: number of overall features
  intersection <- intersect(a, b)
  r <- length(intersection)
  
  consistency <- ((r * n) - (k ^ 2)) / (k * (n - k))
  return(consistency)
}

similarity_index <- function(a, b) {
  # Similarity index presented at Kuncheva
  # a: subset A
  # b: subset B
  intersection <- intersect(a, b)
  union_ab <- union(a, b)
  
  similarity <- length(intersection) / length(union_ab)
  return(similarity)
}

stability_index <- function(f, k, n) {
  # According to Kucheva, stability is the average of all pairwise consistency indices
  # f: a set of set of features
  # k: cut-off used (i.e. 100 (cut-off) features selected on a ranking of 500 features)
  # n: number of overall features
  t <- length(f)
  consistencies <- list()
  similarities <- list()
  
  for(i in 1:(t-1)) {
    for(j in (i+1):t) {
      index <- paste('bag', i, 'bag', j, sep = '')
      consistencies[[index]] <- consistency_index(f[[i]], f[[j]], k, n)
      similarities[[index]] <- similarity_index(f[[i]], f[[j]])
    }
  }
  
  consistencies_sum <- sum(unlist(consistencies))
  stability <- (2 * consistencies_sum) / (t * (t - 1))
  
  results <- list(consistencies = consistencies, similarities = similarities, stability = stability)
  return(results)
}

all_metrics <- function(y_true, y_pred) {
  y_true <- as.factor(y_true)
  y_pred <- as.factor(y_pred)

  confMatrix <- confusionMatrix(y_pred, y_true, positive = 'Tumor', mode = 'everything')
  
  results <- list(
    'Accuracy' = confMatrix[['overall']][['Accuracy']],
    'Kappa' = confMatrix[['overall']][['Kappa']],
    'Recall' = confMatrix[['byClass']][['Recall']],
    'Precision' = confMatrix[['byClass']][['Precision']],
    'F1-Score' = confMatrix[['byClass']][['F1']]
  )
  
  return(results)
}
