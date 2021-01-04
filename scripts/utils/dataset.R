library(smotefamily)

dataset.load <- function(filepath) {
  print(paste("Load", filepath))
  load(filepath)
  
  data <- list('x' = x, 'y' = as.factor(y))
  return(data)
}

dataset.smote <- function(x, y, k = 5) {
  data <- smotefamily::SMOTE(x, y, K = k)[["data"]]
  x <- data[, names(data) != "class"]
  y <- as.factor(data[, "class"])
  
  data <- list('x' = x, 'y' = y)
  return(data)
}
