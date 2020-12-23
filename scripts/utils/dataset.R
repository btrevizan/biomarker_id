library(smotefamily)

dataset.load <- function(filepath) {
  print(paste("Load", filepath))
  load(filepath)
  
  data <- list('x' = x, 'y' = as.factor(y))
  return(data)
}

dataset.smote <- function(x, y) {
  data <- SMOTE(x, y, K = 3)[["data"]]
  x <- data[, names(data) != "class"]
  y <- as.factor(data[, "class"])
  
  data <- list('x' = x, 'y' = y)
  return(data)
}
