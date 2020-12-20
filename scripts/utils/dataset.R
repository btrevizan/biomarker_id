library(smotefamily)

dataset.load <- function(filepath) {
  print(paste("Load", filepath))
  load(filepath) 
  
  # SMOTE
  print("Run SMOTE.")
  
  data <- dataset.smote(x, y)
  return(data)
}

dataset.smote <- function(x, y) {
  data <- SMOTE(x, y, K = 3)[["data"]]
  x <- data[, names(data) != "class"]
  y <- as.factor(data[, "class"])
  
  data <- list('x' = x, 'y' = y)
  return(data)
}
