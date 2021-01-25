library(smotefamily)

dataset.load <- function(filepath) {
  print(paste("Load", filepath))
  load(filepath)
  
  data <- list('x' = x, 'y' = as.factor(y))
  return(data)
}

dataset.load_all <- function(filepaths) {
  x_all <- data.frame()
  y_all <- c()
  
  for(filepath in filepaths) {
    data <- dataset.load(filepath)
    x_all <- rbind(x_all, data$x)
    y_all <- c(y_all, as.character(data$y))
  }
  
  data <- list('x' = x_all, 'y' = as.factor(y_all))
  return(data)
}

dataset.smote <- function(x, y, k = 5) {
  data <- smotefamily::SMOTE(x, y, K = k)[["data"]]
  x <- data[, names(data) != "class"]
  y <- as.factor(data[, "class"])
  
  data <- list('x' = x, 'y' = y)
  return(data)
}
