setwd('~/biomarker_id/')
options(java.parameters = "-Xmx16000m")

library(dplyr)

source('scripts/utils/ranking_aggregation.R')
source('scripts/utils/feature_selection.R')
source('scripts/utils/parameters.R')
source('scripts/utils/ensemble.R')
source('scripts/utils/dataset.R')
source('scripts/utils/train.R')
source('scripts/utils/utils.R')

performance_path <- paste('results/performance.rds', sep = '')
performances <- data.frame()

print("Load training sets.")
datasets <- Sys.glob("data/processed/train/*_common.rda")
train_data <- dataset.load_all(datasets)
train_data <- dataset.smote(train_data$x, train_data$y, 10)

print("Load testing sets.")
datasets <- Sys.glob("data/processed/test/*_common.rda")
test_data <- dataset.load_all(datasets)

print("Get all final rankings.")
datasets <- Sys.glob('results/training/*_common/')
rankings <- lapply(datasets, function(dirpath) { readRDS(file.path(dirpath, 'final_ranking.rds')) })

for(a in names(aggrs)) {
  
  print("Aggregate rankings.")
  final_ranking <- aggrs[[a]](rankings)
  
  for(threshold in thresholds) {
    
    print("Calculate stability.")
    stability <- stability_index(lapply(rankings, function(r) { head(r$feature, n=threshold) }), threshold, nrow(final_ranking))
    print(stability$stability)
    
    print("Select genes.")
    selected_genes <- head(final_ranking$feature, n=threshold)
    
    x_train <- train_data$x[, selected_genes]
    y_train <- train_data$y[selected_genes]
    
    x_test <- test_data$x[, selected_genes]
    y_test <- test_data$y[selected_genes]
    
    for(method in methods) {
      print(paste("Train using ", method, " and threshold ", threshold, ".", sep = ""))
      
      model <- train_model(x_train, y_train, tr_control, method)
      y_pred <- test_model(model, x_test)
      
      metrics <- all_metrics(y_test, y_pred)
      
      metrics[['Classifier']] <- c(method)
      metrics[['Threshold']] <- c(threshold)
      metrics[['Aggregation method']] <- c(a)
      
      metrics <- data.frame(metrics)
      metrics$Aggregation.method <- as.factor(metrics$Aggregation.method)
      metrics$Threshold <- as.factor(metrics$Threshold)
      metrics$Classifier <- as.factor(metrics$Classifier)
      
      performances <- rbind(performances, metrics)
    }
  }
}

saveRDS(performances, performance_path)