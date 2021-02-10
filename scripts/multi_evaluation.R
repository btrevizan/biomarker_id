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

results_dir <- 'results/test'
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

performance_path <- file.path(results_dir, 'performance.rds')
performances <- data.frame()

print("Load training sets.")
datasets <- Sys.glob("data/processed/train/*_common.rda")
train_data <- dataset.load_all(datasets)
# train_data <- dataset.smote(train_data$x, train_data$y, 10)

print("Load testing sets.")
test_datasets <- list()
datasets <- Sys.glob("data/processed/test/*_common.rda")
for(filepath in datasets) test_datasets[[utils.filename(filepath)]] <- dataset.load(filepath)

print("Get all final rankings.")
datasets <- Sys.glob('results/smote_training/*_common/')
final_ranking_file <- 'fs_Variance_bags_10_a_Mean.rds'
rankings <- lapply(datasets, function(dirpath) { readRDS(file.path(dirpath, final_ranking_file))$final_ranking })

for(a in names(aggrs)) {
  
  print("Aggregate rankings.")
  final_ranking <- aggrs[[a]](rankings)
  
  final_path <- file.path(results_dir, paste('final_ranking_', a, '.rds', sep=''))
  saveRDS(final_ranking, final_path)
  
  for(threshold in thresholds) {
    
    print("Calculate stability.")
    stability <- stability_index(lapply(rankings, function(r) { head(r$feature, n=threshold) }), threshold, nrow(final_ranking))
    print(stability$stability)
    
    print("Select genes.")
    selected_genes <- head(final_ranking$feature, n=threshold)
    
    x_train <- train_data$x[, selected_genes]
    y_train <- train_data$y
    
    for(method in methods) {
      print(paste("Train using ", method, " and threshold ", threshold, ".", sep = ""))
      
      model <- train_model(x_train, y_train, tr_control, method)
      
      model_path <- file.path(results_dir, paste('model_', method, '_t_', threshold, '_a_', a, '.rds', sep=''))
      saveRDS(model, model_path)
      
      for(dt in names(test_datasets)) {
        test_data <- test_datasets[[dt]]
        
        x_test <- test_data$x[, selected_genes]
        y_test <- test_data$y
        
        y_pred <- test_model(model, x_test)
        
        pred_path <- file.path(results_dir, paste('y_pred_', dt, '_m_', method, '_t_', threshold, '_a_', a, '.rds', sep=''))
        saveRDS(y_pred, pred_path)
        
        metrics <- all_metrics(y_test, y_pred)
        
        metrics[['Stability']] <- c(stability$stability)
        metrics[['Classifier']] <- c(method)
        metrics[['Threshold']] <- c(threshold)
        metrics[['Aggregation method']] <- c(a)
        metrics[['Test set']] <- c(gsub('_common', '', dt))
        
        metrics <- data.frame(metrics)
        metrics$Aggregation.method <- as.factor(metrics$Aggregation.method)
        metrics$Threshold <- as.factor(metrics$Threshold)
        metrics$Classifier <- as.factor(metrics$Classifier)
        
        performances <- rbind(performances, metrics)
      }
    }
  }
}

saveRDS(performances, performance_path)
