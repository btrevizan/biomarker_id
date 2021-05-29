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

args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])
datapath <- datasets[i]

for(datapath in datasets) {
  filename <- utils.filename(datapath)
  
  results_dir <- file.path('results/training/Down Sampling', filename)
  results_path <- file.path(results_dir, 'performance.rds')
  partitions_path <- file.path(results_dir, 'partitions.rds')
  
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  results <- data.frame()
  
  print("Build dataset.")
  data <- dataset.load(datapath)
  
  if(file.exists(partitions_path)) {
    partitions <- readRDS(partitions_path)
  } else {
    partitions <- ensemble.data_partition(data$x, data$y, train_perc)
    saveRDS(partitions, partitions_path)
  }
  
  train_i <- partitions[["train"]]
  train_x <- data$x[train_i, ]
  train_y <- data$y[train_i]
  
  val_i <- partitions[["validation"]]
  validation_x <- data$x[val_i, ]
  validation_y <- data$y[val_i]
  
  for(b in n_bags_vec) {
    
    bags_path <- file.path(results_dir, paste('bags_', b, '.rds', sep = ''))
    if(file.exists(bags_path)) {
      bags <- readRDS(bags_path)
    } else {
      print(paste("Create", b, "bags.", sep = " "))
      bags <- ensemble.create_bags(train_x, train_y, b)
      saveRDS(bags, bags_path)
    }
    
    for(a in names(aggrs)) {
      
      aggr <- list()
      aggr[[a]] <- aggrs[[a]]
      
      for(f in names(fses)) {
      
        fs <- list()
        fs[[f]] <- fses[[f]]
        
        fs_path <- file.path(results_dir, paste('fs_', f, '_bags_', b, '_a_', a, '.rds', sep = ''))
        if(file.exists(fs_path)) {
          fs_result <- readRDS(fs_path)
        } else {
          fs_result <- ensemble.feature_selection(fs, aggr, bags)
          saveRDS(fs_result, fs_path)
        }
        
        for(t in thresholds) {
          
          for(m in methods) {

            model_path <- file.path(results_dir, paste('model_', m, '_t_', t, '_fs_', f, '_bags_', b, '_a_', a, '.rds', sep = ''))
            if(file.exists(model_path)) next;

            print("============================= Evaluate ensemble =============================")
            print(paste('Dataset = ', filename))
            print(paste('Threshold = ', t))
            print(paste('Method = ', m))
            print(paste('# bags = ', b))
            print(paste('Aggregation = ', a))
            print(paste('Feature Selection = ', f))

            res <- ensemble.eval(validation_x, validation_y, fs_result$rankings, fs_result$final_ranking, t, m, tr_control)

            r <- res$results
            r[['Number of bags']] <- c(r[['Number of bags']], b)
            r[['Feature selector']] <- c(r[['Feature selector']], f)
            r[['Aggregation method']] <- c(r[['Aggregation method']], a)

            r <- data.frame(r)
            r$Number.of.bags <- as.factor(r$Number.of.bags)
            r$Feature.selector <- as.factor(r$Feature.selector)
            r$Aggregation.method <- as.factor(r$Aggregation.method)
            r$Threshold <- as.factor(r$Threshold)
            r$Classifier <- as.factor(r$Classifier)
            r$K.Fold <- as.factor(r$K.Fold)
            r$Evaluation.method <- as.factor(r$Evaluation.method)

            r <- cbind(r, list('Dataset' = filename))
            results <- rbind(results, r)

            print(paste('Mean Recall =', r$Recall.Mean))
            print(paste('Std Recall =', r$Recall.Std))

            saveRDS(results, results_path)
            saveRDS(res$model, model_path)
          }
        }
      }
    }
  }
}
