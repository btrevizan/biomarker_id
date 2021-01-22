setwd('~/biomarker_id/')

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
filename <- utils.filename(datapath)

results_path <- paste('results/training/results_', filename, '.rds', sep = '')

results <- data.frame()
if(file.exists(results_path))
  results <- readRDS(results_path)

# for(datapath in datasets) {
  filename <- utils.filename(datapath)
  
  print("Build dataset.")
  data <- dataset.load(datapath)
  
  partitions <- ensemble.data_partition(data$x, data$y, train_perc)
  
  train_i <- partitions[["train"]]
  train_x <- data$x[train_i, ]
  train_y <- data$y[train_i]
  
  val_i <- partitions[["validation"]]
  validation_x <- data$x[val_i, ]
  validation_y <- data$y[val_i]
  
  for(b in n_bags_vec) {
    
    bags <- ensemble.create_bags(train_x, train_y, b)
    
    for(a in names(aggrs)) {
      
      aggr <- list()
      aggr[[a]] <- aggrs[[a]]
      
      for(f in names(fses)) {
      
        fs <- list()
        fs[[f]] <- fses[[f]]
        
        print("============================= Evaluate ensemble =============================")
        fs_result <- ensemble.feature_selection(fs, aggr, bags)
      
        
        for(t in thresholds) {
          
          for(m in methods) {
            
            result <- results %>% filter(Threshold == t, Classifier == m, Number.of.bags == b, Aggregation.method == a, Feature.selector == f)
            if(nrow(result) > 0) next;
            
            print(paste('Dataset = ', filename))
            print(paste('Threshold = ', t))
            print(paste('Method = ', m))
            print(paste('# bags = ', b))
            print(paste('Aggregation = ', a))
            print(paste('Feature Selection = ', f))
            
            r <- ensemble.eval(validation_x, validation_y, fs_result$rankings, fs_result$final_ranking, t, m, tr_control)
            
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
          }
        }
      }
    }
  }
# }
