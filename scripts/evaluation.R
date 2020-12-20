setwd('/Users/btrevizan/Documents/codelab.nosync/biomarker_id')

source('scripts/utils/ranking_aggregation.R')
source('scripts/utils/feature_selection.R')
source('scripts/utils/parameters.R')
source('scripts/utils/ensemble.R')
source('scripts/utils/dataset.R')
source('scripts/utils/train.R')
source('scripts/utils/utils.R')

results_path <- 'results/training/results.rds'
results <- readRDS(results_path)

for(datapath in datasets) {
  filename <- utils.filename(datapath)
  
  print("Build dataset.")
  data <- dataset.load(datapath)
  
  for(t in thresholds) {
    for(m in methods) {
      for(b in n_bags_vec) {
        for(a in names(aggrs)) {
          aggr <- list(a = aggrs[[a]])
          
          for(f in names(fses)) {
            fs <- list(f = fses[[f]])
            
            print("Evaluate ensemble.")
            print(paste('Dataset = ', filename))
            print(paste('Threshold = ', t))
            print(paste('Method = ', m))
            print(paste('# bags = ', b))
            print(paste('Aggregation = ', a))
            print(paste('Feature Selection = ', f))
            
            r <- ensemble.eval(data$x, data$y, b, fs, aggr, t, m, train_perc, tr_control)
            r <- cbind(r$results, list('Dataset' = filename))
            
            results <- rbind(results, r)
            
            print(paste('Mean Recall =', r$Recall.Mean))
            print(paste('Std Recall =', r$Recall.Std))
            
            saveRDS(results, results_path)
          }
        }
      }
    }
  }
}
