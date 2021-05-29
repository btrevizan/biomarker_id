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

sampling <- 'SMOTE'
results_dir <- file.path('results/test/efs', sampling)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

performance_path <- file.path(results_dir, 'performance.rds')
if(file.exists(performance_path)) {
   performances <- readRDS(performance_path)
} else {
  performances <- data.frame()
}

print("Load training sets.")
datasets <- Sys.glob("data/processed/train/*_common.rda")
train_data <- dataset.load_all(datasets)
train_data <- dataset.smote(train_data$x, train_data$y, 10)

print("Load testing sets.")
test_datasets <- list()
datasets <- Sys.glob("data/processed/test/*_common.rda")
for(filepath in datasets) test_datasets[[utils.filename(filepath)]] <- dataset.load(filepath)
# test_data <- dataset.load_all(datasets)

datasets <- Sys.glob(file.path('results/training', sampling, '*_common/'))

for(a in names(aggrs)) {

  for(b in n_bags_vec) {

    for(f in names(fses)) {

      final_path <- file.path(results_dir, paste('final_ranking_fs_', f, '_bags_', b, '_a_', a, '.rds', sep=''))

      if(file.exists(final_path)) {
        final <- readRDS(final_path)
        rankings <- final$rankings
        final_ranking <- final$final_ranking
        # final_ranking <- readRDS('../meta_analysis/results/final_ranking_rem.rds')
      } else {
        print("Get all final rankings.")
        final_ranking_file <- paste('fs_', f, '_bags_', b, '_a_', a, '.rds', sep = '')
        rankings <- lapply(datasets, function(dirpath) { readRDS(file.path(dirpath, final_ranking_file))$final_ranking })

        print("Aggregate rankings.")
        final_ranking <- aggrs[[a]](rankings)

        final <- list(rankings = rankings, final_ranking = final_ranking)
        saveRDS(final, final_path)
      }

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

            for(test_name in names(test_datasets)) {

              if(nrow(performances) > 0) {
                df <- performances %>% filter(Classifier == method, Threshold == threshold, Number.of.Bags == b, Feature.Selector == f, Aggregation.method == a, Test.Dataset == test_name)
                if(nrow(df) > 0) next
              }

              test_data <- test_datasets[[test_name]]
              x_test <- test_data$x[, selected_genes]
              y_test <- test_data$y

              y_pred <- test_model(model, x_test)
              metrics <- all_metrics(y_test, y_pred)

              metrics[['Stability']] <- c(stability$stability)
              metrics[['Classifier']] <- c(method)
              metrics[['Threshold']] <- c(threshold)
              metrics[['Number of Bags']] <- c(b)
              metrics[['Feature Selector']] <- c(f)
              metrics[['Aggregation method']] <- c(a)
              metrics[['Test Dataset']] <- c(test_name)

              metrics <- data.frame(metrics)
              metrics$Aggregation.method <- as.factor(metrics$Aggregation.method)
              metrics$Threshold <- as.factor(metrics$Threshold)
              metrics$Classifier <- as.factor(metrics$Classifier)

              performances <- rbind(performances, metrics)
            }

            saveRDS(performances, performance_path)
        }
      }
    }
  }
}

