source("scripts/utils/metrics.R")
source("scripts/utils/dataset.R")
source("scripts/utils/train.R")

ensemble.eval <- function(validation_x, validation_y, rankings, final_ranking, threshold, method, tr_control) {
  ##### Calculate stability
  print("Calculate stability.")
  stability <- stability_index(lapply(rankings, function(r) { head(r$feature, n = threshold) }), threshold, nrow(final_ranking))
  print(stability$stability)
  
  ##### Apply threshold
  selected_genes <- head(final_ranking$feature, n=threshold)
  
  #### 30% for validation
  validation_x <- validation_x[, selected_genes]
  
  ##### Train classifier ####
  print(paste("Train model with method ", method, ".", sep = ""))
  model <- train_model(validation_x, validation_y, tr_control, method)
  
  ##### Extract metrics from model ####
  metrics <- list()
  model_predictions <- model[['pred']]
  folds <- levels(as.factor(model_predictions$Resample))

  for(fold in folds) {
    pred <- model_predictions %>% filter(Resample == fold)
    all <- all_metrics(pred$obs, pred$pred)
    for(m in names(all)) metrics[[m]] <- c(metrics[[m]], all[[m]])
  }

  sum_metrics <- list()
  for(metric in names(metrics)) {
    sum_metrics[[paste(metric, "Mean")]] <- mean(metrics[[metric]])
    sum_metrics[[paste(metric, "Std")]] <- sd(metrics[[metric]])
  }
  
  #### Save results ####
  results <- list()
  results[['Threshold']] <- c(results[['Threshold']], threshold)
  results[['Classifier']] <- c(results[['Classifier']], method)
  results[['K-Fold']] <- c(results[['K-Fold']], kfold)
  results[['Training percentage']] <- c(results[['Training percentage']], train_perc)
  results[['Evaluation method']] <- c(results[['Evaluation method']], tr_control$method)
  results[['Stability']] <- c(results[['Stability']], stability$stability)
  for(metric in names(sum_metrics)) results[[metric]] <- c(results[[metric]], sum_metrics[[metric]])
  
  #### Finish ####
  print("Done.")
  
  res <- list('results' = results, 'model' = model)
  return(res)
}

ensemble.create_bags <- function(x, y, n_bags) {
  bags <- list()
  i <- 1
  
  for(bag in 1:n_bags) {
    # Resample bag with at least 3 normal observations
    n_normal <- 0
    n_tumor <- 0
    
    while((n_normal < 4) | (n_tumor < 4)) {
      indices <- createResample(y, times = 1, list = TRUE)[[1]]
      bag_y <- train_y[indices]
      
      n_normal <- length(bag_y[bag_y == 'Normal'])
      n_tumor <- length(bag_y[bag_y == 'Tumor'])
    }
    
    # Apply SMOTE over bag
    bags[[i]] <- dataset.smote(x[indices, ], bag_y, 3)
    i <- i + 1
  }
  
  return(bags)
}

ensemble.data_partition <- function(x, y, train_perc) {
  train_i <- createDataPartition(y, times = 1, p = train_perc, list = TRUE)[[1]]
  validation_i <- setdiff(1:nrow(x), train_i)
  
  result <- list("train" = train_i, "validation" = validation_i)
  return(result)
}

ensemble.feature_selection <- function(fs, aggr, bags) {
  fs_name <- names(fs)[1]
  fs <- fs[[fs_name]]
  
  aggr_name <- names(aggr)[1]
  aggr <- aggr[[aggr_name]]
  
  ##### Feature selection step
  i <- 1
  n_bags <- length(bags)
  rankings <- list()
  
  for(bag in bags) {
    print(paste("Run bag #", i, " of ", n_bags, " bags.", sep = ""))
    rankings[[i]] <- fs(bag$x, bag$y)
    i <- i + 1
  }
  
  ##### Aggregate rankings
  print("Aggregate rankings.")
  final_ranking <- aggr(rankings)
  
  result <- list("rankings" = rankings, "final_ranking" = final_ranking)
  return(result)
}
