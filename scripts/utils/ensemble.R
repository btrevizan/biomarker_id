source("scripts/utils/metrics.R")
source("scripts/utils/dataset.R")
source("scripts/utils/train.R")

ensemble.eval <- function(x, y, n_bags, fs, aggr, threshold, method, train_perc = 0.7, tr_control = get_repeated_cv()) {
  classes <- summary(y)
  
  fs_name <- names(fs)[1]
  fs <- fs[[fs_name]]
  
  aggr_name <- names(aggr)[1]
  aggr <- aggr[[aggr_name]]
  
  # 10-fold cross validation ####################################################
  results <- list()
  k <- 1
  
  train_i <- createDataPartition(y, times = 1, p = train_perc, list = TRUE)[[1]]
  validation_i <- setdiff(1:nrow(x), train_i)
  
  #### 70% for training
  train_data <- dataset.smote(x[train_i, ], y[train_i])
  train_x <- train_data$x
  train_y <- train_data$y
  
  ###### Create bag
  bags <- createResample(train_y, times = n_bags)
  
  ##### For each bag
  i <- 1
  rankings <- list()
  
  for(bag in bags) {
    print(paste("Run bag #", i, " of ", n_bags, " bags.", sep = ""))
    
    bag_x <- train_x[bag, ]
    bag_y <- train_y[bag]
    
    ###### Run feature selection
    ranking <- fs(bag_x, bag_y)
    
    ###### Save feature ranking
    rankings[[i]] <- ranking
    
    i <- i + 1
  }
  
  ##### Calculate stability
  print("Calculate stability.")
  stability <- stability_index(lapply(rankings, function(r) { head(r$feature, n = threshold) }), threshold, ncol(x))
  print(stability$stability)
  
  ##### Aggregate rankings
  print("Aggregate rankings.")
  final_ranking <- aggr(rankings)
  
  ##### Apply threshold
  selected_genes <- head(final_ranking$feature, n=threshold)
  
  #### 30% for validation
  validation_x <- x[validation_i, selected_genes]
  validation_y <- y[validation_i]
  
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
  results[['Number of bags']] <- c(results[['Number of bags']], n_bags)
  results[['Feature selector']] <- c(results[['Feature selector']], fs_name)
  results[['Aggregation method']] <- c(results[['Aggregation method']], aggr_name)
  results[['Threshold']] <- c(results[['Threshold']], threshold)
  results[['Classifier']] <- c(results[['Classifier']], method)
  results[['K-Fold']] <- c(results[['K-Fold']], kfold)
  results[['Training percentage']] <- c(results[['Training percentage']], train_perc)
  results[['Evaluation method']] <- c(results[['Evaluation method']], tr_control$method)
  results[['Stability']] <- c(results[['Stability']], stability$stability)
  for(metric in names(sum_metrics)) results[[metric]] <- c(results[[metric]], sum_metrics[[metric]])

  results <- data.frame(results)
  results$Number.of.bags <- as.factor(results$Number.of.bags)
  results$Feature.selector <- as.factor(results$Feature.selector)
  results$Aggregation.method <- as.factor(results$Aggregation.method)
  results$Threshold <- as.factor(results$Threshold)
  results$Classifier <- as.factor(results$Classifier)
  results$K.Fold <- as.factor(results$K.Fold)
  results$Evaluation.method <- as.factor(results$Evaluation.method)
  
  #### Finish ####
  print("Done.")
  
  res_list <- list("stability" = stability, "final_ranking" = final_ranking, "rankings" = rankings, "results" = results)
  return(res_list)
}
