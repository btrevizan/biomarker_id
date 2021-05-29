source("scripts/utils/ranking_aggregation.R")
source("scripts/utils/feature_selection.R")
source("scripts/utils/train.R")

# Fixed parameters
kfold <- 5
train_perc <- 0.6
tr_control <- get_cv(kfold)
sampling <- c('SMOTE', 'Down Sampling', 'No Sampling')
datasets <- Sys.glob("data/processed/train/*_common.rda")

# Others
thresholds <- c(5, 10, 15, 20, 25, 30, 50, 75, 100, 150, 200, 250, 500)
methods <- c("svmRadial", "J48", "knn", "nnet")
n_bags_vec <- c(5, 10, 25, 50, 100)
aggrs <- list("Mean" = aggr_mean)
fses <- list("Information Gain" = mlrFS("information_gain"),
             "Chi Squared" = chi_squared, 
             "Symmetrical Uncertainty" = sym_uncertainty, 
             "mRMR" = mlrFS("mrmr"), 
             "Variance" = mlrFS("variance"))
