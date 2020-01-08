consistencyIndex <- function(a, b, k, n) {
  # Compute the consistency index (similarity degree) between 2 subsets of features
  # A Stability Index for Feature Selection, Ludmila I. Kucheva
  # a: subset A
  # b: subset B
  # k: cut-off used (i.e. 100 (cut-off) features selected on a ranking of 500 features)
  # n: number of overall features
  intersection <- intersect(a, b);
  r <- length(intersection);
  
  similarity <- (r * n - k ^ 2) / (k * (n - k));
  return(similarity);
}

stability <- function(f, k, n) {
  # According to Kucheva, stability is the average of all pairwise consistency indices
  # f: a set of set of features
  # k: cut-off used (i.e. 100 (cut-off) features selected on a ranking of 500 features)
  # n: number of overall features
  t <- length(f);
  sumOfConsistencies <- 0;
  
  for(i in 1:t-1) {
    for(j in i+1:t) {
      sumOfConsistencies <- sumOfConsistencies + consistencyIndex(f[i], f[j], k, n);
    }
  }
  
  score <- (2 / (t^2 - t)) * sumOfConsistencies;
  return(score);
}
