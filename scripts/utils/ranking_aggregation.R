library(RobustRankAggreg)

aggr_mean <- function(rankingList) {
  # rankingList: A list of rankings
  # n: number of elements
  # return -> data.frame with elements and scores
  features <- rownames(rankingList[[1]])
  aggrRank <- data.frame(row.names = features)
  
  for(f in features) {
    scores <- sapply(rankingList, function(rank) { return(rank[f, "score"]) })
    
    aggrRank[f, "feature"] <- f
    aggrRank[f, "score"] <- mean(scores)
  }
  
  # aggrRank <- aggregateRanks(glist = ranks, N = length(rankingList[[1]]), method = "mean")
  
  rankOrder <- order(aggrRank$feature)
  aggrRank <- aggrRank[rankOrder, ]
  
  rankOrder <- order(aggrRank$score, decreasing = TRUE)
  return(aggrRank[rankOrder, ])
}