library(RobustRankAggreg)

aggrMean <- function(rankingList) {
  # rankingList: A list of rankings
  # n: number of elements
  # return -> data.frame with elements and scores
  glist <- aggrData(rankingList)
  aggrRank <- aggregateRanks(glist = glist, N = length(rankingList[[1]]), method = "mean")
  rankOrder <- order(aggrRank$Score, decreasing = TRUE)
  return(aggrRank[rankOrder, ])
}