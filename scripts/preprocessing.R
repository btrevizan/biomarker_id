clearGeneSymbol <- function(geneSymbol) {
  geneSymbol <- gsub("-", ".", geneSymbol)
  geneSymbol <- gsub("@", ".", geneSymbol)
  geneSymbol <- gsub("_", ".", geneSymbol)
  geneSymbol <- gsub("/", ".", geneSymbol)
  
  return(geneSymbol)
}

trainFilepaths <- Sys.glob("data/processed/train/*.rda")
testFilepaths <- Sys.glob("data/processed/test/*.rda")
commonTrainFilepaths <- Sys.glob("data/processed/train/*_common.rda")
commonTestFilepaths <- Sys.glob("data/processed/test/*_common.rda")
notProcessedTrainFilepaths <- setdiff(trainFilepaths, commonTrainFilepaths)
notProcessedTestFilepaths <- setdiff(testFilepaths, commonTestFilepaths)
filepaths <- c(notProcessedTrainFilepaths, notProcessedTestFilepaths)

print("Getting all common genes...")

commonGenes <- c()
for(filepath in filepaths) {
  load(filepath)
  geneSymbol <- clearGeneSymbol(geneSymbol)
  
  if(is_null(commonGenes)) {
    commonGenes <- as.vector(geneSymbol)
  } else {
    commonGenes <- as.vector(intersect(commonGenes, geneSymbol))
  }
}

print("Saving datasets only with common genes...")

for(filepath in filepaths) {
  load(filepath)
  names(x) <- clearGeneSymbol(names(x))
  
  x <- x[, commonGenes]
  y <- as.factor(t(y))
  
  print(paste(filepath, dim(x)[1], dim(x)[2]))
  
  new_filepath <- gsub(".rda", "_common.rda", filepath)
  save(x, y, file = new_filepath)
}

print("Done.")
