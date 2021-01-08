library(biomaRt)
library(dplyr)

clearGeneSymbol <- function(geneSymbol) {
  mart <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  mapping <- getBM(attributes = c('hgnc_symbol', 'entrezgene_id'), values = geneSymbol, mart = mart)
  
  geneSymbol.mappedIdx <- match(geneSymbol,mapping$hgnc_symbol)  ## retorna índice (linha) em que o genesymbol foi encontrado
  geneSymbol.mappedEntrez <- mapping[geneSymbol.mappedIdx, ]  ## retorna o entrezID equivalente (e NA se o geneSymbol não foi encontrado)
  
  not.na <- which(!is.na(geneSymbol.mappedEntrez$entrezgene_id))
  genes <- geneSymbol.mappedEntrez[not.na, ]
  
  return(genes)
}

trainFilepaths <- Sys.glob("data/processed/train/*.rda")
testFilepaths <- Sys.glob("data/processed/test/*.rda")
commonTrainFilepaths <- Sys.glob("data/processed/train/*_common.rda")
commonTestFilepaths <- Sys.glob("data/processed/test/*_common.rda")
notProcessedTrainFilepaths <- setdiff(trainFilepaths, commonTrainFilepaths)
notProcessedTestFilepaths <- setdiff(testFilepaths, commonTestFilepaths)
filepaths <- c(notProcessedTrainFilepaths, notProcessedTestFilepaths)

print("Getting all common genes...")

commonGenes <- NULL
for(filepath in filepaths) {
  print(filepath)
  load(filepath)
  geneSymbol <- clearGeneSymbol(geneSymbol)
  
  if(is.null(commonGenes)) {
    commonGenes <- geneSymbol
  } else {
    common <- commonGenes %>% semi_join(geneSymbol, by = 'entrezgene_id')
    commonGenes <- common
  }
}

mapping <- list()
for(i in 1:dim(commonGenes)[1]) {
  mapping[[commonGenes$hgnc_symbol[i]]] <- as.character(commonGenes$entrezgene_id[i])
}

print("Saving datasets only with common genes...")

for(filepath in filepaths) {
  load(filepath)
  
  x <- x[, commonGenes$hgnc_symbol]
  for(i in 1:length(colnames(x))) {
    colnames(x)[i] <- paste('gene_', mapping[[colnames(x)[i]]], sep = "")
  }
  
  y <- as.factor(t(y))
  
  print(paste(filepath, dim(x)[1], dim(x)[2]))
  
  new_filepath <- gsub(".rda", "_common.rda", filepath)
  save(x, y, file = new_filepath)
}

print("Done.")

