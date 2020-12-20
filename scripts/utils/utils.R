utils.filename <- function(filepath) {
  filenameParts <- strsplit(filepath, "/")[[1]]
  filenameWithExtension <- tail(filenameParts, n = 1)
  filenameWithExtensionParts <- strsplit(filenameWithExtension, "\\.")[[1]]
  filename <- filenameWithExtensionParts[1] 
  return(filename)
}