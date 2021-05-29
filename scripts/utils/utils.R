utils.filename <- function(filepath) {
  filenameParts <- strsplit(filepath, "/")[[1]]
  filenameWithExtension <- tail(filenameParts, n = 1)
  filenameWithExtensionParts <- strsplit(filenameWithExtension, "\\.")[[1]]
  filename <- filenameWithExtensionParts[1] 
  return(filename)
}

utils.save_plot <- function(path, plot) {
  ggsave(path, plot, width = 8, height = 5, dpi = 320, units = "in");
}