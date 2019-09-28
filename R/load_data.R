#' Load Data
#'
#' Imports multiple .csv files into R from a single, specified directory.
#' Files should all be .csv format and have the same columns and names.
#' Modified to include source file as a column
#'
#' @param path the file path where all of your files are stored
#'
#' @return a data frame with all files concatenated
#' @export
#'
#' @examples
load_data <- function(path) {
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  names <- list.files(path_string)
  tables <- lapply(files, read.delim)
  for (i in 1:length(tables)){tables[[i]]<-cbind(tables[[i]],files[i])} # Add filename as a source
  do.call(rbind, tables)
}
