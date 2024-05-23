#' Load Values Dictionary
#'
#' This function loads the values dictionary from the package's extdata directory.
#' @return A data frame with the values dictionary.
#' @export
load_values_dictionary <- function() {
  file_path <- system.file("extdata", "values_dictionary.csv", package = "virtus")
  if (file_path == "") {
    stop("The file 'values_dictionary.csv' does not exist in 'inst/extdata/' directory.")
  }
  print(paste("Loading file from:", file_path))
  values_dict <- read.csv(file_path, stringsAsFactors = FALSE)
  return(values_dict)
}
