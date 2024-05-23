#' Load Values Dictionary
#'
#' This function loads the values dictionary from the package's extdata directory.
#' @param dictionary_path Optional. The path to the values dictionary CSV file. If not provided, the default path will be used.
#' @return A data frame with the values dictionary.
#' @export
load_values_dictionary <- function(dictionary_path = NULL) {
  if (is.null(dictionary_path)) {
    dictionary_path <- system.file("extdata", "values_dictionary.csv", package = "virtus")
  }
  if (dictionary_path == "") {
    stop("The file 'values_dictionary.csv' does not exist in 'inst/extdata/' directory.")
  }
  values_dict <- read.csv(dictionary_path, stringsAsFactors = FALSE)
  return(values_dict)
}
