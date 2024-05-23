#' Analyze Human Values in Text
#'
#' This function analyzes a batch of texts to measure human values based on a predefined dictionary.
#' @param data A dataframe containing the texts to analyze.
#' @param text_column The name of the column containing the texts.
#' @param dictionary_path Optional. The path to the values dictionary CSV file. If not provided, the default path will be used.
#' @param output_type The type of output: "score" (default), "percentage", or "word_percentage".
#' @return A data frame with the original texts and columns for each human value.
#' @export
analyze_values <- function(data, text_column, dictionary_path = NULL, output_type = "score") {
  # Load the values dictionary
  dictionary <- load_values_dictionary(dictionary_path)

  # Ensure the texts are in lowercase for matching
  texts <- tolower(data[[text_column]])

  # Initialize a data frame to store the results
  values <- unique(dictionary$Value)
  prefix <- switch(output_type,
                   score = "count_",
                   percentage = "perc_",
                   word_percentage = "int_")
  result_colnames <- paste0(prefix, values)
  results <- data.frame(data, matrix(0, nrow = nrow(data), ncol = length(values)))
  colnames(results)[(ncol(data) + 1):ncol(results)] <- result_colnames

  # Calculate the score for each value
  for (i in seq_along(texts)) {
    text <- texts[i]
    for (j in seq_along(values)) {
      value <- values[j]
      words <- dictionary[dictionary$Value == value, "Word"]
      count <- sum(sapply(words, function(word) {
        matches <- gregexpr(paste0("\\b", word, "\\b"), text, perl = TRUE)
        sum(sapply(matches, function(match) ifelse(match[1] == -1, 0, length(match))))
      }))
      results[i, result_colnames[j]] <- count
    }

    # Calculate percentages if required
    total_score <- sum(results[i, result_colnames])
    total_words <- sum(strsplit(text, "\\W+")[[1]] != "")
    if (output_type == "percentage" && total_score > 0) {
      results[i, result_colnames] <- results[i, result_colnames] / total_score * 100
    } else if (output_type == "word_percentage" && total_words > 0) {
      results[i, result_colnames] <- results[i, result_colnames] / total_words * 100
    }
  }

  return(results)
}
