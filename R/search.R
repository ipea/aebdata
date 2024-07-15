#' Search for specific words
#'
#' @description
#' The `search_series()` function provides a quick way to access a list of
#' series that may be of interest, along with their respective IDs. Each series
#' appears according to the number of themes it is present in.
#'
#' If one result should appear before the other, the result can be sorted by
#' using the word multiple times in the search, thus giving it greater
#' importance.
#'
#' @param words A word or an array of words to check
#' @param case_insensitive Ignore the difference between uppercase and lowercase
#' @param require_all Require all words
#'
#' @return A data.frame
#' @export
#'
#' @examplesIf aebdata:::test_connection_aeb() & requireNamespace("stringi", quietly = TRUE)
#' \donttest{
#' search_result <- search_series("regime de contratação")
#' search_result$series_title
#'
#' search_result <- search_series(c("remuneração", "raça"), require_all = TRUE)
#' search_result$series_title
#' }

search_series <- function(words, case_insensitive = TRUE, require_all = FALSE) {

  # Check if stringi is installed
  # nocov start
  if(!requireNamespace("stringi", quietly = TRUE)) {
    stop("This function needs the instalation of 'stringi' package",
         call. = FALSE)
  }
  # nocov end

  # List all series available
  series_df <- aebdata::list_series()

  # Search for each word in the list of titles and count the number of
  # occurrences
  result <- words |>
    sapply(
      function(x) stringi::stri_subset_fixed(
        unique(series_df$series_title),
        x,
        case_insensitive = case_insensitive)
    ) |>
    unlist() |>
    table() |>
    as.data.frame()

  # If is an empty result
  if (nrow(result) == 0 & ncol(result) == 1) {
    result <- data.frame(character(), integer(), character(), integer())
    names(result) <- c("series_title", "series_id", "theme_title", "theme_id")

    return(result)
  }

  names(result) <- c("series_title", "freq")

  # Remove cases where not all words were found when require_all = TRUE
  if (require_all) {
    result <- result[result$freq == length(words), ]
  }

  # Merge the data.frames and order by number of occurrences
  result <- merge(series_df, result, all.x = TRUE)
  result <- result[!is.na(result$freq), ]
  result <- result[order(-result$freq, result$series_id, result$theme_id), ]
  result <- result[,!names(result) %in% c("freq")]

  # Reorder columns
  result <- result[,c("series_title", "series_id", "theme_title", "theme_id")]

  # Remove original row numbers
  rownames(result) <- NULL

  return(result)
}
