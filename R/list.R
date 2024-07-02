#' List available themes
#'
#' `list_themes()` returns a data.frame containing all available themes and
#' their corresponding ids.
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' list_themes()

list_themes <- function() {

  # Get the themes from API
  df_themes <- "https://www.ipea.gov.br/atlasestado/api/v1/temas" |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    # Select only the title and id to avoid problems with subthemes
    lapply(`[`, c("titulo", "id")) |>
    do.call(rbind.data.frame, args = _)

  # Change the column names to more specific names
  names(df_themes) <- c("theme_title", "theme_id")

  # Return the object
  return(df_themes)

}

