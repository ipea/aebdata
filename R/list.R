#' List available themes
#'
#' `list_themes()` returns a data.frame containing all available themes and
#' their corresponding ids.
#'
#' @return A data.frame
#' @export
#'
#' @examplesIf aebdata:::test_connection_aeb("temas")
#' # Get the data frame and show the theme titles
#' themes <- list_themes()
#' themes$theme_title

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

#' List created series
#'
#' List all series from the Atlas, regardless of whether they have data
#' available for download or not.
#'
#' @param theme_id,theme_title Optional parameters that can be used individually
#'  or combined to filter the selected themes.
#'
#' @return A data.frame
#' @export
#'
#' @examplesIf aebdata:::test_connection_aeb("temas")
#' series <- list_series(theme_id = c(42, 50))
#' series$series_title
#' \donttest{
#' # List all series and count the number of series available
#' all_series <- list_series()
#' length(unique(all_series$series_id))
#'
#' # Count the number of series from Organizações do Estado theme
#' organizacoes <- list_series(theme_title = "Organizações do Estado")
#' nrow(organizacoes)
#' }

list_series <- function(theme_id = NULL, theme_title = NULL) {

  # List available themes
  df_themes <- aebdata::list_themes()

  # Check the need to filter specific themes
  if (!is.null(theme_id) | !is.null(theme_title)) {

    # Check if at least one theme_id or theme_title exists
    if (length(c(intersect(theme_id, df_themes$theme_id),
                 intersect(theme_title, df_themes$theme_title))) == 0) {
      if (is.null(theme_id) & !is.null(theme_title)) {
        stop("All theme_title's are missing", call. = FALSE)
      } else if (!is.null(theme_id) & is.null(theme_title)) {
        stop("All theme_id's are missing", call. = FALSE)
      } else if (!is.null(theme_id) & !is.null(theme_title)) {
        stop("All theme_id's and theme_title's are missing", call. = FALSE)
      }
    }

    # Check for missing values and warn about them
    if (length(setdiff(theme_id, df_themes$theme_id) > 0)) {
      paste(
        "Missing theme_id values:",
        paste(setdiff(theme_id, df_themes$theme_id), collapse = ", ")
      ) |>
        warning()
    }
    if (length(setdiff(theme_title, df_themes$theme_title) > 0)) {
      paste(
        "Missing theme_title values:",
        paste(setdiff(theme_title, df_themes$theme_title), collapse = ", ")
      ) |>
        warning()
    }

    # Filter the selected themes
    df_themes <- df_themes[df_themes$theme_id %in% theme_id |
                             df_themes$theme_title %in% theme_title, ]

  }

  # List series from each theme
  ls_series <- df_themes$theme_id |>
    lapply(function(id) list_series_id(theme_id = id))

  vc_theme_rep <- sapply(ls_series, nrow)

  # Bind the series
  df_series <- cbind(
    theme_title = rep(df_themes$theme_title, times = vc_theme_rep),
    theme_id = rep(df_themes$theme_id, times = vc_theme_rep),
    do.call(rbind, ls_series)
  )

  rm(vc_theme_rep, df_themes)

  return(df_series)

}

#' List series from a specific theme
#'
#' @param theme_id The theme's id to return series from.
#'
#' @return A data.frame containing serie_title and serie_id.
#'
#' @examplesIf aebdata:::test_connection_aeb()
#' list_series_id(theme_id = 26)
#'
#' @noRd

list_series_id <- function(theme_id) {
  df_series <- "https://www.ipea.gov.br/atlasestado/api/v1/series/" |>
    paste0(theme_id) |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    lapply(`[`, c("titulo", "id")) |>
    do.call(rbind.data.frame, args = _)

  names(df_series) <- c("series_title", "series_id")

  return(df_series)
}
