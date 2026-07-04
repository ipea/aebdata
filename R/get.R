#' Get series values
#'
#' The function `get_series()` is the main function of this package. Its goal
#' is to facilitate direct access to the data published in the Atlas do Estado
#' Brasileiro so that the user can work with them as they wish.
#'
#' If the parameter used is for just one series, the result will be a data.frame
#' containing the requested information. Now, if the parameter refers to more
#' than one series, the return will be a list of data.frames, with each
#' data.frame corresponding to a series.
#'
#' @param series_id,series_title The series ids or titles to download
#'
#' @return A data.frame or a list containing the data from the series
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Get the series 230 and print the head
#' serie_230 <- get_series(series_id = 230)
#' head(serie_230)
#'
#' # Get the series from 230 to 232 and print the head of the 232
#' \donttest{
#' series <- get_series(series_id = 230:232)
#' head(series[["232"]])
#' }

get_series <- function(series_id = NULL, series_title = NULL) {
  old_timeout <- getOption("timeout")
  new_timeout <- max(old_timeout, 60)
  options(timeout = new_timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Check if both series_id and serie_title are empty
  if (is.null(series_id) & is.null(series_title)) {

    stop("Provide a series_id or a series_title", call. = FALSE)

  } else {

    # Check connection
    if(!test_connection_aeb()) {
      cli::cli_abort(
        "Could not connect. Please, check your connection or try again later."
      )
    }

    # List all series
    df_series <- "https://www.ipea.gov.br/atlasestado/api/v1/series" |>
      httr2::request() |>
      httr2::req_timeout(new_timeout) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      # Select only the title and id to avoid problems with subthemes
      lapply(`[`, c("titulo", "id")) |>
      do.call(rbind.data.frame, args = _)

    names(df_series) <- c("series_title", "series_id")

      # Check if at least one theme_id or theme_title is correct
      if (length(c(intersect(series_id, df_series$series_id),
                   intersect(series_title, df_series$series_title))) == 0) {
        if (is.null(series_id) & !is.null(series_title)) {
          stop("None of the series_title's exist", call. = FALSE)
        } else if (!is.null(series_id) & is.null(series_title)) {
          stop("None of the series_id's exist", call. = FALSE)
        } else if (!is.null(series_id) & !is.null(series_title)) {
          stop("None of the series_id's and series_title's exist",
               call. = FALSE)
        }
      }

      # Check for missing values
      if (length(setdiff(series_id, df_series$series_id) > 0)) {
        paste(
          "Values of series_id that don't exist:",
          paste(setdiff(series_id, df_series$series_id), collapse = ", ")
        ) |>
          warning()
      }
      if (length(setdiff(series_title, df_series$series_title) > 0)) {
        paste(
          "Values of series_title that don't exist:",
          paste(setdiff(series_title, df_series$series_title), collapse = ", ")
        ) |>
          warning()
      }

      # Filter the selected series
      df_series <- df_series[df_series$series_id %in% series_id |
                               df_series$series_title %in% series_title, ]

      # If is just one valid series
      if (nrow(df_series) == 1) {

        # Try to get the files
        result <- get_series_csv(series_id = df_series$series_id)

        # If there is no file
        if (is.null(result)) {

          paste0(
            "The following series don't have any data available:\n id  title\n",
            paste(
              formatC(df_series$series_id, digits = 3),
              substr(df_series$series_title, 1, 65)
            )
          )|>
            stop(call. = FALSE)

        } else {

          # Return the files if they exist
          return(result)

        }

      } else {
        # If there are more than one valid series

        # Get values for each one
        ls_series <- df_series$series_id |>
          lapply(function(id) try(get_series_csv(series_id = id)))

        # Use the ids as names to identify the series
        names(ls_series) <- df_series$series_id

        # Check for valid results
        vt_null <- sapply(ls_series, is.null)

        # If nothing is valid
        if (sum(vt_null) == length(ls_series)) {

          # Stop
          paste0(
            "The following series don't have any data available:\n id  title\n",
            paste(
              formatC(df_series$series_id, digits = 3),
              substr(df_series$series_title, 1, 65),
              collapse = "\n"
            )
          )|>
            stop(call. = FALSE)

        } else {
          # If at least one is not NULL

          # If one is NULL, print a warning
          if (sum(vt_null) > 0) {
            df_series <- df_series[vt_null,]
            paste0(
              "The following series don't have any data available:\n",
              " id  title\n",
              paste(
                formatC(df_series$series_id, digits = 3),
                substr(df_series$series_title, 1, 65),
                collapse = "\n"
              )
            )|>
              warning()
          }

          # Remove the NULLs
          ls_series <- ls_series[!vt_null]

          # Return the list
          return(ls_series)

        }

      }

    }

}

get_series_csv <- function(series_id) {
  old_timeout <- getOption("timeout")
  new_timeout <- max(old_timeout, 60)
  options(timeout = new_timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)

  # List all csv files of a specific series
  vc_downloads <- "https://www.ipea.gov.br/atlasestado/consulta/" |>
    paste0(series_id) |>
    httr2::request() |>
    httr2::req_timeout(new_timeout) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    rvest::html_element(".modal-body") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    grep("\\.csv$", x = _, ignore.case = TRUE, value = TRUE)

  # Check if it is an empty vector
  if (length(vc_downloads) == 0) {
    return(NULL)
  }

  # Helper function to download CSV raw data and parse it
  download_and_parse_csv <- function(path) {
    req <- paste0("https://www.ipea.gov.br/atlasestado/", path) |>
      httr2::request() |>
      httr2::req_timeout(new_timeout)
    resp <- httr2::req_perform(req)
    readr::read_delim(httr2::resp_body_raw(resp), delim = ";", show_col_types = FALSE) |>
      as.data.frame()
  }

  # Download if it is not an empty vector
  if (length(vc_downloads) == 1) {

    # Read the file as a data.frame
    df_series <- download_and_parse_csv(vc_downloads)

  } else {

    # Read all files as data.frames
    df_series <- vc_downloads |>
      lapply(download_and_parse_csv)

    # Assign names to each element based on the file name
    names(df_series) <- basename(vc_downloads) |>
      gsub("\\.csv$", "", x = _, ignore.case = TRUE)

  }

  return(df_series)

}
