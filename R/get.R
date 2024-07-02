
#' Get series data.frames
#'
#' @param series_id The series id to download
#'
#' @return a data.frame or a list
#'
#' @examples
#' get_series_csv(231)
#'
#' @noRd

get_series_csv <- function(series_id) {

  # List all csv files of a specific series
  vc_downloads <- "https://www.ipea.gov.br/atlasestado/consulta/" |>
    paste0(series_id) |>
    rvest::read_html() |>
    rvest::html_element(".modal-body") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    grep("\\.csv$", x = _, ignore.case = TRUE, value = TRUE)

  # Check if it is an empty vector
  if (length(vc_downloads) == 0) {
    return(NULL)
  }

  # Download if it is not an empty vector
  if (length(vc_downloads) == 1) {

    # Read the file as a data.frame
    df_series <- vc_downloads |>
      (\(x) paste0("https://www.ipea.gov.br/atlasestado/", x))() |>
      readr::read_delim(delim = ";", show_col_types = FALSE) |>
      as.data.frame()

  } else {

    # Read all files as data.frames
    df_series <- vc_downloads |>
      (\(x) paste0("https://www.ipea.gov.br/atlasestado/", x))() |>
      sapply(function(link) readr::read_delim(link,
                                              delim = ";",
                                              show_col_types = FALSE)) |>
      lapply(as.data.frame)

    # Assign names to each element based on the file name
    names(df_series) <- basename(vc_downloads) |>
      gsub("\\.csv$", "", x = _, ignore.case = TRUE)

  }

  return(df_series)

}
