#' Test the connection to the API
#'
#' @param api API to be tested
#'
#' @return Boolean
#'
#' @noRd

test_connection_aeb <- function(api = "series") {

  # Returns the status code of the connection to the API
  vc_status <- "https://www.ipea.gov.br/atlasestado/api/v1/" |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_perform() |>
    httr2::resp_status() |>
    tryCatch(error = function(cnd) NULL)

  # A boolean value if the status is equal to 200
  vc_connected <- identical(vc_status, 200L)

  return(vc_connected)
}
