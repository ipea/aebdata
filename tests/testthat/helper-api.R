skip_if_api_offline <- function() {
  testthat::skip_if_offline()
  if (!aebdata:::test_connection_aeb()) {
    testthat::skip("API is offline")
  }
}

try_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      if (grepl("Could not connect", e$message, fixed = TRUE) ||
          grepl("cannot open the connection", e$message, fixed = TRUE) ||
          grepl("Timeout was reached", e$message, fixed = TRUE)) {
        testthat::skip("API connection error occurred; skipping test")
      } else {
        stop(e)
      }
    }
  )
}
