test_that("missing parameters", {
  skip_if_api_offline()
  expect_error(try_or_skip(get_series()))
})

test_that("wrong parameters", {
  skip_if_api_offline()
  # Totally wrong
  expect_error(try_or_skip(get_series(series_id = 1)))
  expect_error(try_or_skip(get_series(series_title = "Test")))
  expect_error(try_or_skip(get_series(series_id = 1, series_title = "Test")))
  # Partially wrong
  expect_warning(try_or_skip(get_series(series_id = c(1, 215))))
  expect_warning(try_or_skip(get_series(series_id = 215, series_title = "Test")))
})

test_that("empty series", {
  skip_if_api_offline()
  # all empty
  expect_error(try_or_skip(get_series(series_id = 162)))
  expect_error(try_or_skip(get_series(series_id = c(162, 177))))
  # one works
  expect_warning(try_or_skip(get_series(series_id = c(162, 215))))
})

test_that("get works", {
  skip_if_api_offline()
  rds <- readRDS(test_path("_data", "get.rds"))
  expect_equal(try_or_skip(get_series(series_id = 215)), rds$one)
  expect_equal(try_or_skip(get_series(series_id = c(215, 197))),
               list(`215` = rds$one,
                    `197` = rds$multiple))
})

test_that("get_csv works", {
  skip_if_api_offline()
  rds <- readRDS(test_path("_data", "get.rds"))
  expect_null(try_or_skip(get_series_csv(162)))
  expect_equal(try_or_skip(get_series_csv(215)), rds$one)
  expect_equal(try_or_skip(get_series_csv(197)), rds$multiple)
})

test_that("get_series fails clearly when series endpoint is unavailable", {
  local_mocked_bindings(
    test_connection_aeb = function(api = "series") FALSE,
    .package = "aebdata"
  )

  expect_error(
    get_series(series_id = 215),
    "Could not connect. Please, check your connection or try again later."
  )
})
