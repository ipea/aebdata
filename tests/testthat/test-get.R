test_that("missing parameters", {
  expect_error(get_series())
})

test_that("wrong parameters", {
  # Totally wrong
  expect_error(get_series(series_id = 1))
  expect_error(get_series(series_title = "Test"))
  expect_error(get_series(series_id = 1, series_title = "Test"))
  # Partially wrong
  expect_warning(get_series(series_id = c(1, 215)))
  expect_warning(get_series(series_id = 215, series_title = "Test"))
})

test_that("empty series", {
  # all empty
  expect_error(get_series(series_id = 162))
  expect_error(get_series(series_id = c(162, 177)))
  # one works
  expect_warning(get_series(series_id = c(162, 215)))
})

test_that("get works", {
  rds <- readRDS(test_path("_data", "get.rds"))
  expect_equal(get_series(series_id = 215), rds$one)
  expect_equal(get_series(series_id = c(215, 197)),
               list(`215` = rds$one,
                    `197` = rds$multiple))
})

test_that("get_csv works", {
  rds <- readRDS(test_path("_data", "get.rds"))
  expect_null(get_series_csv(162))
  expect_equal(get_series_csv(215), rds$one)
  expect_equal(get_series_csv(197), rds$multiple)
})
