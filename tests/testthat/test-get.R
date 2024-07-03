test_that("get works", {
  expect_snapshot(get_series(series_id = 215))
  expect_snapshot(get_series(series_id = c(215, 219)))
})
