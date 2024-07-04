test_that("multiple search works", {
  skip_if_not_installed("stringi")
  expect_snapshot(search_series(c("militares", "cargos"), require_all = TRUE))
})

test_that("return empty", {
  skip_if_not_installed("stringi")
  expect_equal(search_series("miiltares"),
               data.frame(series_title = character(), series_id = integer(),
                          theme_title = character(), theme_id = integer()))
})
