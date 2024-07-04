test_that("multiple search works", {
  expect_snapshot(search_series(c("militares", "cargos"), require_all = TRUE))
})

test_that("return empty", {
  expect_equal(search_series("miiltares"),
               data.frame(series_title = character(), series_id = integer(),
                          theme_title = character(), theme_id = integer()))
})
