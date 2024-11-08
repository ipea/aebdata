test_that("search works", {
  skip_if_not_installed("stringi")
  rds <- readRDS(test_path("_data", "search.rds"))
  expect_equal(search_series("militares"), rds$simple)
})

test_that("require_all works", {
  skip_if_not_installed("stringi")
  rds <- readRDS(test_path("_data", "search.rds"))
  expect_equal(search_series(c("militares", "cargos"), require_all = TRUE),
               rds$requireall_true)
})

test_that("case_insensitive works", {
  skip_if_not_installed("stringi")
  rds <- readRDS(test_path("_data", "search.rds"))
  expect_equal(search_series("CAP ", case_insensitive = FALSE),
               rds$caseinsensitive_false)
})

test_that("return empty", {
  skip_if_not_installed("stringi")
  expect_equal(search_series("miiltares"),
               data.frame(series_title = character(), series_id = integer(),
                          theme_title = character(), theme_id = integer()))
})
