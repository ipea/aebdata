test_that("list works", {
  rds <- readRDS(test_path("_data", "list.rds"))
  expect_equal(list_themes(), rds$themes)
  expect_equal(list_series(theme_id = 50,
                           theme_title = "Militares na burocracia"),
               rds$militares_organizacoes)
})

test_that("wrong parameters", {
  # Totally wrong
  expect_error(list_series(theme_id = 1))
  expect_error(list_series(theme_title = "Test"))
  expect_error(list_series(theme_id = 1, theme_title = "Test"))
  # Partially wrong
  expect_warning(list_series(theme_id = c(1, 50)))
  expect_warning(list_series(theme_id = 50, theme_title = "Test"))
})
