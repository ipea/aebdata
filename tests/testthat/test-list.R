test_that("list works", {
  skip_if_api_offline()
  rds <- readRDS(test_path("_data", "list.rds"))
  expect_equal(try_or_skip(list_themes()), rds$themes)
  expect_equal(try_or_skip(list_series(theme_id = 50,
                                       theme_title = "Militares na burocracia")),
               rds$militares_organizacoes)
})

test_that("wrong parameters", {
  skip_if_api_offline()
  # Totally wrong
  expect_error(try_or_skip(list_series(theme_id = 1)))
  expect_error(try_or_skip(list_series(theme_title = "Test")))
  expect_error(try_or_skip(list_series(theme_id = 1, theme_title = "Test")))
  # Partially wrong
  expect_warning(try_or_skip(list_series(theme_id = c(1, 50))))
  expect_warning(try_or_skip(list_series(theme_id = 50, theme_title = "Test")))
})

test_that("list_themes fails clearly when themes endpoint is unavailable", {
  local_mocked_bindings(
    test_connection_aeb = function(api = "series") FALSE,
    .package = "aebdata"
  )

  expect_error(
    list_themes(),
    "Could not connect. Please, check your connection or try again later."
  )
})
