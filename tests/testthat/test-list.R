test_that("list works", {
  expect_snapshot(list_themes())
  expect_snapshot(list_series(theme_id = 50,
                              theme_title = "Militares na burocracia"))
})

test_that("invalid theme id", {
  expect_error(list_series(1:3), regexp = "^All.*are missing$")
  expect_snapshot_warning(list_series(theme_id = c(1,50)))
})
