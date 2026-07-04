test_that("connection works", {
  skip_if_api_offline()
  expect_true(test_connection_aeb())
})
