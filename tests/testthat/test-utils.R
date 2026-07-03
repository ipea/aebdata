test_that("connection works", {
  skip_if_offline()
  expect_true(test_connection_aeb())
})
