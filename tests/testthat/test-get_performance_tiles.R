context("get_performance_tiles")

test_that("input validation works", {
  expect_error(get_performance_tiles(service = "X", quarter = "2020-01-01"))
  expect_error(get_performance_tiles(service = "mobile", quarter = "2020-01-13"))
  expect_error(get_performance_tiles(service = "fixed", quarter = "2020-01-01", sf = 12))
})
