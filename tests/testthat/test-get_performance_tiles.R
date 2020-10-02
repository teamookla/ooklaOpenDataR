context("get_performance_tiles")

test_that("input validation works", {
  expect_error(get_performance_tiles(service = "X", year = 2020, quarter = 1))
  expect_error(get_performance_tiles(service = "mobile", year = 20, quarter = 1))
  expect_error(get_performance_tiles(service = "mobile", year = 2020, quarter = 0))
  expect_error(get_performance_tiles(service = "mobile", year = 2020, quarter = 1, sf = 12))
})
