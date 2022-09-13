source("../R/get_region_aggregation.R")



test_that("get_region_aggregation", {
  
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  
  expect_equal(
    nrow(get_region_aggregation(region = "World", years = 1990:2021, weights = test_weights, period = "yearly", threshold = 1)),
    32
  )
  expect_equal(
    nrow(get_region_aggregation(region = "Western Asia", years = 1990:2021, weights = test_weights, period = "monthly", threshold = 1)),
    (32 * 12)
  )
  expect_equal(
    nrow(get_region_aggregation(region = "Western Asia", years = 1990:2021, weights = test_weights, period = "biannually", threshold = 1)),
    (32 * 2)
  )
})
