source("../R/get_CAR.R")


test_that("get_CAR", {
  
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  
  iso_count <- nrow(dbGetQuery(connect_to_capa(), "SELECT DISTINCT iso3n FROM region_key"))
  region_count <- nrow(dbGetQuery(connect_to_capa(), "SELECT DISTINCT region FROM region_key WHERE region != 'World'"))
  
  case1 <- get_CAR("World", 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, exclusive = T)
  case2 <- agg_car(case1, level = "Region")
  case3 <- agg_car(case1, level = "Global")
  
  expect_equal(
    nrow(case1),
    iso_count * 32
  )
  expect_equal(
    nrow(case2),
    region_count * 32
  )
  expect_equal(
    nrow(case3),
    32
  )
  
  #test exclusive vs inclusive
  case4 <- get_CAR("World", 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, exclusive = F)
  expect_equal(
    sum(case4$risk_pop_low),
    sum(case1$risk_pop_low, case1$risk_pop_medium, case1$risk_pop_high, case1$risk_pop_extreme)
  )
  
  
})
