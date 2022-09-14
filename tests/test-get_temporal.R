source("../R/get_temporal.R")



test_that("get_temporal", {
  
  case1 <- get_temporal("AFG", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = 2, max_periods = 4)
  case2 <- get_temporal("AFG", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = NA, max_periods = 4)
  case3 <- get_temporal("AFG", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = TRUE, threshold = 1, p_threshold = 2, max_periods = 4)
  case4 <- get_temporal("AFG", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = TRUE, threshold = 1, p_threshold = NA, max_periods = 4)
  
  case5 <- get_temporal("World", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = 2, max_periods = 4)
  
  case_list <- list(
    case1,
    case2,
    case3,
    case4
  )
  
  expect_equal(nrow(case1), 1)
  expect_equal(nrow(case2), 4)
  
  expect_gt(nrow(case5), 0)
  expect_gt(sum(case5$risk_pop), 0)
  
  lapply(case_list, function(case){ expect_gt(sum(case$risk_pct), 0)})
  
  
})
