source("../R/query_region_gv.R")



test_that("query_region_gv", {
  
  region = "World"
  
  case_list <- list(
    query_region_gv(region, period = "yearly"),
    query_region_gv(region, period = "monthly"),
    query_region_gv(region, period = "biannually"),
    query_region_gv(region, period = "quarterly")
  )
  
  lapply(case_list, function(case){expect_equal(length(case), 3)})
  lapply(case_list, function(case){expect_equal(sum(is.na(case)), 0)})
  lapply(case_list, function(case){expect_equal(sum(is.na(unlist(case))), 0)})
  
})
