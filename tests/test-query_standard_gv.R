source("../R/query_standard_gv.R")



test_that("query_standard_gv", {
  
  case_list <- list(
    query_standard_gv(adm1 = TRUE, period = "yearly"),
    query_standard_gv(adm1 = FALSE, period = "monthly"),
    query_standard_gv(adm1 = TRUE, period = "biannually"),
    query_standard_gv(adm1 = FALSE, period = "quarterly")
  )
  
  lapply(case_list, function(case){expect_equal(length(case), 3)})
  lapply(case_list, function(case){expect_equal(sum(is.na(case)), 0)})
  lapply(case_list, function(case){expect_equal(sum(is.na(unlist(case))), 0)})
  
})
