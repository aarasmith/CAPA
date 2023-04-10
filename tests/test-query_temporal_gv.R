source("../R/query_temporal_gv.R")



test_that("query_temporal_gv", {
  
  case_list <- list(
    query_temporal_gv(adm1 = FALSE, period = "monthly", start_end, years, capa_db),
    query_temporal_gv(adm1 = TRUE, period = "yearly", start_end, years, capa_db),
    query_temporal_gv(adm1 = FALSE, period = "biannually", start_end, years, capa_db),
    query_temporal_gv(adm1 = TRUE, period = "quarterly", start_end, years, capa_db)
  )
  
  lapply(case_list, function(case){expect_equal(length(case), 6)})
  lapply(case_list, function(case){expect_equal(sum(is.na(case)), 0)})
  lapply(case_list, function(case){expect_equal(sum(is.null(case)), 0)})
  lapply(case_list, function(case){expect_equal(sum(is.na(unlist(case))), 0)})
  
})
