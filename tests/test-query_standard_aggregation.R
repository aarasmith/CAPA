source("../R/query_standard_aggregation.R")

#query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db, score = FALSE)

test_that("query_standard_aggregation", {
  
  #inputs
  iso3n <- ison(c("AFG", "SYR", "YEM"))
  years <- 1990:2021
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  threshold <- 1
  capa_db <- connect_to_capa()
  
  case_list <- list(
    query_standard_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_standard_gv(adm1 = FALSE, period = "monthly"), capa_db, score = FALSE),
    query_standard_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_standard_gv(adm1 = TRUE, period = "yearly"), capa_db, score = FALSE),
    query_standard_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_standard_gv(adm1 = FALSE, period = "biannually"), capa_db, score = FALSE),
    query_standard_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_standard_gv(adm1 = TRUE, period = "quarterly"), capa_db, score = FALSE)
  )
  
  lapply(case_list, function(case){expect_gt(nrow(case), 0)})
  
})
