source("../R/query_region_aggregation.R")

#query_region_aggregation(iso3n, years, weights, threshold, gv, capa_db)

test_that("query_region_aggregation", {
  
  #inputs
  region <- "Western Asia"
  iso3n <- ison("Western Asia")
  years <- 1990:2021
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  threshold <- 1
  capa_db <- connect_to_capa()
  
  case_list <- list(
    query_region_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_region_gv(region, period = "monthly"), capa_db),
    query_region_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_region_gv(region, period = "yearly"), capa_db),
    query_region_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_region_gv(region, period = "biannually"), capa_db),
    query_region_aggregation(iso3n, years, weights = test_weights, threshold,
                               gv = query_region_gv(region, period = "quarterly"), capa_db)
  )
  
  lapply(case_list, function(case){expect_gt(nrow(case), 0)})
  
})
