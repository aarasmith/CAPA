source("../R/query_frequency.R")

#query_frequency(iso3n, years, start_end, weights, threshold, gv, capa_db)

test_that("query_frequency", {
  
  #inputs
  iso3n <- ison(c("AFG", "SYR", "YEM"))
  years <- 1990:2021
  start_end <- c(1, 12)
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  threshold <- 1
  capa_db <- connect_to_capa()
  
  case_list <- list(
    query_frequency(iso3n, years, start_end, weights = test_weights, threshold,
                               gv = query_temporal_gv(adm1 = FALSE, period = "monthly", start_end, years, capa_db), capa_db),
    query_frequency(iso3n, years, start_end, weights = test_weights, threshold,
                               gv = query_temporal_gv(adm1 = TRUE, period = "yearly", start_end, years, capa_db), capa_db),
    query_frequency(iso3n, years, start_end, weights = test_weights, threshold,
                               gv = query_temporal_gv(adm1 = FALSE, period = "biannually", start_end, years, capa_db), capa_db),
    query_frequency(iso3n, years, start_end, weights = test_weights, threshold,
                               gv = query_temporal_gv(adm1 = TRUE, period = "quarterly", start_end, years, capa_db), capa_db)
  )
  
  lapply(case_list, function(case){expect_gt(nrow(case), 5)})
  
})
