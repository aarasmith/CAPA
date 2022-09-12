source("../R/query_cell_scores.R")



test_that("query_cell_scores", {
  
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  capa_db <- connect_to_capa()
  
  case_list <- list(
    query_cell_scores(760, 2020, start_end = c(1,12), test_weights, capa_db),
    query_cell_scores(760, 2019:2020, start_end = c(2,11), test_weights, capa_db),
    query_cell_scores(c(760, 368), 2020, start_end = c(1,12), test_weights, capa_db)
  )
  
  lapply(case_list, function(case){expect_s3_class(case, "sf")})
  lapply(case_list, function(case){expect_equal(sum(is.na(case)), 0)})
  lapply(case_list, function(case){expect_gt(nrow(case), 0)})
  
})
