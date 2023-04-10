source("../R/get_cell_scores.R")

#x <- get_cell_scores(iso, years, weights, start_end = c(1, 12), draw_adm1 = TRUE, draw_points = TRUE)

test_that("get_cell_scores", {
  
  test_weights<- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  capa_db <- connect_to_capa()
  
  case_list <- list(
    get_cell_scores(760, 2020, test_weights, start_end = c(1, 12)),
    get_cell_scores(760, 2019:2020, test_weights, start_end = c(2, 11)),
    get_cell_scores(c(760, 368), 2020, test_weights, start_end = c(1, 12)),
    get_cell_scores("USA", 2020, test_weights, start_end = c(1, 12))
  )
  
  lapply(case_list, function(case){expect_s3_class(case$cells, "sf")})
  lapply(case_list, function(case){expect_s3_class(case$adm, "sf")})
  lapply(case_list, function(case){expect_s3_class(case$events, "sf")})
  
  lapply(case_list, function(case){
    if(nrow(case$cells) > 0){
      expect_gt(nrow(case$adm), 0)
      expect_gt(nrow(case$events), 0)
    }
  })
  
  
})
