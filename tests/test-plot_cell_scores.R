source("../R/plot_cell_scores.R")


test_that("plot_cell_scores", {
  test_weights<- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                      int25 = 1, int50 = 1, int100 = 0)
  
  data1 <- get_cell_scores(iso = "SYR", years = 2020, weights = test_weights)
  case1 <- plot_cell_scores(data_list = data1, isos = "SYR")
  
  expect_that(case1, is_a("ggplot"))
  
  data2 <- get_cell_scores(iso = ison("Western Asia"), years = 2020, weights = test_weights, start_end = c(2, 11), draw_adm1 = F, draw_points = F)
  case2 <- plot_cell_scores(data_list = data2, isos = ison("Western Asia"))
  
  expect_that(case2, is_a("ggplot"))
  
})
