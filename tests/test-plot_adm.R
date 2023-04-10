source("../R/plot_adm.R")


test_that("plot_adm", {
  test_weights<- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                      int25 = 1, int50 = 1, int100 = 0)
  
  data1 <- get_standard_aggregation(iso = "SYR", years = 2020, period = "yearly", adm1 = TRUE, weights = test_weights, threshold = 1)
  case1 <- plot_adm(df = data1, isos = "SYR", adm1 = TRUE, id_col = "capa_id_adm1")
  
  expect_that(case1, is_a("ggplot"))
  
  data2 <- get_standard_aggregation(iso = ison("World"), years = 2020, period = "yearly", adm1 = FALSE, weights = test_weights, threshold = 1)
  case2 <- plot_adm(df = data2, isos = ison("World"), adm1 = FALSE)
  
  expect_that(case2, is_a("ggplot"))
  
})
