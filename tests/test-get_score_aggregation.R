source("../R/get_score_aggregation.R")

#get_score_aggregation(iso, years, period, adm1, weights, threshold = 1, cap = NA, score_selection = NULL)

test_that("get_score_aggregation quick", {
  
  test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 1, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                       int25 = 1, int50 = 1, int100 = 0)
  
  expect_gt(
    nrow(get_score_aggregation(iso = "AFG", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, threshold = 1, score_selection = c(1, 25))),
    0
  )
  expect_gt(
    nrow(get_score_aggregation(iso = "AFG", years = 1990:2021, period = "monthly", adm1 = FALSE, weights = test_weights, threshold = 1, score_selection = c(1, 25))),
    0
  )
  expect_gt(
    nrow(get_score_aggregation(iso = "AFG", years = 1990:2021, period = "yearly", adm1 = TRUE, weights = test_weights, threshold = 1, score_selection = c(1, 25))),
    0
  )
  expect_gt(
    nrow(get_score_aggregation(iso = "AFG", years = 1990:2021, period = "monthly", adm1 = TRUE, weights = test_weights, threshold = 1, score_selection = c(1, 25))),
    0
  )
})
