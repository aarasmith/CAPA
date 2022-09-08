
test_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 0, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                     int25 = 1, int50 = 1, int100 = 0)


test_that("std_agg runs", {
  
  world_yearly_length <- length(ison("World")) * 32
  
  expect_equal(
    nrow(get_standard_aggregation(iso = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, threshold = 1, selected_period = NA)),
    world_yearly_length
  )
  
  wa_monthly_length <- length(ison("World")) * (32 * 12)
  
  expect_equal(
    nrow(get_standard_aggregation(iso = "World", years = 1990:2021, period = "monthly", adm1 = FALSE, weights = test_weights, threshold = 1, selected_period = NA)),
    wa_monthly_length
  )
  
  wya <- get_standard_aggregation(iso = "World", years = 1990:2021, period = "yearly", adm1 = T, weights = test_weights, threshold = 1, selected_period = NA)
  no_adm <- ison("World")[ison("World") %!in% adm1_cgaz$iso3n]
  wya_length <- (nrow(adm1_cgaz) * 32) + (length(no_adm) * 32)
  expect_equal(
    nrow(wya),
    wya_length
  )
  
  wma <- get_standard_aggregation(iso = "World", years = 1990:2021, period = "monthly", adm1 = T, weights = test_weights, threshold = 1, selected_period = NA)
  no_adm <- ison("World")[ison("World") %!in% adm1_cgaz$iso3n]
  wma_length <- (nrow(adm1_cgaz) * (32 * 12)) + (length(no_adm) * (32 * 12))
  expect_equal(
    nrow(wma),
    wma_length
  )
  
  
})

test_that("std_agg quick", {
  
  expect_equal(
    nrow(get_standard_aggregation(iso = "AFG", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, threshold = 1, selected_period = NA)),
    32
  )
  expect_equal(
    nrow(get_standard_aggregation(iso = "AFG", years = 1990:2021, period = "monthly", adm1 = FALSE, weights = test_weights, threshold = 1, selected_period = NA)),
    (32 * 12)
  )
  expect_equal(
    nrow(get_standard_aggregation(iso = "AFG", years = 1990:2021, period = "yearly", adm1 = TRUE, weights = test_weights, threshold = 1, selected_period = NA)),
    (34 * 32)
  )
  expect_equal(
    nrow(get_standard_aggregation(iso = "AFG", years = 1990:2021, period = "monthly", adm1 = TRUE, weights = test_weights, threshold = 1, selected_period = NA)),
    (34 * 32 * 12)
  )
})
