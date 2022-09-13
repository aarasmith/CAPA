source("../R/placeholders.R")


####create_placeholder_freq####
test_that("create_placeholder_freq", {
  
  case1 <- create_placeholder_freq(760, max_periods = 32, adm1 = FALSE)
  expect_equal(nrow(case1), 32)
  
  case2 <- create_placeholder_freq(ison("Western Asia"), max_periods = 32, adm1 = FALSE)
  expect_equal(nrow(case2), (length(ison("Western Asia")) * 32))
  
})

test_that("create_placeholder_freq adm = T", {
  
  case1 <- create_placeholder_freq(ison("World"), max_periods = 32, adm1 = TRUE)
  no_adm <- ison("World")[ison("World") %!in% adm1_cgaz$iso3n]
  case1_length <- (nrow(adm1_cgaz) * 32) + (length(no_adm) * 32)
  expect_equal(nrow(case1), case1_length)
  
})

####create_placeholder####
test_that("create_placeholder", {
  
  case1 <- create_placeholder(760, years = 1990:2021, period = "yearly", adm1 = FALSE)
  expect_equal(nrow(case1), 32)
  
  case2 <- create_placeholder(ison("Western Asia"), years = 1990:2021, period = "monthly", adm1 = FALSE)
  expect_equal(nrow(case2), (length(ison("Western Asia")) * (32 * 12)))
  
})

test_that("create_placeholder adm = T", {
  
  case1 <- create_placeholder(ison("World"), years = 1990:2021, period = "quarterly", adm1 = TRUE)
  no_adm <- ison("World")[ison("World") %!in% adm1_cgaz$iso3n]
  case1_length <- (nrow(adm1_cgaz) * (32 * 4)) + (length(no_adm) * (32 * 4))
  expect_equal(nrow(case1), case1_length)
  expect_equal(sum(is.na(case1)), (length(no_adm) * (32 * 4) * 2))
  
})
