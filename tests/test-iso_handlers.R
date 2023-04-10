source("../R/utils-iso_handlers.R")


####ison####
test_that("ison is numeric", {
  
  case_list <- list(
    ison("AFG"),
    ison("KOS"),
    ison(c("SYR", "YEM", "CAF")),
    ison("Western Asia")
  )
  
  lapply(case_list, function(case){expect_that(case, is_a("numeric"))})
})

test_that("ison correct length", {
  
  case1 <- ison(c("AFG", "AFG", "AFG"), vectorized = FALSE)
  expect_equal(length(case1), 1)
  
  case2 <- ison(c("AFG", "AFG", "AFG"), vectorized = TRUE)
  expect_equal(length(case2), 3)
  
})

####ison_region####
test_that("ison_region is numeric", {
  
  case_list <- list(
    ison_region("World"),
    ison_region("GWNO_Asia"),
    ison_region("Western Asia")
  )
  
  lapply(case_list, function(case){expect_that(case, is_a("numeric"))})
})

test_that("ison_region returns isos", {
  
  case_list <- list(
    ison_region("World"),
    ison_region("GWNO_Asia"),
    ison_region("Western Asia")
  )
  
  lapply(case_list, function(case){expect_gt(length(case), 0)})
})

####isoc####
test_that("ison is character", {
  
  case_list <- list(
    isoc(4),
    isoc(899),
    isoc(c(760, 887, 4)),
    isoc(c("AFG", "SYR"))
  )
  
  lapply(case_list, function(case){expect_that(case, is_a("character"))})
})

test_that("ison correct length", {
  
  case1 <- ison(c("AFG", "AFG", "AFG"), vectorized = FALSE)
  expect_equal(length(case1), 1)
  
  case2 <- ison(c("AFG", "AFG", "AFG"), vectorized = TRUE)
  expect_equal(length(case2), 3)
  
})

####cnames####
test_that("cnames works", {
  
  case_list <- list(
    data.frame(),
    data.frame(iso3n = rep(c(4, 760, 887, 899), 100))
  )
  
  lapply(case_list, function(case){ expect_equal(sum(is.na(case)), 0) })
  
})
