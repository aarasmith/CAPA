source("../R/utils.R")

test_that("is numeric", {
  
  normal_iso <- ison("AFG")
  edge_iso <- ison("KOS")
  multi_iso <- ison(c("SYR", "YEM", "CAF"))
  
  expect_that(normal_iso, is_a("numeric"))
  expect_that(edge_iso, is_a("numeric"))
  expect_that(multi_iso, is_a("numeric"))
})
