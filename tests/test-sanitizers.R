source("../R/utils-sanitizers.R")



test_that("sanitize_iso", {
  
  case_list <- list(
    "SYR",
    760,
    c("SYR", "AFG"),
    760, 4,
    c("SYR", 4),
    c("SYR", 4, 887, "Western Asia")
  )
  
  lapply(case_list, function(case){ expect_that(sanitize_iso(case), is_a("numeric")) })
  
})
