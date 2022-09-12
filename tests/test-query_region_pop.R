source("../R/query_total_pop.R")

#query_region_pop(region, years, capa_db)

#GWNO_Africa

test_that("query_region_pop multi-year", {
  
  #inputs
  #region <- "World"
  years <- 1990:2021
  capa_db <- connect_to_capa()
  
  case_list <- list(
    query_region_pop("World", years = years, capa_db),
    query_region_pop("Western Asia", years = years, capa_db)
  )
  
  lapply(case_list, function(case){expect_equal(nrow(case), length(years))})
  lapply(case_list, function(case){expect_equal(sum(is.na(case)), 0)})
  
})

test_that("query_region_pop single-year", {
  
  years <- 2000
  capa_db <- connect_to_capa()
  
  case_list <- list(
    query_region_pop("World", years = years, capa_db),
    query_region_pop("Western Asia", years = years, capa_db)
  )
  
  lapply(case_list, function(case){expect_equal(nrow(case), length(years))})
  lapply(case_list, function(case){expect_equal(sum(is.na(case)), 0)})
  
})
