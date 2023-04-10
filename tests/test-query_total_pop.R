source("../R/query_total_pop.R")



test_that("query_total_pop", {
  
  #inputs
  iso3n <- ison("World")
  years <- 1990:2021
  capa_db <- connect_to_capa()
  adm1_count <- dbGetQuery(capa_db, "SELECT COUNT(DISTINCT capa_id_adm1) FROM adm1_pops")
  
  test_adm0 <- query_total_pop(iso3n, adm1 = FALSE, years, capa_db)
  test_adm1 <- query_total_pop(iso3n, adm1 = TRUE, years, capa_db) %>% filter(!is.na(capa_id_adm1))
  
  expect_equal(
    nrow(test_adm0),
    length(years) * length(iso3n)
  )
  expect_equal(
    nrow(test_adm1),
    length(years) * as.numeric(adm1_count$count)
  )
  
})
