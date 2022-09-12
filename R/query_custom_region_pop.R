



query_custom_region_pop <- function(isos, years, capa_db){
  
  #' Query the CAPA db for custom regional population totals
  #' 
  #' @description Query the CAPA db for custom regional population totals
  #' 
  #' @usage query_custom_region_pop(isos, year, capa_db)
  #' 
  #' @param isos character: A vector of strings containing the iso3n values for the custom region
  #' @param years a single numeric year or a vector range of years
  #' @param capa_db a database connection to the CAPA database
  #' 
  #' @examples
  #' query_custom_region_pop(c(4, 760, 887), years = 2001, capa_db = con)
  #' query_custom_region_pop(c(ison("Western Asia"), 4, 364), years = 1990:2021, capa_db = con)
  #' 
  #' @details
  #' Dependencies: glue, dplyr, DBI
  #' Used-in: get_custom_region_aggregation()
  
  region_query <- glue_sql(
    "SELECT
    	year,
    	sum(total_pop) AS total_pop
    FROM
    	country_pops
    WHERE
      iso3n IN ({isos*}) AND
      year >= {min(years)} AND
      year <= {max(years)}
    GROUP BY year",
    .con = capa_db
  )
  
  region_pop <- dbGetQuery(capa_db, region_query) %>%
    within(total_pop <- as.numeric(total_pop))
  return(region_pop)
}
