



query_region_pop <- function(region, years, capa_db){
  
  #' Query the CAPA db for regional population totals
  #' 
  #' @description Query the CAPA db for regional population totals
  #' 
  #' @param region character: A single string matching one of the regions in the 'region_pops' materialized view
  #' @param years a single numeric year or a vector range of years
  #' @param capa_db a database connection to the CAPA database
  #' 
  #' @usage
  #' query_region_pop("World", years = 2001, capa_db = con)
  #' query_region_pop("Western Asia", years = 1990:2021, capa_db = con)
  #' 
  #' @details
  #' Dependencies: glue, DBI
  #' Used-in: get_region_aggregation()
  
  region_query <- glue_sql(
    "SELECT
    	region,
    	year,
    	total_pop
    FROM
    	region_pops
    WHERE
      region = {region} AND
      year >= {min(years)} AND
      year <= {max(years)}",
    .con = capa_db
  )
  
  region_pop <- dbGetQuery(capa_db, region_query)
  return(region_pop)
}
