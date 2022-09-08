



query_total_pop <- function(iso3n, adm1, years, capa_db){

  #' Query the CAPA db for ADM0 and ADM1 population totals
  #' 
  #' @description Query the CAPA db for ADM0 and ADM1 population totals
  #' 
  #' @param iso3n a single numeric iso3n or a vector of multiple iso3n numeric values
  #' @param adm1 boolean - if FALSE will use ADM0
  #' @param years a single numeric year or a vector range of years
  #' @param capa_db a database connection to the CAPA database
  #' 
  #' @usage
  #' query_total_pop(iso3n = 760, adm1 = FALSE, years = 1990, capa_db = conn)
  #' query_total_pop(iso3n = c(760, 4, 887), adm1 = TRUE, years = 1990:2021, capa_db = conn)
  #' 
  #' @details
  #' Dependencies: glue, dplyr, DBI
  
  
  if(adm1){
    table <- "adm1_pops"
  }else{
    table <- "country_pops"
  }
  
  total_query <- glue_sql(
    "SELECT *
    FROM {`table`}
    WHERE iso3n IN ({iso3n*}) AND
      year >= {min(years)} AND
      year <= {max(years)}",
  .con = capa_db)
  
  total_pop <- dbGetQuery(capa_db, total_query) %>% mutate(across(.fns = ~as.numeric(.)))
  return(total_pop)
}
