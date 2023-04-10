



ison <- function(iso3c, vectorized = FALSE){
  
  #' Convert iso3c to iso3n
  #' 
  #' @description wrapper for countrycode::Countrycode() to convert iso3c or exact region names into vectors of iso3n
  #' 
  #' @usage ison(iso3c, vectorized = FALSE)
  #' 
  #' @param iso3c character: a single or vector of character iso3c values and/or exact region names from the UN geoscheme or GWNO definition
  #' @param vectorized boolean: if FALSE removes duplicated iso3n values from output
  #' 
  #' @examples 
  #' 
  #' ison("SYR")
  #' ison(c("AFG", "Africa"))
  #' ison(df$iso3c, vectorized = TRUE)
  #'                          
  #' @returns numeric vector
  #' 
  #' @details 
  #' Dependencies: countrycode
  #' Uses: ison_region()
  
  individual_isos <- iso3c[str_length(iso3c) == 3]
  iso3n <- countrycode(individual_isos, origin = "iso3c", destination = "iso3n", custom_match = c("KOS" = 899))
  iso3n <- ifelse(is.na(iso3n), 899, iso3n)
  
  regions <- iso3c[str_length(iso3c) > 3]
  for(region in regions){
    iso3n <- append(iso3n, ison_region(region))
  }
  if(vectorized){
    return(iso3n)
  }else{
    return(unique(iso3n))
  }
}

ison_region <- function(region){
  
  #' Convert region to iso3n
  #' 
  #' @description wrapper for countrycode::Countrycode() to convert exact region names into vectors of iso3n
  #' 
  #' @usage ison_region(region)
  #' 
  #' @param region character: a single character string of an exact region name from the UN geoscheme or GWNO definition
  #' @param vectorized boolean: if FALSE removes duplicated iso3n values from output
  #' 
  #' @examples 
  #' 
  #' ison_region("Western Asia")
  #' ison_region("World")
  #' ison_region("GWNO_Asia")
  #'                          
  #' @returns numeric vector
  #' 
  #' @details 
  #' Dependencies: countrycode, DBI, glue
  #' Uses: connect_to_capa(), disconnect_from_capa()
  #' Used-in: ison()
  
  capa_db <- connect_to_capa()
  if(region == "World"){
    iso3n <- unique(dbGetQuery(capa_db, glue("SELECT * FROM region_key"))$iso3n)
  }else if(any(grepl("GWNO", region))){
    iso3n <- manual_regions[[region]]
  }else{
    iso3n <- dbGetQuery(capa_db, glue("SELECT * FROM region_key WHERE region = '{region}'"))$iso3n
  }
  disconnect_from_capa(capa_db)
  return(iso3n %>% as.numeric())
}

isoc <- function(iso3n){
  
  #' Convert iso3n to iso3c
  #' 
  #' @description wrapper for countrycode::Countrycode() to convert iso3n to iso3c
  #' 
  #' @usage isoc(iso3n)
  #' 
  #' @param iso3n numeric: a single or vector of numeric iso3n values. If character iso3c values are input they will return unchanged provided
  #' it is not a mixed vector of numeric/character
  #' 
  #' @examples 
  #' 
  #' isoc(760)
  #' isoc(c(760, 4))
  #' isoc(ison("Western Asia"))
  #'                          
  #' @returns character vector
  #' 
  #' @details 
  #' Dependencies: countrycode
  
  if(is.character(iso3n)){
    return(iso3n)
  }
  iso3c <- countrycode(iso3n, origin = "iso3n", destination = "iso3c", custom_match = c("899" = "KOS"))
  iso3c <- ifelse(is.na(iso3c), "KOS", iso3c)
  return(iso3c)
}

cnames <- function(df){
  
  #' Add iso3c and country names to dataframe
  #' 
  #' @description Takes a dataframe with an 'iso3n' column and adds iso3c and the country name as the first 2 columns
  #' 
  #' @usage cnames(df)
  #' 
  #' @param df dataframe: a dataframe with an 'iso3n' column
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: countrycode, dplyr
  #' Uses: isoc()
  #' Used-in: get functions
  
  if(nrow(df) > 0){
    df <- df %>%
      within(iso3c <- isoc(iso3n)) %>%
      within(country <- countrycode(iso3c, origin = "iso3c", destination = "country.name", custom_match = c("KOS" = "Kosovo"))) %>%
      dplyr::select(country, iso3c, everything())
    return(df)
  }else{
    return(df)
  }
}