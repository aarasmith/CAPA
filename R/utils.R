



ison <- function(iso3c, vectorized = FALSE){
  individual_isos <- iso3c[str_length(iso3c) == 3]
  iso3n <- countrycode(individual_isos, origin = "iso3c", destination = "iso3n", custom_match = c("KOS" = 899))
  #iso3n <- ifelse(is.na(iso3n), 899, iso3n)
  
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
