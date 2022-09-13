



sanitize_weights <- function(x){
  
  weight_list <- lapply(x, function(x){if(!is.numeric(x)){return(0)}else{return(x)}})
  #weight_list <- lapply(x, function(x){if(!is.numeric(x)){return(0)}else{return(round(x))}}) to be implemented when can be tested
  return(weight_list)
  
}

sanitize_threshold <- function(x){
  if(!is.numeric(x)){return(0)}else{return(x)}
  #if(!is.numeric(x)){return(0)}else{return(round(x))} to be implemented when can be tested
}

sanitize_iso <- function(iso){
  if(is.numeric(iso)){
    iso3n <- iso
  }else{
    iso3n <- c()
    for(i in iso){
      if(grepl("\\d+", i)){
        iso3n <- append(iso3n, i)
      }else{
        iso3n <- append(iso3n, ison(i))
      }
    }
  }
  
  return(iso3n %>% as.numeric())
}
