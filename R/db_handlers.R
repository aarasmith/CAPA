



connect_to_capa <- function(){
  #hdr_db <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
  #hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = post_host, dbname = "HDR", user = post_user, password = post_pass, port = post_port)
  capa_db <- dbConnect(drv = RPostgres::Postgres(), host = prio_host, dbname = "capa", user = prio_user, password = prio_pass, port = prio_port)
  return(capa_db)
}
disconnect_from_capa <- function(x){
  dbDisconnect(x)
}