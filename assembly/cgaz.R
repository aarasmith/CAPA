

#The geoboundaries cgaz data has missing shape names for a number of countries. These are fixed, names standardized (lowercase, underscore) and iso3n is added along with a simplified shape_id
#The capa_id is the iso3n + 100 (to enforce 3 digit length) as a prefix appended to the numbers in the cgaz shapeID after the "B" padded with 0's on the right to end up with an 11 digit ID

drop_path <- "C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/"
adm1_cgaz <- readRDS(paste0(drop_path, "adm1.RDS"))
adm2_cgaz <- readRDS(paste0(drop_path, "adm2.RDS"))

#Kosovo assigned an iso3n code of 899
cgazer <- function(cgaz){
  cgaz <- cgaz %>% mutate(iso3n = countrycode(shape_group, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    rowwise() %>%
    mutate(capa_id = as.numeric(str_pad(paste0(iso3n + 100, str_match(shape_id, "B(\\d+)")[2]), 11, side = "right", pad = "0")))
  return(cgaz)
}

fix_adm1_cgaz <- function(adm1_cgaz, drop_path = drop_path){
  
  AFG1_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-AFG-ADM1.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, PROV_34_NA)
  adm1_cgaz <- left_join(adm1_cgaz, AFG1_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(PROV_34_NA), PROV_34_NA, shapeName)) %>% dplyr::select(-PROV_34_NA)
  
  AGO1_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-AGO-ADM1.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, ADM1_NAME)
  adm1_cgaz <- left_join(adm1_cgaz, AGO1_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(ADM1_NAME), ADM1_NAME, shapeName)) %>% dplyr::select(-ADM1_NAME)
  
  names(adm1_cgaz) <- c("shape_id", "shape_group", "shape_name", "geometry")
  
  hdr_db_2 <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
  countries <- dbGetQuery(hdr_db_2, "SELECT DISTINCT iso3c FROM grid")
  
  adm1_cgaz <- adm1_cgaz %>% filter(shape_group %in% countries$iso3c)
  
  adm1_cgaz <- cgazer(adm1_cgaz)
  
  return(adm1_cgaz)
}

fix_adm2_cgaz <- function(adm2_cgaz, drop_path = drop_path){
  
  AFG2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-AFG-ADM2.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, DIST_34_NA)
  adm2_cgaz <- left_join(adm2_cgaz, AFG2_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(DIST_34_NA), DIST_34_NA, shapeName)) %>% dplyr::select(-DIST_34_NA)
  #JPN2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-JPN-ADM2.geojson"))
  NPL2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-NPL-ADM2.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, DISTRICT)
  adm2_cgaz <- left_join(adm2_cgaz, NPL2_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(DISTRICT), DISTRICT, shapeName)) %>% dplyr::select(-DISTRICT)
  SLE2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-SLE-ADM2.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, admin2Name)
  adm2_cgaz <- left_join(adm2_cgaz, SLE2_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(admin2Name), admin2Name, shapeName)) %>% dplyr::select(-admin2Name)
  #TKM2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-TKM-ADM2.geojson"))
  #CHN_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-CHN-ADM2.geojson")) %>% st_drop_geometry() 
  #also missing in this data, missing china shape is tiny and therefore dropped
  #RUS_fix <- readOGR(paste0(drop_path, "adm_fix/geoBoundaries-RUS-ADM2_simplified.shp"), encoding = "UTF-8") %>% st_as_sf() %>% st_drop_geometry()
  #RUS name encoding is bad
  
  names(adm2_cgaz) <- c("shape_id", "shape_group", "shape_name", "geometry")
  
  adm2_cgaz <- adm2_cgaz %>% filter(shape_group %in% countries$iso3c) %>%
    filter(!is.na(shape_name))
 
  adm2_cgaz <- cgazer(adm2_cgaz)
  
  return(adm2_cgaz)
}


#HIERARCHY 
create_adm_hierarchy <- function(adm1_cgaz, adm2_cgaz){
  country_list <- list()
  for(i_country in unique(adm2$shape_group)){
    adm1_c <- filter(adm1_cgaz, shape_group == i_country)
    adm2_c <- filter(adm2_cgaz, shape_group == i_country)
    adm_join <- st_join(adm2_c, dplyr::select(adm1_c, capa_id), largest = TRUE) %>%
      st_drop_geometry() %>%
      rename(capa_id_adm1 = capa_id.y,
             capa_id_adm2 = capa_id.x)
    country_list[[i_country]] <- adm_join
  }
  
  adm_hierarchy <- rbindlist(country_list)
  
  return(adm_hierarchy)
}
