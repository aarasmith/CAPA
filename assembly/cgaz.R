

#The geoboundaries cgaz data has missing shape names for a number of countries. These are fixed, names standardized (lowercase, underscore) and iso3n is added along with a simplified shape_id
#The capa_id is the iso3n + 100 (to enforce 3 digit length) as a prefix appended to the numbers in the cgaz shapeID after the "B" padded with 0's on the right to end up with an 11 digit ID

drop_path <- "C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/"
adm1_cgaz <- readRDS(paste0(drop_path, "adm1.RDS"))
adm21 <- st_read(paste0(drop_path, "geoBoundariesCGAZ_ADM2.geojson"), options = "ENCODING=UTF8") %>% dplyr::select(-LEVEL, -ISO_CODE, -DistrictCo, -layer, -path, -Level, -shapeType)
adm2 <- readRDS(paste0(drop_path, "adm2.RDS"))


AFG1_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-AFG-ADM1.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, PROV_34_NA)
adm1_cgaz <- left_join(adm1_cgaz, AFG1_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(PROV_34_NA), PROV_34_NA, shapeName)) %>% dplyr::select(-PROV_34_NA)
AFG2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-AFG-ADM2.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, DIST_34_NA)
adm2 <- left_join(adm2, AFG2_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(DIST_34_NA), DIST_34_NA, shapeName)) %>% dplyr::select(-DIST_34_NA)
#JPN2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-JPN-ADM2.geojson"))
NPL2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-NPL-ADM2.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, DISTRICT)
adm2 <- left_join(adm2, NPL2_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(DISTRICT), DISTRICT, shapeName)) %>% dplyr::select(-DISTRICT)
SLE2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-SLE-ADM2.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, admin2Name)
adm2 <- left_join(adm2, SLE2_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(admin2Name), admin2Name, shapeName)) %>% dplyr::select(-admin2Name)
#TKM2_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-TKM-ADM2.geojson"))
AGO1_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-AGO-ADM1.geojson")) %>% st_drop_geometry() %>% dplyr::select(shapeID, ADM1_NAME)
adm1_cgaz <- left_join(adm1_cgaz, AGO1_fix, by = "shapeID") %>% mutate(shapeName = ifelse(!is.na(ADM1_NAME), ADM1_NAME, shapeName)) %>% dplyr::select(-ADM1_NAME)
CHN_fix <- st_read(paste0(drop_path, "adm_fix/geoBoundaries-CHN-ADM2.geojson")) %>% st_drop_geometry() 
#also missing in this data, missing china shape is tiny and therefore dropped
RUS_fix <- readOGR(paste0(drop_path, "adm_fix/geoBoundaries-RUS-ADM2_simplified.shp"), encoding = "UTF-8") %>% st_as_sf() %>% st_drop_geometry()

names(adm1_cgaz) <- c("shape_id", "shape_group", "shape_name", "geometry")
names(adm2) <- c("shape_id", "shape_group", "shape_name", "geometry")

hdr_db_2 <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
countries <- dbGetQuery(hdr_db_2, "SELECT DISTINCT iso3c FROM comb")

adm1_cgaz <- adm1_cgaz %>% filter(shape_group %in% countries$iso3c)
adm2 <- adm2 %>% filter(shape_group %in% countries$iso3c) %>%
  filter(!is.na(shape_name))

sum(is.na(adm1_cgaz$shape_name))
sum(is.na(adm2$shape_name))

#Kosovo assigned an iso3n code of 899
cgazer <- function(cgaz){
  cgaz <- cgaz %>% mutate(iso3n = countrycode(shape_group, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    rowwise() %>%
    mutate(capa_id = as.numeric(str_pad(paste0(iso3n + 100, str_match(shape_id, "B(\\d+)")[2]), 11, side = "right", pad = "0")))
  return(cgaz)
}

adm1_cgaz <- cgazer(adm1_cgaz)
adm2 <- cgazer(adm2)


#HIERARCHY 
country_list <- list()
for(i_country in unique(adm2$shape_group)){
  adm1_c <- filter(adm1_cgaz, shape_group == i_country)
  adm2_c <- filter(adm2, shape_group == i_country)
  adm_join <- st_join(adm2_c, dplyr::select(adm1_c, capa_id), largest = TRUE) %>%
    st_drop_geometry() %>%
    rename(capa_id_adm1 = capa_id.y,
           capa_id_adm2 = capa_id.x)
  country_list[[i_country]] <- adm_join
}
adm_hierarchy <- rbindlist(country_list)
