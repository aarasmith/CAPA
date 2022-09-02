library(shiny)
library(shinyjs)
#library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(RPostgres)
library(sf)
library(data.table)
library(ggplot2)
library(glue)
library(RSQLite)
library(writexl)
#library(leaflet)
library(shinymanager)
library(dotenv)
library(countrycode)
library(shinydashboard)
library(markdown)
load_dot_env()

`%!in%` = Negate(`%in%`)

sf_use_s2(FALSE)

drop_path <- "C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/"
#source("credentials.R")
source("R/app_helpers/query_funcs.R")
source("R/app_helpers/app_funcs.R")
#source("manual_regions.R")
manual_regions <- readRDS("data/manual_regions.RDS")
#source("mod.R")

aws_host <- Sys.getenv("AWS_DB_HOST")
aws_user <- Sys.getenv("AWS_DB_USER")
aws_pass <- Sys.getenv("AWS_DB_PASSWORD")
aws_port <- Sys.getenv("AWS_DB_PORT")

prio_host <- Sys.getenv("PRIO_DB_HOST")
prio_user <- Sys.getenv("PRIO_DB_USER")
prio_pass <- Sys.getenv("PRIO_DB_PASSWORD")
prio_port <- Sys.getenv("PRIO_DB_PORT")

adm1_cgaz <- readRDS("data/adm1_cgaz.RDS")
adm0_cgaz <- readRDS("data/adm0_cgaz.RDS")
country_choices <- sort(c(unique(adm1_cgaz$shape_group), "PSE"))
nid_grid <- readRDS("data/nid_grid.RDS")
all_country_choices <- sort(unique(nid_grid$ISOCODE))
capa_db <- connect_to_capa()
region_choices <- c(sort(dbGetQuery(capa_db, "SELECT DISTINCT region FROM region_key")$region), names(manual_regions))
ged <- readRDS("data/ged22.RDS")
un_demos <- readRDS("data/un_demos.RDS")
un_geoscheme <- readRDS("data/un_geoscheme.RDS")


credentials <- data.frame(
  user = c(Sys.getenv("USER1")), # mandatory
  password = c(Sys.getenv("USER1_PASSWORD")),
  admin = c(FALSE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

weight_presets_list <- list()
weight_presets_list[['events_100km_unweighted']] <- c(rep(1, 3), rep(0, 3), rep(1, 6))
weight_presets_list[['events_50km_unweighted']] <- c(0, 1, 1, rep(0, 4), 1, 1, 0, 1, 1)
weight_presets_list[['events_25km_unweighted']] <- c(0, 1, rep(0, 5), 1, 0, 0, 1, 0)
weight_presets_list[['events_100km_dist_weighted']] <- c(1, 4, 2, 0, 0, 0, 1, 4, 2, 1, 4, 2)
weight_presets_list[['events_50km_dist_weighted']] <- c(0, 2, 1, 0, 0, 0, 0, 2, 1, 0, 2, 1)
weight_presets_list[['events_100km_int_weighted']] <- c(4, 4, 4, 0, 0, 0, 1, 1, 1, 2, 2, 2)
weight_presets_list[['events_50km_int_weighted']] <- c(0, 4, 4, 0, 0, 0, 0, 1, 1, 0, 2, 2)
weight_presets_list[['events_25km_int_weighted']] <- c(0, 4, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0)
weight_presets_list[['events_100km_both_weighted']] <- c(4, 16, 8, 0, 0, 0, 1, 4, 2, 2, 8, 4)
weight_presets_list[['events_50km_both_weighted']] <- c(0, 8, 4, 0, 0, 0, 0, 2, 1, 0, 4, 2)
weight_presets_list[['int_100km_weighted']] <- c(rep(0, 3), 1, 4, 2, rep(0, 6))
weight_presets_list[['int_50km_weighted']] <- c(rep(0, 3), 0, 2, 1, rep(0, 6))
weight_presets_list[['int_100km_unweighted']] <- c(rep(0, 3), 1, 1, 1, rep(0, 6))
weight_presets_list[['int_50km_unweighted']] <- c(rep(0, 3), 0, 1, 1, rep(0, 6))
weight_presets_list[['int_25km_unweighted']] <- c(rep(0, 3), 0, 1, 0, rep(0, 6))

default_car_weights <- list(L25 = 0, L50 = 0, L100 = 0, M25 = 0, M50 = 0, M100 = 0, H25 = 0, H50 = 0, H100 = 0,
                int25 = 1, int50 = 1, int100 = 0)




disconnect_from_capa(capa_db)
