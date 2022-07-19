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
load_dot_env("CAPA/.env")

`%!in%` = Negate(`%in%`)

sf_use_s2(FALSE)

drop_path <- "C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/"
#source("credentials.R")
source("CAPA/query_funcs.R")
source("CAPA/app_funcs.R")

adm1_cgaz <- readRDS("data/adm1_cgaz.RDS")
country_choices <- sort(unique(adm1_cgaz$shape_group))
capa_db <- connect_to_capa()
region_choice <- sort(dbGetQuery(capa_db, "SELECT DISTINCT region FROM region_key")$region)
ged <- readRDS("data/ged.RDS")
nid_grid <- readRDS("data/nid_grid.RDS")

credentials <- data.frame(
  user = c(Sys.getenv("USER1")), # mandatory
  password = c(Sys.getenv("USER1_PASSWORD")),
  admin = c(FALSE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

aws_host <- Sys.getenv("AWS_DB_HOST")
aws_user <- Sys.getenv("AWS_DB_USER")
aws_pass <- Sys.getenv("AWS_DB_PASSWORD")
aws_port <- Sys.getenv("AWS_DB_PORT")

prio_host <- Sys.getenv("PRIO_DB_HOST")
prio_user <- Sys.getenv("PRIO_DB_USER")
prio_pass <- Sys.getenv("PRIO_DB_PASSWORD")
prio_port <- Sys.getenv("PRIO_DB_PORT")

disconnect_from_capa(capa_db)
