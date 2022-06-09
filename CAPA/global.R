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
load_dot_env()

`%!in%` = Negate(`%in%`)

drop_path <- "C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/"
#source("credentials.R")
source("helpers_post.R")

adm1_cgaz <- readRDS("adm1_cgaz_sub.RDS")
ged <- readRDS("ged.RDS")
nid_grid <- readRDS("nid_grid.RDS")

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