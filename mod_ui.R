library(shiny)
#source("R/mod.R")

header <- dashboardHeader(title = "Conflict Affected Populations App (CAPA)", titleWidth = 400)

sidebar <- dashboardSidebar(width = 400, tags$style(type='text/css', '#info_custom {white-space: pre-wrap;}'),
                            sidebarMenu(id = "tab_selection", style = "white-space: normal;",
                                        menuItem("Info", tabName = "homepage", icon = icon("home"),
                                                 mod_info_ui("info")
                                        ),
                                        menuItem("Weights", tabName = "weights", icon = icon("weight-hanging"),
                                                 mod_weights_ui("weights")
                                        ),
                                        menuItem("Conflict Exposure", tabName = "std_exposure", icon = icon("users"),
                                                 mod_exposure_ui("exposure")
                                        ),
                                        menuItem("Exposure Map", tabName = "exposure_map", icon = icon("globe"),
                                                 mod_exposure_map_ui("exposure_map")
                                        ),
                                        menuItem("Score Map", tabName = "score_map", icon = icon("border-all"),
                                                 mod_score_map_ui("score_map")
                                        ),
                                        menuItem("Conflict Duration", tabName = "duration", icon = icon("stopwatch"),
                                                 textOutput("info_dur"),
                                                 selectInput("country_dur", "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
                                                 sliderInput("year_slider_dur", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
                                                 splitLayout(cellArgs = list(style='white-space: normal;'),
                                                             numericInput(inputId = "start_dur", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                                                             numericInput(inputId = "stop_dur", label = "End Month", value = 12, min = 1, max = 12, step = 1)
                                                 ),
                                                 radioButtons(inputId = "period_dur", label = "Period size", choices = c("yearly", "biannually", "quarterly", "monthly"), inline = T),
                                                 radioButtons(inputId = "adm_dur", label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
                                                 numericInput(inputId = "threshold_dur", label = "Intensity Threshold", value = 1, min = 1, step = 1),
                                                 actionButton(inputId = "submit_dur", "submit"),
                                                 radioButtons(inputId = "output_type_dur", label = "Output type", choices = c("Table", "Map"), inline = T),
                                                 splitLayout(cellArgs = list(style='white-space: normal;'),
                                                             numericInput(inputId = "font_size_dur", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                                                             numericInput(inputId = "legend_size_dur", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
                                                 ),
                                                 downloadButton("download_dur", label = "Download Output")
                                        ),
                                        menuItem("Conflict Frequency", tabName = "frequency", icon = icon("wave-square"),
                                                 mod_frequency_ui("frequency")
                                        ),
                                        menuItem("Regional Aggregation", tabName = "regional_aggregation", icon = icon("layer-group"),
                                                 tabsetPanel(
                                                   tabPanel("Parameters", fluid = TRUE, style = "height: 95vh; overflow-y: auto;",
                                                            textOutput("info_global"),
                                                            selectInput("region_global", "Select Region", choices = c("World", region_choices, "Custom Region")),
                                                            sliderInput("year_slider_global", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
                                                            radioButtons(inputId = "period_global", label = "Period size", choices = c("yearly", "biannually", "quarterly", "monthly"), inline = T),
                                                            numericInput(inputId = "threshold_global", label = "Intensity Threshold", min = 1, value = 1, step = 1),
                                                            actionButton(inputId = "submit_global", label = "Submit"),
                                                            downloadButton("download_global", label = "Download Table")
                                                   ),
                                                   tabPanel("Custom Region", fluid = TRUE,
                                                            style = "height: 95vh; overflow-y: auto;",
                                                            verbatimTextOutput("info_custom"),
                                                            selectInput("region_custom", "Select Region", choices = c("World", region_choices)),
                                                            selectInput("region_add", "Countries to add", choices = all_country_choices, multiple = T),
                                                            selectInput("region_subtract", "Countries to remove", choices = all_country_choices, multiple = T),
                                                            actionButton(inputId = "submit_custom", label = "Display custom region")
                                                            
                                                   )
                                                 )
                                        ),
                                        menuItem("Children at Risk", tabName = "children_at_risk", icon = icon("child"),
                                                 textOutput("info_car"),
                                                 selectInput("country_car", "Select Country/Region", choices = c(country_choices, "World", region_choices), multiple = T, selected = "World"),
                                                 sliderInput("year_slider_car", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
                                                 selectizeInput("categories_car", "Select Category Labels", choices = c("low", "medium", "high", "extreme"), selected = c("low", "medium", "high", "extreme"),
                                                                multiple = TRUE, options = list(create = TRUE)),
                                                 selectizeInput("scores_car", "Select Category Lower-Bounds", choices = c(1, 25, 100, 1000), selected = c(1, 25, 100, 1000), multiple = TRUE, options = list(create = TRUE)),
                                                 radioButtons(inputId = "exclusive_car", label = "Category Types", choiceNames = c("Inclusive", "Exclusive"), choiceValues = c(FALSE, TRUE), selected = TRUE, inline = T),
                                                 radioButtons(inputId = "level_car", label = "Aggregation Level", choices = c("Country", "Region", "Global"), selected = "Country", inline = T),
                                                 actionButton(inputId = "submit_car", label = "Submit"),
                                                 downloadButton("download_car", label = "Download Table"))
                            )
)

body <- dashboardBody(verbatimTextOutput("weight_system"),
                      uiOutput("body_plot"))

ui <- dashboardPage(header, sidebar, body, useShinyjs())