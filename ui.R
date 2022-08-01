library(shiny)

header <- dashboardHeader(title = "Conflict Affected Populations App (CAPA)", titleWidth = 400)

sidebar <- dashboardSidebar(width = 400, tags$style(type='text/css', '#info_custom {white-space: pre-wrap;}'),
  sidebarMenu(
    menuItem("Weights", tabName = "weights", icon = icon("weight-hanging"),
             selectInput("preset", "Select preset weights", choices = names(weight_presets_list)),
             actionButton("apply_preset", "Apply preset weights"),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "L25_weight", label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                         numericInput(inputId = "L50_weight", label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                         numericInput(inputId = "L100_weight", label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
             ),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "M25_weight", label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                         numericInput(inputId = "M50_weight", label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                         numericInput(inputId = "M100_weight", label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
             ),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "H25_weight", label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                         numericInput(inputId = "H50_weight", label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                         numericInput(inputId = "H100_weight", label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
             ),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "int25_weight", label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                         numericInput(inputId = "int50_weight", label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                         numericInput(inputId = "int100_weight", label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
             )
    ),
    menuItem("Conflict Exposure", tabName = "std_exposure", icon = icon("users"),
             textOutput("info"),
             selectInput("country", "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
             sliderInput("year_slider", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
             radioButtons(inputId = "monthly", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
             radioButtons(inputId = "adm", label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
             numericInput(inputId = "threshold", label = "Intensity Threshold", value = 1, min = 1, step = 1),
             actionButton(inputId = "submit", label = "Submit"),
             downloadButton("download", label = "Download Table")
    ),
    menuItem("Exposure Map", tabName = "exposure_map", icon = icon("globe"),
             textOutput("info_long_map"),
             selectInput("country_long_map", "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
             numericInput("year_long_map", "Year:", min = 1990, max = 2021, value = 1990, step = 1),
             radioButtons(inputId = "monthly_long_map", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
             numericInput("month_long_map", "Month(if monthly selected):", min = 1, max = 12, value = 1, step = 1),
             numericInput(inputId = "threshold_long_map", label = "Intensity Threshold", value = 1, min = 1, max = 1000000, step = 1),
             actionButton(inputId = "submit_long_map", label = "Submit"),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "font_size_long_map", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                         numericInput(inputId = "legend_size_long_map", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
             ),
             downloadButton("download_exposure_map", label = "Download Plot")
    ),
    menuItem("Score Map", tabName = "score_map", icon = icon("border-all"),
             textOutput("info_map"),
             selectInput("country_map", "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
             sliderInput("year_slider_map", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "start_map", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                         numericInput(inputId = "stop_map", label = "End Month", value = 12, min = 1, max = 12, step = 1)
             ),
             checkboxInput(inputId = "draw_adm1_map", label = "show ADM1 boundaries"),
             checkboxInput(inputId = "draw_points_map", label = "show conflict events"),
             actionButton(inputId = "submit_map", "generate map"),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "font_size_map", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                         numericInput(inputId = "legend_size_map", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
             ),
             downloadButton("download_score_map", label = "Download Plot")
    ),
    menuItem("Conflict Duration", tabName = "duration", icon = icon("stopwatch"),
             textOutput("info_dur"),
             selectInput("country_dur", "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
             sliderInput("year_slider_dur", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "start_dur", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                         numericInput(inputId = "stop_dur", label = "End Month", value = 12, min = 1, max = 12, step = 1)
             ),
             radioButtons(inputId = "monthly_dur", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
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
             textOutput("info_freq"),
             selectInput("country_freq", "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
             sliderInput("year_slider_freq", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "start_freq", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                         numericInput(inputId = "stop_freq", label = "End Month", value = 12, min = 1, max = 12, step = 1)
             ),
             radioButtons(inputId = "monthly_freq", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
             radioButtons(inputId = "adm_freq", label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
             numericInput(inputId = "threshold_freq", label = "Intensity Threshold", value = 1, min = 1, max = 1000000, step = 1),
             radioButtons(inputId = "p_thresh_logic", label = "Use a period threshold?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), inline = T),
             textOutput("p_thresh_max"),
             numericInput(inputId = "period_threshold", label = "Period Threshold", value = 1, min = 1, step = 1),
             actionButton(inputId = "submit_freq", "submit"),
             radioButtons(inputId = "output_type_freq", label = "Output type", choices = c("Table", "Map"), inline = T),
             splitLayout(cellArgs = list(style='white-space: normal;'),
                         numericInput(inputId = "font_size_freq", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                         numericInput(inputId = "legend_size_freq", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
             ),
             downloadButton("download_freq", label = "Download Table")
    ),
    menuItem("Regional Aggregation", tabName = "regional_aggregation", icon = icon("layer-group"),
             tabsetPanel(
               tabPanel("Parameters", fluid = TRUE, style = "height: 95vh; overflow-y: auto;",
                        textOutput("info_global"),
                        selectInput("region_global", "Select Region", choices = c("World", region_choices, "Custom Region")),
                        sliderInput("year_slider_global", "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
                        radioButtons(inputId = "monthly_global", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
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
    )
  )
)

body <- dashboardBody(uiOutput("body_plot"))

ui <- dashboardPage(header, sidebar, body, useShinyjs())