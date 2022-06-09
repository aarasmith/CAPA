library(shiny)

#hdr_db <- dbConnect(RSQLite::SQLite(), "hdr_db_v2.sqlite")
#hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = post_host, dbname = "HDR", user = post_user, password = post_pass, port = post_port)
hdr_db <- connect_to_hdr_db()
ui <- fluidPage(
  useShinyjs(),
  titlePanel("HDR"),
  
    tabsetPanel(
      
      ####LONG PANEL####
      
      tabPanel("Exposure", fluid = TRUE,
        sidebarLayout(
                 
          sidebarPanel(style = "height: 95vh; overflow-y: auto;",
            
            tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                              Shiny.onInputChange("innerWidth", window.innerWidth);
                              });
                              $(window).resize(function(e) {
                              Shiny.onInputChange("innerWidth", window.innerWidth);
                              });
                              ')),
                   
            textOutput("info"),
            selectInput("country", "Select Country", choices = sort(dbGetQuery(hdr_db, "SELECT DISTINCT iso3c FROM grid")$iso3c)),
            sliderInput("year_slider", "Year Range:", min = 1990, max = 2020, value = c(1990, 2020), sep = ""),
            radioButtons(inputId = "monthly", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
            radioButtons(inputId = "adm", label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
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
            ),
            textInput(inputId = "threshold", label = "Intensity Threshold"),
            actionButton(inputId = "submit", label = "Submit"),
            downloadButton("download", label = "Download Table")
          ),
          mainPanel(
            dataTableOutput("Table")
          )
        )
      ),
      
      ####LONG MAP PANEL####
      
      tabPanel("Exposure Map", fluid = TRUE,
               sidebarLayout(
                 
                 sidebarPanel(style = "height: 95vh; overflow-y: auto;",
                              selectInput("country_long_map", "Select Country", choices = sort(dbGetQuery(hdr_db, "SELECT DISTINCT iso3c FROM grid")$iso3c)),
                              numericInput("year_long_map", "Year:", min = 1990, max = 2020, value = 1990, step = 1),
                              radioButtons(inputId = "monthly_long_map", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
                              numericInput("month_long_map", "Month(if monthly selected):", min = 1, max = 12, value = 1, step = 1),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "L25_weight_long_map", label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L50_weight_long_map", label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L100_weight_long_map", label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "M25_weight_long_map", label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M50_weight_long_map", label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M100_weight_long_map", label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "H25_weight_long_map", label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H50_weight_long_map", label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H100_weight_long_map", label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "int25_weight_long_map", label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int50_weight_long_map", label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int100_weight_long_map", label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
                              ),
                              numericInput(inputId = "threshold_long_map", label = "Intensity Threshold", value = 1, min = 1, max = 1000000, step = 1),
                              actionButton(inputId = "submit_long_map", label = "Submit"),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "font_size_long_map", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                                          numericInput(inputId = "legend_size_long_map", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
                              ),
                              downloadButton("download_long_map", label = "Download Table")
                 ),
                 mainPanel(
                   plotOutput("long_map")
                 )
               )
      ),
      
      #####SCORE MAP PANEL####
      
      tabPanel("Score Map", fluid = TRUE,
        sidebarLayout(
          sidebarPanel(style = "height: 95vh; overflow-y: auto;",
            selectInput("country_map", "Select Country", choices = sort(dbGetQuery(hdr_db, "SELECT DISTINCT iso3c FROM grid")$iso3c)),
            sliderInput("year_slider_map", "Year Range:", min = 1990, max = 2020, value = c(1990, 2020), sep = ""),
            splitLayout(cellArgs = list(style='white-space: normal;'),
                        numericInput(inputId = "start_map", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                        numericInput(inputId = "stop_map", label = "End Month", value = 12, min = 1, max = 12, step = 1)
            ),
            splitLayout(cellArgs = list(style='white-space: normal;'),
                        numericInput(inputId = "L25_weight_map", label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                        numericInput(inputId = "L50_weight_map", label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                        numericInput(inputId = "L100_weight_map", label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
            ),
            splitLayout(cellArgs = list(style='white-space: normal;'),
                        numericInput(inputId = "M25_weight_map", label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                        numericInput(inputId = "M50_weight_map", label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                        numericInput(inputId = "M100_weight_map", label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
            ),
            splitLayout(cellArgs = list(style='white-space: normal;'),
                        numericInput(inputId = "H25_weight_map", label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                        numericInput(inputId = "H50_weight_map", label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                        numericInput(inputId = "H100_weight_map", label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
            ),
            splitLayout(cellArgs = list(style='white-space: normal;'),
                        numericInput(inputId = "int25_weight_map", label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                        numericInput(inputId = "int50_weight_map", label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                        numericInput(inputId = "int100_weight_map", label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
            ),
            checkboxInput(inputId = "draw_points_map", label = "show conflict events"),
            actionButton(inputId = "submit_map", "generate map"),
            splitLayout(cellArgs = list(style='white-space: normal;'),
                        numericInput(inputId = "font_size_map", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                        numericInput(inputId = "legend_size_map", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
            )
          ),
          mainPanel(
            plotOutput("map")
          )
        )
      ),
      
      ####DURATION PANEL####
      
      tabPanel("duration", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(style = "height: 95vh; overflow-y: auto;",
                              selectInput("country_dur", "Select Country", choices = sort(dbGetQuery(hdr_db, "SELECT DISTINCT iso3c FROM grid")$iso3c)),
                              sliderInput("year_slider_dur", "Year Range:", min = 1990, max = 2020, value = c(1990, 2020), sep = ""),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "start_dur", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                                          numericInput(inputId = "stop_dur", label = "End Month", value = 12, min = 1, max = 12, step = 1)
                              ),
                              radioButtons(inputId = "period_dur", label = "Period size", choices = c("year", "half", "quarter", "month"), inline = T),
                              radioButtons(inputId = "adm_dur", label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "L25_weight_dur", label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L50_weight_dur", label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L100_weight_dur", label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "M25_weight_dur", label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M50_weight_dur", label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M100_weight_dur", label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "H25_weight_dur", label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H50_weight_dur", label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H100_weight_dur", label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "int25_weight_dur", label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int50_weight_dur", label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int100_weight_dur", label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
                              ),
                              textInput(inputId = "threshold_dur", label = "Intensity Threshold"),
                              actionButton(inputId = "submit_dur", "submit"),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "font_size_dur", label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                                          numericInput(inputId = "legend_size_dur", label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
                              ),
                              downloadButton("download_dur", label = "Download Table")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("dur_table", fluid = TRUE,
                              dataTableOutput("duration")
                     ),
                     tabPanel("dur_map", fluid = TRUE,
                              plotOutput("duration_map"))
                   )
                   
                 )
               )
      ),
      
      ####FREQUENCY PANEL####
      
      tabPanel("frequency", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(style = "height: 95vh; overflow-y: auto;",
                              selectInput("country_freq", "Select Country", choices = sort(dbGetQuery(hdr_db, "SELECT DISTINCT iso3c FROM grid")$iso3c)),
                              sliderInput("year_slider_freq", "Year Range:", min = 1990, max = 2020, value = c(1990, 2020), sep = ""),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "start_freq", label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                                          numericInput(inputId = "stop_freq", label = "End Month", value = 12, min = 1, max = 12, step = 1)
                              ),
                              radioButtons(inputId = "period_freq", label = "Period size", choices = c("year", "half", "quarter", "month"), inline = T),
                              radioButtons(inputId = "adm_freq", label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "L25_weight_freq", label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L50_weight_freq", label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L100_weight_freq", label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "M25_weight_freq", label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M50_weight_freq", label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M100_weight_freq", label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "H25_weight_freq", label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H50_weight_freq", label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H100_weight_freq", label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "int25_weight_freq", label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int50_weight_freq", label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int100_weight_freq", label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
                              ),
                              numericInput(inputId = "threshold_freq", label = "Intensity Threshold", value = 1, min = 1, max = 1000000, step = 1),
                              actionButton(inputId = "submit_freq", "submit"),
                              downloadButton("download_freq", label = "Download Table")
                 ),
                 mainPanel(
                   dataTableOutput("frequency")
                 )
               )
      ),
      
      ####LONG PANEL####
      
      tabPanel("Global Exposure", fluid = TRUE,
               sidebarLayout(
                 
                 sidebarPanel(style = "height: 95vh; overflow-y: auto;",
                              selectInput("region_global", "Select Region", choices = c("World", sort(dbGetQuery(hdr_db, "SELECT DISTINCT region FROM region_key")$region))),
                              sliderInput("year_slider_global", "Year Range:", min = 1990, max = 2020, value = c(1990, 2020), sep = ""),
                              radioButtons(inputId = "monthly_global", label = "Period size", choiceNames = c("Year", "Month"), choiceValues = c(FALSE, TRUE), inline = T),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "L25_weight_global", label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L50_weight_global", label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "L100_weight_global", label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "M25_weight_global", label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M50_weight_global", label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "M100_weight_global", label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "H25_weight_global", label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H50_weight_global", label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "H100_weight_global", label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
                              ),
                              splitLayout(cellArgs = list(style='white-space: normal;'),
                                          numericInput(inputId = "int25_weight_global", label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int50_weight_global", label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                                          numericInput(inputId = "int100_weight_global", label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
                              ),
                              textInput(inputId = "threshold_global", label = "Intensity Threshold"),
                              actionButton(inputId = "submit_global", label = "Submit"),
                              downloadButton("download_global", label = "Download Table")
                 ),
                 mainPanel(
                   dataTableOutput("global_table")
                 )
               )
      )
      
    )
)
ui <- secure_app(ui)
#disconnect_from_hdr_db(hdr_db)