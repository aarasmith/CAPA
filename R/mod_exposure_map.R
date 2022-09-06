


mod_exposure_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    selectInput(ns("country"), "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
    numericInput(ns("year"), "Year:", min = 1990, max = 2021, value = 1990, step = 1),
    radioButtons(inputId = ns("period"), label = "Period size", choices = c("yearly", "biannually", "quarterly", "monthly"), inline = T),
    numericInput(ns("selected_period"), "Period (if less than yearly selected):", min = 1, max = 12, value = 1, step = 1),
    radioButtons(inputId = ns("adm"), label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
    numericInput(inputId = ns("threshold"), label = "Intensity Threshold", value = 1, min = 1, max = 1000000, step = 1),
    actionButton(inputId = ns("submit"), label = "Submit"),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("font_size"), label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                numericInput(inputId = ns("legend_size"), label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
    ),
    mod_download_ui(ns("download"), output_type = "Plot")
  )
}

mod_exposure_map_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #Handler for the Exposure Map tab
      observeEvent(input$submit, {
        if(is.null(input$country)){
          output$info <- renderText("Please choose at least one country/region")
          return()
        }
        input_list <- reactiveValuesToList(input)
        toggle_inputs(input_list,F,T)
        
        if(input$period != "yearly"){
          std_agg_output <- get_standard_aggregation(iso = input$country, years = input$year, period = input$period, adm1 = input$adm, weights = rv$weights(),
                                                     threshold = input$threshold, selected_period = input$selected_period)
        }else{
          std_agg_output <- get_standard_aggregation(iso = input$country, years = input$year, period = input$period, adm1 = input$adm, weights = rv$weights(),
                                                     threshold = input$threshold)
        }
        
        country <- input$country
        adm <- input$adm
        data_output <- reactive({adm_plot(x = std_agg_output, isos = country, adm1 = adm, id_col = "capa_id_adm1", input$legend_size, input$font_size)})
        
        output$long_map <- renderPlot(data_output())
        rv$payload <- renderUI({plotOutput(ns("long_map"), height = "90vh")})
        
        mod_download_server("download", data_output = data_output(), output_type = "Plot")
        
        toggle_inputs(input_list,T,T)
      })
    }
  )
}