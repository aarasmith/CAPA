



mod_regional_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    selectInput(ns("region"), "Select Region", choices = c("World", region_choices, "Custom Region")),
    sliderInput(ns("year_slider"), "Year Range:", min = 1990, max = max_year, value = c(1990, max_year), sep = ""),
    radioButtons(inputId = ns("period"), label = "Period size", choices = c("yearly", "biannually", "quarterly", "monthly"), inline = T),
    numericInput(inputId = ns("threshold"), label = "Intensity Threshold", min = 1, value = 1, step = 1),
    actionButton(inputId = ns("submit"), label = "Submit"),
    mod_download_ui(ns("download"), output_type = "Table")
  )
}

mod_regional_custom_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("info_custom")),
    selectInput(ns("region_custom"), "Select Region", choices = c("World", region_choices)),
    selectInput(ns("region_add"), "Countries to add", choices = all_country_choices, multiple = T),
    selectInput(ns("region_subtract"), "Countries to remove", choices = all_country_choices, multiple = T),
    actionButton(inputId = ns("submit_custom"), label = "Display custom region")
  )
}

mod_regional_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #Handler for regional aggregations
      observeEvent(input$submit, {
        if(input$region == ""){ #is.null(input$region) when multiple is added
          output$info <- renderText("Please choose a region")
          return()
        }
        input_list <- reactiveValuesToList(input)
        toggle_inputs(input_list,F,T)
        
        
        if(input$region == "Custom Region"){
          data_output <- get_region_aggregation(region = ison(custom_region()), years = c(input$year_slider[1]:input$year_slider[2]), period = input$period,
                                                             weights = rv$weights(), threshold = input$threshold)
        }else if(any(grepl("GWNO", input$region))){
          data_output <- get_region_aggregation(region = manual_regions[[input$region]], years = c(input$year_slider[1]:input$year_slider[2]), period = input$period,
                                                             weights = rv$weights(), threshold = input$threshold)
        }else{
          data_output <- get_region_aggregation(region = input$region, years = c(input$year_slider[1]:input$year_slider[2]), period = input$period,
                                                      weights = rv$weights(), threshold = input$threshold) 
        }
        
        rv$payload <- renderUI({renderDataTable(data_output)})
        
        mod_download_server("download", data_output = data_output, output_type = "Table")
        
        toggle_inputs(input_list,T,T)
      })
      
      #Handler for custom region
      custom_region <- reactive({
        if(any(grepl("GWNO", input$region_custom))){
          c(isoc(manual_regions[[input$region_custom]]), input$region_add)[c(isoc(manual_regions[[input$region_custom]]), input$region_add) %!in% input$region_subtract]
        }else{
          c(isoc(ison(input$region_custom)), input$region_add)[c(isoc(ison(input$region_custom)), input$region_add) %!in% input$region_subtract]
        }
      })
      
      #Handler for displaying custom region
      observeEvent(input$submit_custom, {
        output$info_custom <- renderText(sort(custom_region()))
      })
    }
  )
}