


mod_exposure_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    selectInput(ns("country"), "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
    sliderInput(ns("year_slider"), "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
    radioButtons(inputId = ns("period"), label = "Period size", choices = c("yearly", "biannually", "quarterly", "monthly"), inline = T),
    radioButtons(inputId = ns("adm"), label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
    numericInput(inputId = ns("threshold"), label = "Intensity Threshold", value = 1, min = 1, step = 1),
    actionButton(inputId = ns("submit"), label = "Submit"),
    downloadButton(ns("download"), label = "Download Table")
  )
}

mod_exposure_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      observeEvent(input$submit, {
        if(is.null(input$country)){
          output$info <- renderText("Please choose at least one country/region")
          return()
        }
        input_list <- reactiveValuesToList(input)
        toggle_inputs(input_list,F,T)
        
        std_agg_output <- get_standard_aggregation(iso = input$country, years = c(input$year_slider[1]:input$year_slider[2]), period = input$period, adm1 = input$adm, weights = rv$weights(),
                                                   threshold = input$threshold)

        rv$payload <- renderUI({
          renderDataTable(std_agg_output)
        })
        
        
        output$download <- downloadHandler(
          filename = function(){"hdr_data.xlsx"},
          content = function(fname){
            write_xlsx(std_agg_output, fname)
          }
        )
        toggle_inputs(input_list,T,T)
      })
    }
  )
}