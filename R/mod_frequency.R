



mod_frequency_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    selectInput(ns("country"), "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
    sliderInput(ns("year_slider"), "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("start"), label = "Start Period", value = 1, min = 1, max = 12, step = 1),
                numericInput(inputId = ns("stop"), label = "End Period", value = 12, min = 1, max = 12, step = 1)
    ),
    radioButtons(inputId = ns("period"), label = "Period size", choices = c("yearly", "biannually", "quarterly", "monthly"), inline = T),
    radioButtons(inputId = ns("adm"), label = "Admin Level", choiceNames = c("ADM0", "ADM1"), choiceValues = c(FALSE, TRUE), inline = T),
    numericInput(inputId = ns("threshold"), label = "Intensity Threshold", value = 1, min = 1, max = 1000000, step = 1),
    radioButtons(inputId = ns("p_thresh_logic"), label = "Use a period threshold?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), inline = T),
    textOutput(ns("p_thresh_max")),
    numericInput(inputId = ns("period_threshold"), label = "Period Threshold", value = 1, min = 1, step = 1),
    actionButton(inputId = ns("submit"), "submit"),
    radioButtons(inputId = ns("output_type"), label = "Output type", choices = c("Table", "Map"), inline = T),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("font_size"), label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                numericInput(inputId = ns("legend_size"), label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
    ),
    mod_download_ui(ns("download"), output_type = "Plot")
  )
}

mod_frequency_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #Handler for the Frequency tab
      observeEvent(input$submit, {
        if(is.null(input$country)){
          output$info <- renderText("Please choose at least one country/region")
          return()
        }
        
        input_list <- reactiveValuesToList(input)
        toggle_inputs(input_list,F,T)
        
        if(as.logical(input$p_thresh_logic)){
          p_thresh <- input$period_threshold
        }else{
          p_thresh <- NA
        }
        
        data_output <- get_temporal(iso = input$country, years = c(input$year_slider[1]:input$year_slider[2]), start_end = c(input$start, input$stop),
                                         period = input$period, adm1 = input$adm, weights = rv$weights(), threshold = input$threshold, p_threshold = p_thresh, max_periods = p_thresh_max())
        
        if(input$output_type == "Table"){
          rv$payload <- renderUI({renderDataTable(data_output)})
          
          mod_download_server("download", data_output = data_output, output_type = "Table")
          
        }else{
          country <- input$country
          adm <- input$adm
          plot_output <- reactive({plot_adm(df = data_output, iso = country, adm1 = adm, id_col = "capa_id", input$legend_size, input$font_size)})
          output$frequency_map <- renderPlot(plot_output())
          rv$payload <- renderUI({plotOutput(ns("frequency_map"), height = "90vh")})
          
          mod_download_server("download", data_output = plot_output(), output_type = "Plot")
          
        }
        
        toggle_inputs(input_list,T,T)
      })
      
      #handler for freq_plot output type
      observe({
        if(as.logical(input$p_thresh_logic)){
          shinyjs::enable("output_type_freq")
        }else{
          updateRadioButtons(session, "output_type_freq", selected = "Table")
          shinyjs::disable("output_type_freq")
        }
      })
      
      #stupid proofing frequency start_stop
      observe({
        shinyjs::enable("start")
        shinyjs::enable("stop")
        if(input$period == "monthly"){
          updateNumericInput(session, "stop", max = 12)
          if(input$stop > 12){
            updateNumericInput(session, "stop", value = 12)
          }
        }else if(input$period == "quarterly"){
          updateNumericInput(session, "stop", max = 4)
          if(input$stop > 4){
            updateNumericInput(session, "stop", value = 4)
          }
        }else if(input$period == "biannually"){
          updateNumericInput(session, "stop", max = 2)
          if(input$stop > 2){
            updateNumericInput(session, "stop", value = 2)
          }
        }else{
          shinyjs::disable("start")
          shinyjs::disable("stop")
          updateNumericInput(session, "start", value = NULL)
          updateNumericInput(session, "stop", value = NULL)
        }
      })
      
      #Tool-tip for the maximum periods based on selection under Frequency tab
      p_thresh_max <- reactive({
        if(input$period == "monthly"){
          (((max(input$year_slider) - min(input$year_slider)) + 1) * 12) - (input$start - 1) - (12 - input$stop)
        }else if(input$period == "quarterly"){
          (((max(input$year_slider) - min(input$year_slider)) + 1) * 4) - (input$start - 1) - (4 - input$stop)
        }else if(input$period == "biannually"){
          (((max(input$year_slider) - min(input$year_slider)) + 1) * 2) - (input$start - 1) - (2 - input$stop)
        }else{
          (max(input$year_slider) - min(input$year_slider)) + 1
        }
      })
      output$p_thresh_max <- renderText(glue("Maximum periods in selection = {p_thresh_max()}"))
      
      #prohibit a period_threshold over the p_thresh_max
      observe({
        if(!is.numeric(input$period_threshold)){
          updateNumericInput(session, "period_threshold", value = 0)
        }else if(input$period_threshold > p_thresh_max()){
          updateNumericInput(session, "period_threshold", value = p_thresh_max())
        }
      })
    }
  )
}