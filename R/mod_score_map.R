



mod_score_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    selectInput(ns("country"), "Select Country", choices = c(country_choices, "World", region_choices), multiple = T),
    sliderInput(ns("year_slider"), "Year Range:", min = 1990, max = 2021, value = c(1990, 2021), sep = ""),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("start"), label = "Start Month", value = 1, min = 1, max = 12, step = 1),
                numericInput(inputId = ns("stop"), label = "End Month", value = 12, min = 1, max = 12, step = 1)
    ),
    checkboxInput(inputId = ns("draw_adm1"), label = "show ADM1 boundaries"),
    checkboxInput(inputId = ns("draw_points"), label = "show conflict events"),
    actionButton(inputId = ns("submit"), "generate map"),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("font_size"), label = "Legend font size", value = 18, min = 1, max = 100, step = 1),
                numericInput(inputId = ns("legend_size"), label = "Legend key size (in CM)", value = 2, min = 0.1, max = 100, step = 0.1)
    ),
    mod_download_ui(ns("download"), output_type = "Plot")
  )
}

mod_score_map_server <- function(id, rv){
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
        
        out_plot <- get_cell_scores(iso = input$country, years = c(input$year_slider[1]:input$year_slider[2]), start_end = c(input$start, input$stop),
                                    weights = rv$weights(), draw_adm1 = input$draw_adm1, draw_points = input$draw_points)
        
        country <- input$country
        data_output <- reactive({plot_cell_scores(data_list = out_plot, isos = country, legend_size = input$legend_size, font_size = input$font_size)})
        
        output$map <- renderPlot(data_output())
        rv$payload <- renderUI({plotOutput(ns("map"), height = "90vh")})
        
        mod_download_server("download", data_output = data_output(), output_type = "Plot")
        
        toggle_inputs(input_list,T,T)
      })
    }
  )
}