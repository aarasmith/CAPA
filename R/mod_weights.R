


mod_weights_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("preset"), "Select preset weights", choices = names(weight_presets_list)),
    actionButton(ns("apply_preset"), "Apply preset weights"),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("L25_weight"), label = "Low 25km Weight", value = 1, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("L50_weight"), label = "Low 50km Weight", value = 1, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("L100_weight"), label = "Low 100km Weight", value = 1, min = 0, max = 100, step = 1)
    ),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("M25_weight"), label = "Medium 25km Weight", value = 1, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("M50_weight"), label = "Medium 50km Weight", value = 1, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("M100_weight"), label = "Medium 100km Weight", value = 1, min = 0, max = 100, step = 1)
    ),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("H25_weight"), label = "High 25km Weight", value = 1, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("H50_weight"), label = "High 50km Weight", value = 1, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("H100_weight"), label = "High 100km Weight", value = 1, min = 0, max = 100, step = 1)
    ),
    splitLayout(cellArgs = list(style='white-space: normal;'),
                numericInput(inputId = ns("int25_weight"), label = "B-deaths 25km Weight", value = 0, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("int50_weight"), label = "B-deaths 50km Weight", value = 0, min = 0, max = 100, step = 1),
                numericInput(inputId = ns("int100_weight"), label = "B-deaths 100km Weight", value = 0, min = 0, max = 100, step = 1)
    )
  )
}

mod_weights_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #Handler for weight presets
      observeEvent(input$apply_preset, {
        input_list <- reactiveValuesToList(input)
        weight_list <- input_list[grepl('weight',names(input_list))]
        weight_presets(session, weight_list, input$preset)
        current_weight_system <- input$preset
        rv$weight_system <- renderText(paste("Current Weight System:", current_weight_system))
      })
      
      #Reactive list of weights based on input from the Weights tab
      rv$weights <- reactive(
        sanitize_weights(
          list(
            L25 = input$L25_weight, L50 = input$L50_weight, L100 = input$L100_weight, M25 = input$M25_weight, M50 = input$M50_weight, M100 = input$M100_weight,
            H25 = input$H25_weight, H50 = input$H50_weight, H100 = input$H100_weight, int25 = input$int25_weight, int50 = input$int50_weight, int100 = input$int100_weight
          )
        )
      )
    }
  )
}








