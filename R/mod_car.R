



mod_car_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    selectInput(ns("country"), "Select Country/Region", choices = c(country_choices, "World", region_choices), multiple = T, selected = "World"),
    sliderInput(ns("year_slider"), "Year Range:", min = 1990, max = max_year, value = c(1990, max_year), sep = ""),
    selectizeInput(ns("categories"), "Select Category Labels", choices = c("low", "medium", "high", "extreme"), selected = c("low", "medium", "high", "extreme"),
                   multiple = TRUE, options = list(create = TRUE)),
    selectizeInput(ns("scores"), "Select Category Lower-Bounds", choices = c(1, 25, 100, 1000), selected = c(1, 25, 100, 1000), multiple = TRUE, options = list(create = TRUE)),
    radioButtons(inputId = ns("exclusive"), label = "Category Types", choiceNames = c("Inclusive", "Exclusive"), choiceValues = c(FALSE, TRUE), selected = TRUE, inline = T),
    radioButtons(inputId = ns("level"), label = "Aggregation Level", choices = c("Country", "Region", "Global"), selected = "Country", inline = T),
    actionButton(inputId = ns("submit"), label = "Submit"),
    mod_download_ui(ns("download"), output_type = "Table")
  )
}

mod_car_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #Handler for children at risk
      observeEvent(input$submit, {
        
        #Stupid proofing#
        if(length(input$categories) != length(input$scores)){
          output$info <- renderText("Must have equal amount of category labels and category lower-bounds")
          return()
        }
        
        input_list <- reactiveValuesToList(input)
        toggle_inputs(input_list,F,T)
        
        CAR_output1 <- get_CAR(iso3c = input$country, years = c(input$year_slider[1]:input$year_slider[2]), period = "yearly", adm1 = FALSE, weights = rv$weights(),
                                            score_selection = as.numeric(input$scores), cat_names = input$categories, exclusive = input$exclusive)
        
        
        CAR_output <- reactive({
          if(input$level == "Country"){
            CAR_output <- CAR_output1 %>% dplyr::select(-matches("region\\d"), -contains("risk_pop"), -contains("risk_pct")) %>% arrange(country)
          }else{
            CAR_output <- agg_car(CAR_output1, level = input$level)
          }
          
          if(input$exclusive){
            CAR_output <- CAR_output %>%
              mutate(risk_children_total = rowSums(across(contains("risk_children") & !contains("share")))) %>%
              mutate(risk_children_total_share = round(risk_children_total/total_children, 4))
          }
          
          mod_download_server("download", data_output = CAR_output(), output_type = "Table")
          
          CAR_output
        })
        
        
        output$car_table <- renderDataTable(CAR_output(), options = list(scrollX = TRUE))
        rv$payload <- renderUI({dataTableOutput(ns("car_table"))})
        
        
        
        toggle_inputs(input_list,T,T)
      })
    }
  )
}