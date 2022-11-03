#library(shiny)

server <- function(input, output, session) {
  track_usage(storage_mode = store_json(path = "/root/logs"))
  #### Initial state ####
  output$body_plot <- renderUI({HTML(markdown::markdownToHTML('markdown/home_page.md', fragment.only = T))})
  output$weight_system <- renderText(paste("Current Weight System:", "events_100km_unweighted"))
  rv <- reactiveValues(submit = NULL, payload = NULL, weight_system = NULL, weights = NULL)
  
  
  #### Info tab handlers ####

  mod_info_server("info", rv = rv)
  
  observeEvent(rv$payload, {
    output$body_plot <- rv$payload
  })
  observeEvent(rv$weight_system, {
    output$weight_system <- rv$weight_system
  })

  
  #### Weights tab handlers ####

  mod_weights_server("weights", rv = rv)
  
  #### Exposure tab handlers ####
  
  #Handler for the Exposure tab 
  mod_exposure_server("exposure", rv = rv)
  
  
  #### Exposure Map tab handlers ####
  
  mod_exposure_map_server("exposure_map", rv = rv)
  
  
  #### Score Map tab handlers ####
  
  #Handler for the Score Map tab
  mod_score_map_server("score_map", rv = rv)
  
  
  #### Duration tab handlers ####
  
  #Handler for the Duration tab
  observeEvent(input$submit_dur, {
    #browser()
    if(is.null(input$country_dur)){
      output$info_dur <- renderText("Please choose at least one country/region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    duration_output <- get_temporal(type = "duration", iso = input$country_dur, years = c(input$year_slider_dur[1]:input$year_slider_dur[2]), start_end = c(input$start_dur, input$stop_dur),
                                    adm1 = input$adm_dur, weights = weights(), period = input$period_dur, threshold = input$threshold_dur)
    
    # if(input$adm_dur){
    #   #duration_output <- duration_output %>% dplyr::select(-geometry)
    #   duration_map <- adm_plot(x = duration_output, iso = input$country_dur, id_col = "capa_id", input$legend_size_dur, input$font_size_dur)
    #   output$duration_map <- renderPlot(duration_map, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
    #   #duration_output <- duration_output %>% dplyr::select(-shape_id, -shape_group)
    # }
    
    if(input$output_type_dur == "Table"){
      output$duration <- renderDataTable(duration_output)
      output$body_plot <- renderUI({dataTableOutput("duration")})
      
      output$download_dur <- downloadHandler(
        filename = function(){"hdr_data.xlsx"},
        content = function(fname){
          write_xlsx(duration_output, fname)
        }
      )
      
    }else{
      duration_map <- adm_plot(x = duration_output, iso = input$country_dur, adm1 = input$adm_dur, id_col = "capa_id", input$legend_size_dur, input$font_size_dur)
      output$duration_map <- renderPlot(duration_map)
      output$body_plot <- renderUI({plotOutput("duration_map", height = "90vh")})
      
      output$download_dur <- downloadHandler(
        filename = function(){"hdr_duration_map.pdf"},
        content = function(fname){
          ggsave(fname, plot = duration_map, device = "pdf")
        }
      )
      
    }
    
    toggle_inputs(input_list,T,T)
  })
  
  #handler for dur_plot output type
  # observe({
  #   if(input$adm_dur == FALSE){
  #     updateRadioButtons(session, "output_type_dur", selected = "Table")
  #     shinyjs::disable("output_type_dur")
  #   }else{
  #     shinyjs::enable("output_type_dur")
  #   }
  # })
  
  
  #### Frequency tab handlers ####
  
  mod_frequency_server("frequency", rv = rv)
  
  
  #### Regional tab handlers ####
  
  mod_regional_server("regional", rv = rv)
  
  
  #### Children at Risk handlers ####
  
  mod_car_server("CAR", rv = rv)
  
}
