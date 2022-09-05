#library(shiny)

server <- function(input, output, session) {
  
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
  
  #Handler for regional aggregations
  observeEvent(input$submit_global, {
    if(is.null(input$region_global)){
      output$info_global <- renderText("Please choose a region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    
    if(input$region_global == "Custom Region"){
      region_agg_output <- get_custom_region_aggregation(isos = ison(custom_region()), years = c(input$year_slider_global[1]:input$year_slider_global[2]), period = input$period_global,
                                                         weights = weights(), threshold = input$threshold_global)
    }else if(any(grepl("GWNO", input$region_global))){
      region_agg_output <- get_custom_region_aggregation(isos = manual_regions[[input$region_global]], years = c(input$year_slider_global[1]:input$year_slider_global[2]), period = input$period_global,
                                                         weights = weights(), threshold = input$threshold_global)
    }else{
      region_agg_output <- get_region_aggregation(region = input$region_global, years = c(input$year_slider_global[1]:input$year_slider_global[2]), period = input$period_global,
                                                  weights = weights(), threshold = input$threshold_global) 
    }
    
    
    output$global_table <- renderDataTable(region_agg_output)
    output$body_plot <- renderUI({dataTableOutput("global_table")})
    
    
    output$download_global <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(region_agg_output, fname)
      }
    )
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
  
  
  #### Children at Risk handlers ####
  
  #Handler for children at risk
  observeEvent(input$submit_car, {
    
    #Stupid proofing#
    if(length(input$categories_car) != length(input$scores_car)){
      output$info_car <- renderText("Must have equal amount of category labels and category lower-bounds")
      return()
    }
    
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    CAR_output1 <- children_in_conflict(iso3c = input$country_car, years = c(input$year_slider_car[1]:input$year_slider_car[2]), period = "yearly", adm1 = FALSE, weights = weights(),
                                        score_selection = as.numeric(input$scores_car), cat_names = input$categories_car, exclusive = input$exclusive_car, level = input$level_car)
    
    
    CAR_output <- reactive({
      if(input$level_car == "Country"){
        CAR_output <- CAR_output1 %>% dplyr::select(-matches("region\\d"), -contains("risk_pop"), -contains("risk_pct")) %>% arrange(country)
      }else{
        CAR_output <- agg_car(CAR_output1, level = input$level_car)
      }
      if(input$exclusive_car){
        CAR_output <- CAR_output %>%
          mutate(risk_children_total = rowSums(across(contains("risk_children") & !contains("share")))) %>%
          mutate(risk_children_total_share = round(risk_children_total/total_children, 4))
      }
      CAR_output
    })
    
    
    output$car_table <- renderDataTable(CAR_output(), options = list(scrollX = TRUE))
    output$body_plot <- renderUI({dataTableOutput("car_table")})
    
    output$download_car <- downloadHandler(
      filename = function(){"CAR_data.xlsx"},
      content = function(fname){
        write_xlsx(CAR_output(), fname)
      }
    )
    
    toggle_inputs(input_list,T,T)
  })
  
}
