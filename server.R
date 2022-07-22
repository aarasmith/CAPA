#library(shiny)

server <- function(input, output, session) {
  
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
  #determining the maximum periods based on selection under Frequency tab
  p_thresh_max <- reactive({
    if(input$monthly_freq){
      (((max(input$year_slider_freq) - min(input$year_slider_freq)) + 1) * 12) - (input$start_freq - 1) - (12 - input$stop_freq)
    }else{
      (max(input$year_slider_freq) - min(input$year_slider_freq)) + 1
    }
  })
  output$p_thresh_max <- renderText(glue("Maximum periods in selection = {p_thresh_max()}"))
  
  #Reactive list of weights based on input from the Weights tab
  weights <- reactive(sanitize_weights(
    list(L25 = input$L25_weight, L50 = input$L50_weight, L100 = input$L100_weight, M25 = input$M25_weight, M50 = input$M50_weight, M100 = input$M100_weight,
        H25 = input$H25_weight, H50 = input$H50_weight, H100 = input$H100_weight, int25 = input$int25_weight, int50 = input$int50_weight, int100 = input$int100_weight)
    )
  )
  
  observeEvent(input$apply_preset, {
    input_list <- reactiveValuesToList(input)
    weight_list <- input_list[grepl('weight',names(input_list))]
    weight_presets(session, weight_list, input$preset)
  })
  
  #Handler for the Exposure tab 
  observeEvent(input$submit, {
    if(is.null(input$country)){
      output$info <- renderText("Please choose at least one country/region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    #weight_list <- input_list[grepl('weight',names(input_list))]
    #print(weight_list)
    #browser()
    toggle_inputs(input_list,F,T)
    


    std_agg_output <- get_standard_aggregation(iso = input$country, years = c(input$year_slider[1]:input$year_slider[2]), monthly = input$monthly, adm1 = input$adm, weights = weights(),
                                  threshold = input$threshold)

    
    # if(input$adm){
    #   worst_adm_output <- worst_adm_output %>% dplyr::select(-geometry, -shape_id)
    # }
    
    output$Table <- renderDataTable(std_agg_output)
  
  
    output$download <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(std_agg_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  #Handler for the Exposure Map tab
  observeEvent(input$submit_long_map, {
    if(is.null(input$country_long_map)){
      output$info_long_map <- renderText("Please choose at least one country/region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    if(input$monthly_long_map){
      std_agg_output <- get_standard_aggregation(iso = input$country_long_map, years = input$year_long_map, monthly = input$monthly_long_map, adm1 = T, weights = weights(),
                                                 threshold = input$threshold_long_map, selected_month = input$month_long_map)
    }else{
      std_agg_output <- get_standard_aggregation(iso = input$country_long_map, years = input$year_long_map, monthly = input$monthly_long_map, adm1 = T, weights = weights(),
                                                 threshold = input$threshold_long_map)
    }
    
    
    output$long_map <- renderPlot(adm_plot(x = std_agg_output, isos = input$country_long_map, id_col = "capa_id_adm1", input$legend_size_long_map, input$font_size_long_map),
                                  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
    
    toggle_inputs(input_list,T,T)
  })
  
  #Handler for the Score Map tab
  observeEvent(input$submit_map, {
    if(is.null(input$country_map)){
      output$info_map <- renderText("Please choose at least one country/region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    out_plot <- get_cell_scores(iso = input$country_map, years = c(input$year_slider_map[1]:input$year_slider_map[2]), start_end = c(input$start_map, input$stop_map),
                               weights = weights(), draw_adm1 = input$draw_adm1_map, draw_points = input$draw_points_map)
    
    output$map <- renderPlot(plot_cell_scores(x = out_plot, isos = input$country_map, legend_size = input$legend_size_map, font_size = input$font_size_map),
                             height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
    toggle_inputs(input_list,T,T)
  })
  
  #Handler for the Duration tab
  observeEvent(input$submit_dur, {
    if(is.null(input$country_dur)){
      output$info_dur <- renderText("Please choose at least one country/region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    duration_output <- get_temporal(type = "duration", iso = input$country_dur, years = c(input$year_slider_dur[1]:input$year_slider_dur[2]), start_end = c(input$start_dur, input$stop_dur),
                                     adm1 = input$adm_dur, weights = weights(), monthly = as.logical(input$monthly_dur), threshold = input$threshold_dur)
    
    if(input$adm_dur){
      #duration_output <- duration_output %>% dplyr::select(-geometry)
      output$duration_map <- renderPlot(adm_plot(x = duration_output, iso = input$country_dur, id_col = "capa_id", input$legend_size_dur, input$font_size_dur), height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
      #duration_output <- duration_output %>% dplyr::select(-shape_id, -shape_group)
    }
    
    output$duration <- renderDataTable(duration_output)
    
    
    output$download_dur <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(duration_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  #Handler for the Frequency tab
  observeEvent(input$submit_freq, {
    if(is.null(input$country_freq)){
      output$info_freq <- renderText("Please choose at least one country/region")
      return()
    }
    #browser()
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    if(as.logical(input$p_thresh_logic)){
      p_thresh <- input$period_threshold
    }else{
      p_thresh <- NA
    }
    
    frequency_output <- get_temporal(type = "frequency", iso = input$country_freq, years = c(input$year_slider_freq[1]:input$year_slider_freq[2]), start_end = c(input$start_freq, input$stop_freq),
                                  monthly = as.logical(input$monthly_freq), adm1 = input$adm_freq, weights = weights(), threshold = input$threshold_freq, p_threshold = p_thresh)
    
    if(as.logical(input$adm_freq) & as.logical(input$p_thresh_logic)){
      output$frequency_map <- renderPlot(adm_plot(x = frequency_output, iso = input$country_freq, id_col = "capa_id", input$legend_size_freq, input$font_size_freq),
                                        height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
    }
    
    output$frequency <- renderDataTable(frequency_output)
    
    
    output$download_freq <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(frequency_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  observeEvent(input$submit_global, {
    if(is.null(input$region_global)){
      output$info_global <- renderText("Please choose at least one region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    region_agg_output <- get_region_aggregation(region = input$region_global, years = c(input$year_slider_global[1]:input$year_slider_global[2]), monthly = input$monthly_global,
                                     weights = weights(), threshold = input$threshold_global)
    
    
    output$global_table <- renderDataTable(region_agg_output)
    
    
    output$download_global <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(region_agg_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  
}
