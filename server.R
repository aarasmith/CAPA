#library(shiny)

server <- function(input, output, session) {
  
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
  weights <- reactive(list(L25 = input$L25_weight, L50 = input$L50_weight, L100 = input$L100_weight, M25 = input$M25_weight, M50 = input$M50_weight, M100 = input$M100_weight,
                           H25 = input$H25_weight, H50 = input$H50_weight, H100 = input$H100_weight, int25 = input$int25_weight, int50 = input$int50_weight, int100 = input$int100_weight))
   
  observeEvent(input$submit, {
    input_list <- reactiveValuesToList(input)
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
  
  observeEvent(input$submit_long_map, {
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
  
  observeEvent(input$submit_map, {
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    out_plot <- plot_score_sql(iso = input$country_map, years = c(input$year_slider_map[1]:input$year_slider_map[2]), start_end = c(input$start_map, input$stop_map),
                               L25_weight = input$L25_weight_map, L50_weight = input$L50_weight_map, L100_weight = input$L100_weight_map, M25_weight = input$M25_weight_map,
                               M50_weight = input$M50_weight_map, M100_weight = input$M100_weight_map, H25_weight = input$H25_weight_map, H50_weight = input$H50_weight_map,
                               H100_weight = input$H100_weight_map, int25_weight = input$int25_weight_map, int50_weight = input$int50_weight_map, int100_weight = input$int100_weight_map,
                               draw_points = input$draw_points_map)
    
    output$map <- renderPlot(plot_score(out_plot, legend_size = input$legend_size_map, font_size = input$font_size_map), height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
    toggle_inputs(input_list,T,T)
  })
  
  observeEvent(input$submit_dur, {
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    long_dur_output <- long_duration(iso = input$country_dur, years = c(input$year_slider_dur[1]:input$year_slider_dur[2]), start_end = c(input$start_dur, input$stop_dur),
                                     period = input$period_dur, adm1 = input$adm_dur, L25_weight = input$L25_weight_dur, L50_weight = input$L50_weight_dur, L100_weight = input$L100_weight_dur,
                                     M25_weight = input$M25_weight_dur, M50_weight = input$M50_weight_dur, M100_weight = input$M100_weight_dur, H25_weight = input$H25_weight_dur,
                                     H50_weight = input$H50_weight_dur, H100_weight = input$H100_weight_dur, int25_weight = input$int25_weight_dur, int50_weight = input$int50_weight_dur,
                                     int100_weight = input$int100_weight_dur, cap = NA, threshold = input$threshold_dur)
    
    if(input$adm_dur){
      long_dur_output <- long_dur_output %>% dplyr::select(-geometry)
      output$duration_map <- renderPlot(long_plot(long_dur_output, input$legend_size_dur, input$font_size_dur), height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
      #long_dur_output <- long_dur_output %>% dplyr::select(-shape_id, -shape_group)
    }
    
    output$duration <- renderDataTable(long_dur_output %>% dplyr::select(-cell_pop))
    
    
    output$download_dur <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(long_dur_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  observeEvent(input$submit_freq, {
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    long_freq_output <- long_freq(iso = input$country_freq, years = c(input$year_slider_freq[1]:input$year_slider_freq[2]), start_end = c(input$start_freq, input$stop_freq),
                                  period = input$period_freq, adm1 = input$adm_freq, L25_weight = input$L25_weight_freq, L50_weight = input$L50_weight_freq, L100_weight = input$L100_weight_freq,
                                  M25_weight = input$M25_weight_freq, M50_weight = input$M50_weight_freq, M100_weight = input$M100_weight_freq, H25_weight = input$H25_weight_freq,
                                  H50_weight = input$H50_weight_freq, H100_weight = input$H100_weight_freq, int25_weight = input$int25_weight_freq, int50_weight = input$int50_weight_freq,
                                  int100_weight = input$int100_weight_freq, cap = NA, threshold = input$threshold_freq)
    
    
    output$frequency <- renderDataTable(long_freq_output %>% dplyr::select(-cell_pop))
    
    
    output$download_freq <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(long_freq_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  observeEvent(input$submit_global, {
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    worst_global_output <- worst_global(region = input$region_global, years = c(input$year_slider_global[1]:input$year_slider_global[2]), monthly = input$monthly_global,
                                     L25_weight = input$L25_weight_global, L50_weight = input$L50_weight_global, L100_weight = input$L100_weight_global, M25_weight = input$M25_weight_global,
                                     M50_weight = input$M50_weight_global, M100_weight = input$M100_weight_global, H25_weight = input$H25_weight_global, H50_weight = input$H50_weight_global,
                                     H100_weight = input$H100_weight_global, int25_weight = input$int25_weight_global, int50_weight = input$int50_weight_global,
                                     int100_weight = input$int100_weight_global, threshold = input$threshold_global)
    
    
    output$global_table <- renderDataTable(worst_global_output)
    
    
    output$download_global <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(worst_global_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  
}
