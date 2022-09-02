#library(shiny)

server <- function(input, output, session) {
  
  #### Initial state ####
  output$body_plot <- renderUI({HTML(markdown::markdownToHTML('markdown/home_page.md', fragment.only = T))})
  output$weight_system <- renderText(paste("Current Weight System:", "events_100km_unweighted"))
  #output$body_plot <- renderUI({testuiServer("test1")})
  
  
  #### Info tab handlers ####
  
  #handler for un geoscheme
  # observeEvent(input$un_geoscheme, {
  #   output$un_geoscheme <- renderDataTable(un_geoscheme)
  #   output$body_plot <- renderUI({dataTableOutput("un_geoscheme")})
  # })
  
  #Handler for markdown homepage
  # observeEvent(input$guide, {
  #   output$body_plot <- renderUI({HTML(markdown::markdownToHTML('markdown/home_page.md', fragment.only = T))})
  # })
  rv <- reactiveValues(submit = NULL, payload = NULL)
  # v <- reactive({
  #   mod_guide_server("guide")
  # })
  # observeEvent(input$guide, {
  #   output$body_plot <- renderUI({mod_guide_server("guide")})
  # })
  # mod_guide_server("guide", rv = rv, md_file = 'markdown/home_page.md')
  # mod_guide_server("citations", rv = rv, md_file = 'markdown/data_citations.md')
  mod_info_server("info", rv = rv)
  observeEvent(rv$payload, {
    output$body_plot <- rv$payload
  })

  # observeEvent(input$citations, {
  #   output$body_plot <- renderUI({HTML(markdown::markdownToHTML('markdown/data_citations.md', fragment.only = T))})
  # })
  # observeEvent(input$codebook, {
  #   insertUI(selector = "#codebook",
  #            where = "afterEnd",
  #            ui = tags$audio(src = "gnome_sound_effect.mp3", type = "audio/mpeg", autoplay = T, controls = NA, style="display:none;")
  #   )
  #   output$body_plot <- renderUI({
  #     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/j3hOd7u35no" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>')
  #   })
  # })
  
  
  # observeEvent(input$sidebarItemExpanded, {
  #   print(input$sidebarItemExpanded)
  #   if(input$sidebarItemExpanded == "Homepage"){
  #     output$body_plot <- renderUI({HTML(markdown::markdownToHTML('home_page.md', fragment.only = T))})
  #   }
  # })
  
  #### Weights tab handlers ####
  
  #Handler for weight presets
  observeEvent(input$apply_preset, {
    input_list <- reactiveValuesToList(input)
    weight_list <- input_list[grepl('weight',names(input_list))]
    weight_presets(session, weight_list, input$preset)
    curent_weight_system <- input$preset
    output$weight_system <- renderText(paste("Current Weight System:", curent_weight_system))
  })
  
  #Reactive list of weights based on input from the Weights tab
  weights <- reactive(sanitize_weights(
    list(L25 = input$L25_weight, L50 = input$L50_weight, L100 = input$L100_weight, M25 = input$M25_weight, M50 = input$M50_weight, M100 = input$M100_weight,
         H25 = input$H25_weight, H50 = input$H50_weight, H100 = input$H100_weight, int25 = input$int25_weight, int50 = input$int50_weight, int100 = input$int100_weight)
  )
  )
  
  
  #### Exposure tab handlers ####
  
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
    
    
    
    std_agg_output <- get_standard_aggregation(iso = input$country, years = c(input$year_slider[1]:input$year_slider[2]), period = input$period, adm1 = input$adm, weights = weights(),
                                               threshold = input$threshold)
    
    
    # if(input$adm){
    #   worst_adm_output <- worst_adm_output %>% dplyr::select(-geometry, -shape_id)
    # }
    
    output$Table <- renderDataTable(std_agg_output)
    output$body_plot <- renderUI({dataTableOutput("Table")})
    
    
    output$download <- downloadHandler(
      filename = function(){"hdr_data.xlsx"},
      content = function(fname){
        write_xlsx(std_agg_output, fname)
      }
    )
    toggle_inputs(input_list,T,T)
  })
  
  
  #### Exposure Map tab handlers ####
  
  #Handler for the Exposure Map tab
  observeEvent(input$submit_long_map, {
    if(is.null(input$country_long_map)){
      output$info_long_map <- renderText("Please choose at least one country/region")
      return()
    }
    input_list <- reactiveValuesToList(input)
    toggle_inputs(input_list,F,T)
    
    if(input$period_long_map != "yearly"){
      std_agg_output <- get_standard_aggregation(iso = input$country_long_map, years = input$year_long_map, period = input$period_long_map, adm1 = input$adm_long_map, weights = weights(),
                                                 threshold = input$threshold_long_map, selected_period = input$selected_period_long_map)
    }else{
      std_agg_output <- get_standard_aggregation(iso = input$country_long_map, years = input$year_long_map, period = input$period_long_map, adm1 = input$adm_long_map, weights = weights(),
                                                 threshold = input$threshold_long_map)
    }
    
    exposure_map <- adm_plot(x = std_agg_output, isos = input$country_long_map, adm1 = input$adm_long_map, id_col = "capa_id_adm1", input$legend_size_long_map, input$font_size_long_map)
    
    output$long_map <- renderPlot(exposure_map)
    #plot_height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0))
    #print(plot_height())
    output$body_plot <- renderUI({plotOutput("long_map", height = "90vh")})
    
    output$download_exposure_map <- downloadHandler(
      filename = function(){"hdr_exposure_map.pdf"},
      content = function(fname){
        ggsave(fname, plot = exposure_map, device = "pdf")
      }
    )
    
    toggle_inputs(input_list,T,T)
  })
  
  
  #### Score Map tab handlers ####
  
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
    
    score_map <- plot_cell_scores(x = out_plot, isos = input$country_map, legend_size = input$legend_size_map, font_size = input$font_size_map)
    
    output$map <- renderPlot(score_map)
    output$body_plot <- renderUI({plotOutput("map", height = "90vh")})
    
    output$download_score_map <- downloadHandler(
      filename = function(){"hdr_score_map.pdf"},
      content = function(fname){
        ggsave(fname, plot = score_map, device = "pdf")
      }
    )
    
    toggle_inputs(input_list,T,T)
  })
  
  
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
                                     period = input$period_freq, adm1 = input$adm_freq, weights = weights(), threshold = input$threshold_freq, p_threshold = p_thresh, max_periods = p_thresh_max())
    
    # if(as.logical(input$adm_freq) & as.logical(input$p_thresh_logic)){
    #   frequency_map <- adm_plot(x = frequency_output, iso = input$country_freq, id_col = "capa_id", input$legend_size_freq, input$font_size_freq)
    #   output$frequency_map <- renderPlot(frequency_map, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/6,0)))
    # }
    
    if(input$output_type_freq == "Table"){
      output$frequency <- renderDataTable(frequency_output)
      output$body_plot <- renderUI({dataTableOutput("frequency")})
      
      output$download_freq <- downloadHandler(
        filename = function(){"hdr_data.xlsx"},
        content = function(fname){
          write_xlsx(frequency_output, fname)
        }
      )
      
    }else{
      frequency_map <- adm_plot(x = frequency_output, iso = input$country_freq, adm1 = input$adm_freq, id_col = "capa_id", input$legend_size_freq, input$font_size_freq)
      output$frequency_map <- renderPlot(frequency_map)
      output$body_plot <- renderUI({plotOutput("frequency_map", height = "90vh")})
      
      output$download_freq <- downloadHandler(
        filename = function(){"hdr_frequency_map.pdf"},
        content = function(fname){
          ggsave(fname, plot = frequency_map, device = "pdf")
        }
      )
      
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
    shinyjs::enable("start_freq")
    shinyjs::enable("stop_freq")
    if(input$period_freq == "monthly"){
      updateNumericInput(session, "stop_freq", max = 12)
      if(input$stop_freq > 12){
        updateNumericInput(session, "stop_freq", value = 12)
      }
    }else if(input$period_freq == "quarterly"){
      updateNumericInput(session, "stop_freq", max = 4)
      if(input$stop_freq > 4){
        updateNumericInput(session, "stop_freq", value = 4)
      }
    }else if(input$period_freq == "biannually"){
      updateNumericInput(session, "stop_freq", max = 2)
      if(input$stop_freq > 2){
        updateNumericInput(session, "stop_freq", value = 2)
      }
    }else{
      shinyjs::disable("start_freq")
      shinyjs::disable("stop_freq")
      updateNumericInput(session, "start_freq", value = NULL)
      updateNumericInput(session, "stop_freq", value = NULL)
    }
  })
  
  #Tool-tip for the maximum periods based on selection under Frequency tab
  p_thresh_max <- reactive({
    if(input$period_freq == "monthly"){
      (((max(input$year_slider_freq) - min(input$year_slider_freq)) + 1) * 12) - (input$start_freq - 1) - (12 - input$stop_freq)
    }else if(input$period_freq == "quarterly"){
      (((max(input$year_slider_freq) - min(input$year_slider_freq)) + 1) * 4) - (input$start_freq - 1) - (4 - input$stop_freq)
    }else if(input$period_freq == "biannually"){
      (((max(input$year_slider_freq) - min(input$year_slider_freq)) + 1) * 2) - (input$start_freq - 1) - (2 - input$stop_freq)
    }else{
      (max(input$year_slider_freq) - min(input$year_slider_freq)) + 1
    }
  })
  output$p_thresh_max <- renderText(glue("Maximum periods in selection = {p_thresh_max()}"))
  
  #prohibit a period_threshold over the p_thresh_max
  observe({
    if(input$period_threshold > p_thresh_max()){
      updateNumericInput(session, "period_threshold", value = p_thresh_max())
    }
  })
  
  
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
