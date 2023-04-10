mod_info_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("guide"), label = "Guide"),
    actionButton(ns("codebook"), label = "Codebook"),
    actionButton(ns("citations"), label = "Citations"),
    actionButton(ns("un_geoscheme"), label = "UN Geoscheme Key")
  )
}

mod_info_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #handler for un geoscheme
      observeEvent(input$un_geoscheme, {
        rv$payload <- renderUI({renderDataTable(un_geoscheme)})
      })
      
      #Handler for markdown homepage
      observeEvent(input$guide, {
        rv$payload <- renderUI({HTML(markdown::markdownToHTML('markdown/home_page.md', fragment.only = T))})
      })
      
      observeEvent(input$citations, {
        rv$payload <- renderUI({HTML(markdown::markdownToHTML('markdown/data_citations.md', fragment.only = T))})
      })
      
      observeEvent(input$codebook, {
        insertUI(selector = paste0("#", ns("codebook")),
                 where = "afterEnd",
                 ui = tags$audio(src = "gnome_sound_effect.mp3", type = "audio/mpeg", autoplay = T, controls = NA, style="display:none;")
        )
        rv$payload <- renderUI({
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/j3hOd7u35no" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>')
        })
      })
    })
}