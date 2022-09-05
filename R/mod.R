# mod_button <- function(id, label){
#   ns <- NS(id)
#   tagList(actionButton(ns("submit"), label = label))
# }
# 
# 
# mod_guide_server <- function(id, rv, md_file){
#   moduleServer(
#     id,
#     function(input, output, session){
#       print("I RAN YAY")
#       ns <- session$ns
#       observeEvent(input$submit, {
#         rv$submit <- input$submit
#         rv$payload <- renderUI({HTML(markdown::markdownToHTML(md_file, fragment.only = T))})
#       })
#       #return(HTML(markdown::markdownToHTML('markdown/home_page.md', fragment.only = T)))
#     })
# }

# cols <- c("col_a", "col_b")
# glue_sql("SELECT {glue_sql_collapse(cols, sep = ', ')}")
# glue_sql("{cols*}", .con = connect_to_capa())
# 
# dbGetQuery(connect_to_capa(), "SELECT \"half\" FROM month_key")


# insertUI(selector = "#codebook",
#          where = "afterEnd",
#          ui = tags$audio(src = "gnome_sound_effect.mp3", type = "audio/mpeg", autoplay = T, controls = NA, style="display:none;")
# )

# mod_info_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#     actionButton(ns("guide"), label = "Guide"),
#     actionButton(ns("codebook"), label = "Codebook"),
#     actionButton(ns("citations"), label = "Citations"),
#     actionButton(ns("un_geoscheme"), label = "UN Geoscheme Key")
#   )
# }
# 
# mod_info_server <- function(id, rv){
#   moduleServer(
#     id,
#     function(input, output, session){
#       ns <- session$ns
#       #handler for un geoscheme
#       observeEvent(input$un_geoscheme, {
#         rv$payload <- renderUI({renderDataTable(un_geoscheme)})
#       })
#       
#       #Handler for markdown homepage
#       observeEvent(input$guide, {
#         rv$payload <- renderUI({HTML(markdown::markdownToHTML('markdown/home_page.md', fragment.only = T))})
#       })
#       
#       observeEvent(input$citations, {
#         rv$payload <- renderUI({HTML(markdown::markdownToHTML('markdown/data_citations.md', fragment.only = T))})
#       })
#       
#       observeEvent(input$codebook, {
#         insertUI(selector = paste0("#", ns("codebook")),
#                  where = "afterEnd",
#                  ui = tags$audio(src = "gnome_sound_effect.mp3", type = "audio/mpeg", autoplay = T, controls = NA, style="display:none;")
#         )
#         rv$payload <- renderUI({
#           HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/j3hOd7u35no" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>')
#         })
#       })
#     })
# }


mod_xxx_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

mod_xxx_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
    }
  )
}