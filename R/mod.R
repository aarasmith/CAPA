


mod_guide_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      # insertUI(selector = "#codebook",
      #          where = "afterEnd",
      #          ui = tags$audio(src = "gnome_sound_effect.mp3", type = "audio/mpeg", autoplay = T, controls = NA, style="display:none;")
      # )
        return(renderUI({HTML(markdown::markdownToHTML('home_page.md', fragment.only = T))}))
    })
  
  
}

# cols <- c("col_a", "col_b")
# glue_sql("SELECT {glue_sql_collapse(cols, sep = ', ')}")
# glue_sql("{cols*}", .con = connect_to_capa())
# 
# dbGetQuery(connect_to_capa(), "SELECT \"half\" FROM month_key")
