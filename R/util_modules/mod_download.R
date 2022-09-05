



mod_download_ui <- function(id, output_type){
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), label = glue("Download {output_type}"))
  )
}

mod_download_server <- function(id, data_output, output_type, file_name = "hdr_data"){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      if(output_type == "Plot"){
        output$download <- downloadHandler(
          filename = function(){glue("{file_name}.pdf")},
          content = function(fname){
            ggsave(fname, plot = data_output, device = "pdf")
          }
        )
      }
      
      if(output_type == "Table"){
        output$download <- downloadHandler(
          filename = function(){glue("{file_name}.xlsx")},
          content = function(fname){
            write_xlsx(data_output, fname)
          }
        )
      }
      
    }
  )
}