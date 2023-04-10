



toggle_inputs <- function(input_list,enable_inputs=T,only_buttons=TRUE){
  # Subset if only_buttons is TRUE.
  if(only_buttons){
    buttons <- which(sapply(input_list,function(x) {any(grepl('Button',attr(x,"class")))}))
    input_list = input_list[buttons]
  }
  
  # Toggle elements
  for(x in names(input_list))
    if(enable_inputs){
      shinyjs::enable(x)} else {
        shinyjs::disable(x) }
}

weight_presets <- function(session, weight_list, preset){
  weight_list <- weight_list[order(names(weight_list))]
  for(i in seq(weight_list)){
    updateNumericInput(session, names(weight_list)[i], value = weight_presets_list[[preset]][i])
  }
}