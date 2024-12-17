#' Headerbox factory
#'
#' @description A function to create boxes with headers
#'
#' @return UI elements for boxes
#'
#' @noRd
headerbox_factory <- function(title, status, content, width = 6, id = NULL){
  box(title = title,
      status = status,
      solidHeader = TRUE,
      collapsible = FALSE,
      closable = FALSE,
      width = width,
      id = id,
      content
  )
}

next_button <- function(id){
  actionBttn(
    inputId = id,
    label = 'Next',
    icon = icon('arrow-right'),
    style = 'jelly',
    color = 'success',
    block = TRUE,
    size = 'sm'
  )
}

colored_text <- function(text, color){
  shiny::tags$span(text,
                   style = paste0('color:', color))
}
