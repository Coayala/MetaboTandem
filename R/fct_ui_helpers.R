#' Headerbox factory
#'
#' @description A function to create boxes with headers
#'
#' @return UI elements for boxes
#'
#' @noRd
headerbox_factory <- function(title, status, content,
                              width = 6, id = NULL,
                              collapsible = FALSE,
                              collapsed = FALSE){
  box(title = title,
      status = status,
      solidHeader = TRUE,
      collapsible = collapsible,
      collapsed = collapsed,
      closable = FALSE,
      width = width,
      id = id,
      content
  )
}

colorpicker_factory <- function(id, label){

  string <- paste0(sample(c(0:9, LETTERS[1:6]), 6, replace = TRUE), collapse = '')

  colourpicker::colourInput(id,
                            label = label,
                            value = paste0('#', string),
                            closeOnClick = TRUE)
}

numpicker_factory <- function(id, label){
  numericInput(id,
               label = label,
               value = 0,
               step = 1)
}

next_button <- function(id){
  shinyWidgets::actionBttn(
    inputId = id,
    label = 'Next',
    icon = icon('arrow-right'),
    style = 'jelly',
    color = 'success',
    block = TRUE,
    size = 'sm'
  )
}

back_button <- function(id){
  shinyWidgets::actionBttn(
    inputId = id,
    label = 'Back',
    icon = icon('arrow-left'),
    style = 'jelly',
    color = 'danger',
    block = TRUE,
    size = 'sm'
  )
}

colored_text <- function(text, color){
  shiny::tags$span(text,
                   style = paste0('color:', color))
}

other_arrow_button <- function(id, label){
  shinyWidgets::actionBttn(
    inputId = id,
    label = label,
    icon = icon('arrow-right'),
    style = 'jelly',
    color = 'success',
    block = TRUE,
    size = 'sm'
  )
}
