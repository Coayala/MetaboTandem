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

colorpicker_factory <- function(id, label){
  colourpicker::colourInput(id,
                            label = label,
                            value = paste0(
                              '#', paste0(sample(0:9, 6, replace = TRUE),
                                          collapse = '')
                            ),
                            closeOnClick = TRUE)
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
