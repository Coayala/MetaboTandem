#' Perform multivariate Analysis
library(plotly)
#'
#' @param id character used to specify namespace, see [`shiny::NS`][shiny::NS()]
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements

multivariateUI <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = 'Options',
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      status = 'primary',
      column(width = 6,
             selectizeInput(ns('group'),
                            label = 'Choose grouping variables for plot',
                            choices = NULL)),
      column(width = 6,
             shinyWidgets::materialSwitch(ns('label'),
                                          label = 'Toogle sample names',
                                          status = 'primary')),
      hr(),
      actionButton(ns('calculate'), label = 'Calculate ordination')
    ),
    box(
      title = 'NMDS',
      solidHeader = TRUE,
      status = 'primary',
      #plotOutput(ns('stress')),
      plotlyOutput(ns('nmds'))
    ),
    box(
      title = 'PCA',
      solidHeader = TRUE,
      status = 'primary',
      #plotOutput(ns('scree')),
      plotlyOutput(ns('PCA'))
    ),
    box(
      title = 'PERMANOVA',
      solidHeader = TRUE,
      width = 12,
      status = 'primary',
      tableOutput(ns('permanova'))
    )
  )
}


#######
multivariateServer <- function(id, norm_df, metadata, user_colors){
  moduleServer(id, function(input, output, session){

    observe({
      updateVarSelectizeInput(session, 'group', data = metadata(),
                              server = TRUE)
    })

    # Reactive functions for NMDS, PCA, and PERMANOVA
    nmds_ord <- reactive({
      # Assuming nmds_ordination returns the required data and plot
      nmds_ordination(norm_df(), metadata(), mode = 'ra', color_by = input$group)
    }) %>%
      bindEvent(input$calculate)

    pca_ord <- reactive({
      # Assuming pca_ordination returns the required data and plot
      pca_ordination(norm_df(), metadata(), color_by = input$group)
    }) %>%
      bindEvent(input$calculate)

    permanova_table <- reactive({
      # Assuming calculate_permanova returns the required table data
      calculate_permanova(norm_df(), metadata(), mode = 'ra', group_by = input$group)
    }) %>%
      bindEvent(input$calculate)

    # Output for NMDS Plot
    output$nmds <- renderPlotly({
      req(nmds_ord()) # Ensure nmds_ord is available
      nmds_plot <- nmds_ord()$nmds_plot

      if(input$label){
        nmds_plot <- nmds_plot + geom_label(aes(label = SampleID))
      }

      if(!all(user_colors() == 'Default')){
        nmds_plot <- nmds_plot + scale_color_manual(values = user_colors())
      }

      ggplotly(nmds_plot)
    }) %>%
      bindEvent(nmds_ord())

    # Output for PCA Scree Plot
    output$scree <- renderPlotly({
      req(pca_ord()) # Ensure pca_ord is available
      ggplotly(pca_ord()$scree_plot)
    }) %>%
      bindEvent(pca_ord())

    # Output for PCA Plot
    output$PCA <- renderPlotly({
      req(pca_ord()) # Ensure pca_ord is available
      pca_plot <- pca_ord()$pca_plot

      if(input$label){
        pca_plot <- pca_plot + geom_label(aes(label = SampleID))
      }

      if(!all(user_colors() == 'Default')){
        pca_plot <- pca_plot + scale_color_manual(values = user_colors())
      }

      ggplotly(pca_plot)
    }) %>%
      bindEvent(pca_ord())

    # Output for PERMANOVA Table
    output$permanova <- renderTable({
      req(permanova_table()) # Ensure permanova_table is available
      permanova_table() %>%
        tibble::rownames_to_column(var = 'treatment')
    }, striped = TRUE, bordered = TRUE)
  })
}
#######



# multivariateServer <- function(id, norm_df, metadata, user_colors){
#   moduleServer(id, function(input, output, session){
#
#     observe({
#       updateVarSelectizeInput(session, 'group', data = metadata(),
#                               server = TRUE)
#     })
#
#
#     # Calculate multivariate statistics
#
#
#     ####Plotly interactive graph code
#
#
#
#     nmds_ord <- reactive({
#       nmds_ordination(norm_df(), metadata(), mode = 'ra', color_by = input$group)
#     }) %>%
#       bindEvent(input$calculate)
#
#     pca_ord <- reactive({
#       pca_ordination(norm_df(), metadata(), color_by = input$group)
#     }) %>%
#       bindEvent(input$calculate)
#
#     permanova_table <- reactive({
#       calculate_permanova(norm_df(), metadata(), mode = 'ra',
#                           group_by =  input$group)
#     }) %>%
#       bindEvent(input$calculate)
#
#     # Calculate outputs
#
#     # output$stress <- renderPlot(
#     #   vegan::stressplot(nmds_ord()$nmds)
#     # ) %>%
#     #   bindEvent(nmds_ord())
#
#     output$nmds <- renderPlot({
#       if(input$label){
#         nmds_plot <- nmds_ord()$nmds_plot +
#           geom_label(aes(label = SampleID))
#       } else {
#         nmds_plot <- nmds_ord()$nmds_plot
#       }
#
#       if(all(user_colors() == 'Default')){
#         nmds_plot
#       } else {
#         nmds_plot +
#           scale_color_manual(values = user_colors())
#       }
#
#     }) %>%
#       bindEvent(nmds_ord())
#
#     output$scree <- renderPlot(
#       pca_ord()$scree_plot
#     ) %>%
#       bindEvent(pca_ord())
#
#     output$PCA <- renderPlot({
#       if(input$label){
#         pca_plot <- pca_ord()$pca_plot +
#           geom_label(aes(label = SampleID))
#       } else {
#         pca_plot <- pca_ord()$pca_plot
#       }
#
#       if(all(user_colors() == 'Default')){
#         pca_plot
#       } else {
#         pca_plot +
#           scale_color_manual(values = user_colors())
#       }
#
#     }) %>%
#       bindEvent(pca_ord())
#
#     output$permanova <- renderTable(
#       permanova_table() %>%
#         tibble::rownames_to_column(var = 'treatment'),
#       striped = TRUE,
#       bordered = TRUE
#     )
#   })
# }
#
#
