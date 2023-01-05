#' Run the NNRR Shiny Application
#'
#' @return An object representing the NNRR app
#' @export

nnrrApp <- function() {
  shiny::shinyApp(ui = appUi, server = appServer)
}
