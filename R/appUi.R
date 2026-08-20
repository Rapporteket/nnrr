#' Client (ui) for the nnrr app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {
  appTitle <- "NNRR"

  shiny::tagList(
    shiny::navbarPage(
      id = "nnrr_app_id",
      title = rapbase::regTitle(appTitle),
      windowTitle = appTitle,
      theme = rapbase::rapTheme(),
      shiny::tabPanel(
        "Startside",
        rapbase::navbarWidgetInput("navbar-widget",
          selectOrganization = TRUE
        ),
        nnrr::startside_UI("startside")
      ),
      shiny::tabPanel(
        "Fordelinger",
        nnrr::fordelingsfig_UI(id = "fordelingsfig_id")
      ),
      shiny::tabPanel(
        "Andeler over tid",
        nnrr::tidsvisning_UI(id = "tidsvisning_id")
      ),
      shiny::tabPanel(
        "Datadump",
        nnrr::datadump_UI(id = "datadump_id")
      ),
      shiny::tabPanel(
        "Administrative tabeller",
        nnrr::admtab_ui(id = "admtabell")
      ),
      shiny::tabPanel(
        "Kvartalsrapport",
        nnrr::samledok_UI(id = "samledok")
      ),
      shiny::tabPanel(
        shiny::span("Abonnement",
          title = "Bestill tilsending av rapporter på e-post"
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("nnrrSubscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("nnrrSubscription")
          )
        )
      )
    )
  )
}
