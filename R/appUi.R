#' Client (ui) for the nnrr app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  # br_valg <- nnrrBrukerkontroller()

  appTitle <- "NNRR"

  shiny::tagList(

    shiny::navbarPage(
      title = rapbase::regTitle(appTitle),
      windowTitle = appTitle,
      theme = rapbase::rapTheme(),
      id = "tabs",

      shiny::tabPanel(
        "Startside",
        rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),

        nnrr::startside_UI("startside")
      ),
      shiny::tabPanel("Fordelinger",
                      nnrr::fordelingsfig_UI(id = "fordelingsfig_id")
      ),
      shiny::tabPanel("Sykehusvisning",
                      nnrr::sykehusvisning_UI(id = "sykehusvisning_id")
      ),
      shiny::tabPanel("Andeler over tid",
                      nnrr::tidsvisning_UI(id = "tidsvisning_id")
      ),
      shiny::tabPanel("Indikatorer",
                      nnrr::indikatorfig_UI(id = "indikatorfig_id")
      ),
      shiny::tabPanel("Datadump",
                      nnrr::datadump_UI(id = "datadump_id")
                      # h2("Datadump", align='center')
      ),
      shiny::tabPanel("Administrative tabeller",
                      h2("Administrative tabeller", align='center')
      ),
      shiny::tabPanel("Kvartalsrapport",
                      nnrr::samledok_UI(id = "samledok")
      ),

      shiny::tabPanel(
        shiny::span("Abonnement",
                    title="Bestill tilsending av rapporter på e-post"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("nnrrSubscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("nnrrSubscription")
          )
        )
      ),

      shiny::navbarMenu(
        "Verktøy",
        shiny::tabPanel(
          "Utsending",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::autoReportOrgInput("nnrrDispatch"),
              rapbase::autoReportInput("nnrrDispatch")
            ),
            shiny::mainPanel(
              rapbase::autoReportUI("nnrrDispatch")
            )
          )
        ),
        shiny::tabPanel(
          "Metadata",
          shiny::sidebarLayout(
            shiny::sidebarPanel(uiOutput("metaControl")),
            shiny::mainPanel(htmlOutput("metaData"))
          )
        ),

        shiny::tabPanel(
          "Eksport",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::exportUCInput("nnrrExport")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("nnrrExportGuide")
            )
          )
        ),

        shiny::tabPanel(
          "Bruksstatistikk",
          shiny::sidebarLayout(
            shiny::sidebarPanel(rapbase::statsInput("nnrrStats")),
            shiny::mainPanel(
              rapbase::statsUI("nnrrStats"),
              rapbase::statsGuideUI("nnrrStatsGuide")
            )
          )
        )
      )
    )
  )
}
