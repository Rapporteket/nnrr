#' Client (ui) for the nnrr app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  # br_valg <- nnrrBrukerkontroller()

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  appTitle <- "NNRR"

  shiny::tagList(

    shiny::navbarPage(
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "rapbase")
          )
        ),
        appTitle
      ),
      windowTitle = appTitle,
      theme = "rap/bootstrap.css",
      id = "tabs",

      # shiny::tabPanel(
      #   "Start",
      #   shiny::mainPanel(
      #     rapbase::renderRmd(
      #       system.file("veiledning.Rmd", package = "nnrr"),
      #       outputType = "html_fragment"
      #     ),
      #     rapbase::navbarWidgetInput("nnrrNavbarWidget")
      #   )
      # ),

      shiny::tabPanel("Startside",
                      shinyjs::useShinyjs(),
                      rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                   organization = uiOutput("appOrgName"),
                                                   addUserInfo = TRUE),
                      tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
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
      shiny::tabPanel("Abonnement",
                      h2("Kvartalsrapport", align='center')
      ),



      # shiny::tabPanel(
      #   "Eksempelrapport",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       shiny::radioButtons("formatReport",
      #                           "Format for nedlasting",
      #                           list(PDF = "pdf", HTML = "html"),
      #                           inline = FALSE),
      #       shiny::downloadButton("downloadReport", "Last ned!")
      #     ),
      #     shiny::mainPanel(
      #       shiny::htmlOutput("exReport", inline = TRUE)
      #     )
      #
      #   )
      # ),
      #
      # shiny::tabPanel(
      #   "Abonnement",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       rapbase::autoReportInput("nnrrSubscription")
      #     ),
      #     shiny::mainPanel(
      #       rapbase::autoReportUI("nnrrSubscription")
      #     )
      #   )
      # ),
#
#       shiny::tabPanel(
#         "Administrative tabeller",
#         nnrr::admtab_ui("admtabell")
#       ),
#
#
#       shiny::tabPanel(
#         "Registeringer per enhet",
#         shiny::sidebarLayout(
#           shiny::sidebarPanel(
#             shiny::dateRangeInput(
#               inputId = "dato_id", label = "Dato intervall",
#               start = Sys.Date() %m-% months(12), end = Sys.Date()
#             ),
#
#             shiny::selectInput(inputId = "regstatus", label = "Skjemastatus",
#                                choices = c('Ferdigstilt'=1, 'I kladd'=0, 'Opprettet'=-1),
#                                multiple = TRUE,
#                                selected = 1),
#
#
#             shiny::uiOutput("forlopstype_ui"),
#           ),
#           shiny::mainPanel(
#             shiny::tableOutput("tabell_id")
#           )
#         )
#       ),

      shiny::navbarMenu(
        "Verkt\u00f8y",

        shiny::tabPanel(
          "Utsending",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::autoReportFormatInput("nnrrDispatchFormat"),
              rapbase::autoReportOrgInput("nnrrDispatchOrg"),
              rapbase::autoReportInput("nnrrDispatch")
            ),
            shiny::mainPanel(
              rapbase::autoReportUI("nnrrDispatch")
            )
          )
        ),

        shiny::tabPanel(
          "Bruksstatistikk",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::statsInput("nnrrStats"),
              rapbase::statsGuideUI("nnrrStats")
            ),
            shiny::mainPanel(
              rapbase::statsUI("nnrrStats")
            )
          )
        )#,

        # shiny::tabPanel(
        #   "Eksport",
        #   shiny::sidebarLayout(
        #     shiny::sidebarPanel(
        #       rapbase::exportUCInput("nnrrExport")
        #     ),
        #     shiny::mainPanel(
        #       rapbase::exportGuideUI("nnrrExport")
        #     )
        #   )
        # )
      )
    )
  )
}
