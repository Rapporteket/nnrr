#' UI-modul for Samledokumenter-fane i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Samledokumenter
#'
#' @export
samledok_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      id = ns("id_samledok_panel"),
      shiny::selectInput(inputId = ns("valgtAar"), label = "Frem til år",
                         choices = if (Sys.Date() %>% as.character() %>% substr(6,7) %>%
                                       as.numeric() >= 4) {
                           rev(2014:as.numeric(format(Sys.Date(), '%Y')))
                         } else {
                           rev(2014:(as.numeric(format(Sys.Date(), '%Y'))-1))
                         }),
      uiOutput(outputId = ns('kvartal_ui')),
      uiOutput(outputId = ns('valgtShus_ui')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(
        id= ns("tabs"),
        tabPanel("Kvartalsrapport for din avdeling", value = "kvartalsrapport",
                 downloadButton(ns("lastNed_kvartal"), "Last ned kvartalsrapport")))
    )
  )
}

#' Server-modul for Samledokumenter-fane i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Samledokumenter
#'
#' @export
#'
samledok_server <- function(id, reshID, RegData, userRole, hvd_session){
  moduleServer(
    id,

    function(input, output, session) {

      sykehus <- setNames(unique(RegData$UnitId),
                          RegData$SykehusNavn[match(unique(RegData$UnitId),
                                                    RegData$UnitId)])

      observeEvent(input$reset_input, {
        shinyjs::reset("id_samledok_panel")
      })

      observe(
        if (userRole != 'SC') {
          shinyjs::hide(id = 'valgtShus_ui')
        })

      output$valgtShus_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                    choices = sykehus, multiple = TRUE)
      })

      output$kvartal_ui <- renderUI({
        ns <- session$ns
        if (!is.null(input$valgtAar)) {
          selectInput(inputId = ns("kvartal"), label = "Til og med (avsluttet) kvartal",
                      choices = if (input$valgtAar == format(Sys.Date(), '%Y')) {
                        ant_kvartal <- match(Sys.Date() %>% as.Date() %>%
                                               lubridate::floor_date(unit = 'quarter') %>%
                                               as.character() %>% substr(6,10),
                                             c('04-01', '07-01', '10-01'))
                        rev(setNames(paste0(input$valgtAar, c('-04-01', '-07-01', '-10-01'))[1:ant_kvartal], paste0(1:ant_kvartal, '. kvartal')))
                      } else {
                        rev(setNames(paste0(c(input$valgtAar, input$valgtAar, input$valgtAar, as.numeric(input$valgtAar) + 1),
                                            c('-04-01', '-07-01', '-10-01', '-01-01')), paste0(1:4, '. kvartal')))
                      })
        }
      })

      contentFile2 <- function(file, baseName, datoTil, reshID=0, valgtShus='') {

        src <- system.file(paste0(baseName, ".Rnw"), package = "nnrr")
        tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.Rnw')

        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, tmpFile, overwrite = TRUE)

        pdfFile <- knitr::knit2pdf(tmpFile)
        file.copy(pdfFile, file)
      }


      output$lastNed_kvartal <- downloadHandler(
        filename = function(){
          if (is.null(input$valgtShus)) {
          paste0("Kvartalsrapp",
                 RegData$SykehusNavn[match(reshID, RegData$UnitId)],
                 format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), ".pdf")
          } else {
            paste0("Kvartalsrapp",
                   paste(RegData$SykehusNavn[match(as.numeric(input$valgtShus),
                                                   RegData$UnitId)], collapse = "_"),
                   format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), ".pdf")
            }
        },
        content = function(file){
          contentFile2(
            file, "KvartalsrapportNNRR_rapporteket",
            datoTil = input$kvartal, reshID = reshID,
            valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {""})

        }
      )

      # shiny::observe({
      #   if (rapbase::isRapContext()) {
      #
      #     shinyjs::onclick(
      #       "lastNed_saml",
      #       rapbase::repLogger(
      #         session = hvd_session,
      #         msg = "NoRGast: samledokument nedlastet"
      #       )
      #     )
      #
      #     shinyjs::onclick(
      #       "lastNed_saml_land",
      #       rapbase::repLogger(
      #         session = hvd_session,
      #         msg = "NoRGast: samledokument med nasjonale talll nedlastet"
      #       )
      #     )
      #     shinyjs::onclick(
      #       "lastNed_kvartal",
      #       rapbase::repLogger(
      #         session = hvd_session,
      #         msg = "NoRGast: Kvartalsrapport nedlastet"
      #       )
      #     )
      #   }
      # })


    }
  )
}
