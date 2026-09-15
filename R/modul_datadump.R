#' Modul for datadump-fane i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
datadump_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_dump_panel"),
      dateRangeInput(
        inputId = ns("datovalg"),
        label = "Dato fra og til",
        language = "nb",
        max = Sys.Date(),
        start = "2014-01-01",
        end = Sys.Date(),
        separator = " til "
      ),
      selectInput(
        inputId = ns("dumptype"),
        label = "Velg type datadump",
        choices = c(
          "Pasientskjema før behandling" = "pasientsvar_pre",
          "Behandlerskjema" = "legeskjema",
          "Pasientskjema 6 mnd." = "pasientsvar_post",
          "Pasientskjema 12 mnd." = "pasientsvar_post2",
          "NNRRdata utflatet" = "nnrr_utflatet"
        )
      ),
      tags$hr(),
      downloadButton(ns("lastNed_dump"), "Last ned datadump")
    ),
    mainPanel(
      h2("Datadump - NNRR", align = "center"),
      br(),
      h4("Her kan du laste ned forskjellige varianter av datadump for NNRR.
         Lokale brukere vil bare kunne laste ned data for egen avdeling.") # ,
      # br(),
      # h4(tags$b(tags$u('Forklaring til de ulike datadump-typene:'))),
      # h4(tags$b('alleVar '), 'inneholder alle kliniske variabler i registeret og benytter etikettene til kategoriske variabler.'),
      # h4(tags$b('alleVarNum '), 'inneholder alle kliniske variabler i registeret og benytter tallkodene til kategoriske variabler.'),
      # h4(tags$b('ForlopsOversikt '), 'inneholder en del administrative data relevant for forløpene.'),
      # h4(tags$b('SkjemaOversikt '), 'er en oversikt over status til alle registreringer i registreret, også uferdige.'),
      # h4(tags$b('alleVarNum_utflatet '), 'inneholder alle kliniske variabler i registeret og benytter tallkodene til kategoriske variabler.
      #    At tabellen er utflatet innebærer at oppfølginger er koblet til sine respective basisregistreringer slik at en linje utgjør et forløp.')
    )
  )
}

#' Modul for serverdel av datadump i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
datadump_Server <- function(id, RegData,
                            hvd_session, user) {
  moduleServer(
    id,
    function(input, output, session) {

      output$lastNed_dump <- downloadHandler(
        filename = function() {
          fs::path_sanitize(paste0(input$dumptype, Sys.time(), ".csv"))
        },
        content = function(file) {
          if (input$dumptype == "nnrr_utflatet") {
            tmpData <- RegData
          } else {
            tmpData <- nnrrHentTabell(
              tabellnavn = input$dumptype,
              datoFra = input$datovalg[1],
              datoTil = input$datovalg[2]
            )
          }
          dumpdata <- tmpData[
            which(as.Date(tmpData$S1b_DateOfCompletion,
                          format = "%d.%m.%Y") >= input$datovalg[1] &
                    as.Date(tmpData$S1b_DateOfCompletion,
                            format = "%d.%m.%Y") <= input$datovalg[2]), ]
          if (user$role() != "SC") {
            dumpdata <- dumpdata[dumpdata$UnitId %in% user$org(), ]
          }
          readr::write_excel_csv2(dumpdata, file)
          rapbase::repLogger2(
            user = user,
            msg = paste0(
              "NNRR: nedlasting datadump: ",
              input$dumptype, " ", input$datovalg[1],
              " til ", input$datovalg[2]
            )
          )
        }
      )

    }
  )
}
