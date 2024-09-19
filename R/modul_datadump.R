#' Modul for datadump-fane i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
datadump_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_dump_panel"),
      dateRangeInput(inputId=ns("datovalg"),
                     label = "Dato fra og til",
                     language = "nb",
                     max = Sys.Date(),
                     start  = '2014-01-01',
                     end = Sys.Date(),
                     separator = " til "),
      selectInput(inputId = ns("dumptype"),
                  label = "Velg type datadump",
                  choices = c("Pasientskjema før behandling" = "pasientsvar_pre",
                              "Behandlerskjema" = "legeskjema",
                              "Pasientskjema 6 mnd." = "pasientsvar_post",
                              "Pasientskjema 12 mnd." = "pasientsvar_post2",
                              "NNRRdata utflatet" = "nnrr_utflatet")),
      tags$hr(),
      downloadButton(ns("lastNed_dump"), "Last ned datadump")
    ),
    mainPanel(
      h2('Datadump - NNRR', align='center'),
      br(),
      h4('Her kan du laste ned forskjellige varianter av datadump for NNRR.
         Lokale brukere vil bare kunne laste ned data for egen avdeling.')#,
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

datadump_Server <- function(id, RegData, userRole, hvd_session, reshID){
  moduleServer(
    id,

    function(input, output, session) {

      output$lastNed_dump <- downloadHandler(
        filename = function(){
          fs::path_sanitize(paste0(input$dumptype, Sys.time(), '.csv'))
        },
        content = function(file){
          if (rapbase::isRapContext()) {
            if (input$dumptype == "nnrr_utflatet") {
              tmpData <- RegData
            } else {
              tmpData <- nnrrHentTabell(input$dumptype)
            }
          } else {
            tmpData <- switch (
              input$dumptype,
              "pasientsvar_pre" = readr::read_csv2(
                'C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+for+behandling_2022-12-09_1116_v2.csv'),
              "legeskjema" = readr::read_csv2(
                'C:/GIT/data/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-12-09_1116.csv'),
              # "legeskjema" = read.table(
              #   'C:/GIT/data/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-12-09_1116.csv', sep=';',
              #   header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F),
              "pasientsvar_post" =  read.table(
                'C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+6+maneder+etter+behandling_2022-12-09_1116.csv',
                sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F),
              "pasientsvar_post2" = read.table(
                'C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+12+maneder+etter+behandling_2022-12-09_1116.csv',
                sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F),
              "nnrr_utflatet" = RegData
            )
          }
          dumpdata <- tmpData[which(as.Date(tmpData$S1b_DateOfCompletion, format="%d.%m.%Y") >= input$datovalg[1] &
                                      as.Date(tmpData$S1b_DateOfCompletion, format="%d.%m.%Y") <= input$datovalg[2]), ]
          if (userRole != 'SC') {
            dumpdata <- dumpdata[dumpdata$UnitId %in% reshID, ]
          }
          # write.csv2(dumpdata, file, row.names = F, na = '', fileEncoding = 'Latin1')
          readr::write_excel_csv2(dumpdata, file)
        }
      )

      shiny::observe({
        if (rapbase::isRapContext()) {

          shinyjs::onclick(
            "lastNed_dump",
            rapbase::repLogger(
              session = hvd_session,
              msg = paste0("NNRR: nedlasting datadump: ", input$dumptype)
            )
          )
        }
      })

    }
  )}
