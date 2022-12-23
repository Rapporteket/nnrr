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

indikatorfigServer <- function(id, RegData, userRole, hvd_session, reshID){
  moduleServer(
    id,

    function(input, output, session) {

      # datadump <- function(input, output, session, reshID, userRole, hvd_session){

      output$lastNed_dump <- downloadHandler(
        filename = function(){
          paste0(input$dumptype, '_NRA', Sys.time(), '.csv')
        },
        content = function(file){
          if (rapbase::isRapContext()) {
            if (input$dumptype == c('alleVarNum_utflatet')) {
              allevar <- nraHentTabell("alleVarNum")
              basisdata <- allevar[allevar$ForlopsType1Num %in% 1:2, ]
              basisdata <- basisdata[, colSums(is.na(basisdata)) != dim(basisdata)[1]]
              oppfdata <- allevar[allevar$ForlopsType1Num %in% 3:4, ]
              oppfdata <- oppfdata[, colSums(is.na(oppfdata)) != dim(oppfdata)[1]]
              oppf1 <- oppfdata[oppfdata$ForlopsType1Num==3, ]
              oppf5 <- oppfdata[oppfdata$ForlopsType1Num==4, ]
              names(oppf1) <- paste0(names(oppf1), "_oppf1")
              names(oppf5) <- paste0(names(oppf5), "_oppf5")
              tmpData <- basisdata %>%
                merge(oppf1, by.x = "ForlopsID", by.y = "KobletForlopsID_oppf1", all.x = T) %>%
                merge(oppf5, by.x = "ForlopsID", by.y = "KobletForlopsID_oppf5", all.x = T)
            } else {
              tmpData <- nraHentTabell(input$dumptype)
            }
          } else {
            tmpData <- read.table(paste0('I:/nra/', input$dumptype, '2020-09-08.txt'),
                                  header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
          }
          dumpdata <- tmpData[which(as.Date(tmpData$HovedDato) >= input$datovalg[1] &
                                      as.Date(tmpData$HovedDato) <= input$datovalg[2]), ]
          if (userRole != 'SC') {
            dumpdata <- dumpdata[dumpdata$AvdRESH %in% reshID, ]
          }
          if (input$dumptype == "ForlopsOversikt") {
            dumpdata <- apply(dumpdata, 2, as.character)
            dumpdata <- as.data.frame(dumpdata)
            dumpdata <- dumpdata[which(dumpdata$PasientID != ""), ]
          }
          write.csv2(dumpdata, file, row.names = F, na = '', fileEncoding = 'Latin1')
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
