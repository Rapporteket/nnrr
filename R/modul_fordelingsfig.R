#' UI-modul for fordelingsfigurer i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
fordelingsfig_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_fordeling_panel"),
      selectInput(inputId = ns("valgtVar"),
                  label = "Velg variabel",
                  choices =   c("Sivilstatus" = "FamilyStatus",
                                "Alder ved registrering" = "PatientAge",
                                "FABQ fysisk aktivitet" = "FABQScore1",
                                "FABQ arbeid" = "FABQScore2",
                                "Sammenhengende varighet av nåværende smerter" = "PainDurationNow",
                                "Pasientrapportert årsak til smerte" = "AarsakSmerte_PasRap",
                                "Behandling i kommunalhelsetjenesten" = "beh_kommunalt",
                                "Behandling i spesialisthelsetjenesten" = "beh_spesialist",
                                "Pasientrapportert behandling på nakke- og ryggpoliklinikk v2" = "pasrapp_beh_klinikk_v2",
                                "EQ5D tilfredshet med behandling" = "Eq5dSatisfactionTreatment",
                                "Tverrfaglig behandlet" = "tverrfaglig_behandlet",
                                "Individuell oppfølging" = "individuell_oppfolging",
                                "FABQ11" = "fabq11",
                                "Utfylt oppfølging" = "Oppfolging_utfylt",
                                "Opplevd nytte av behandling" = "opplevd_nytte_beh",
                                "Funksjonsbedring" = "odi_klinisk_viktig",
                                "Funksjonsbedring v2" = "odi_klinisk_viktig_v2",
                                "Klinisk bedring av smerte i hvile" = "bedring_smerte_hvile",
                                "Klinisk bedring av smerte i aktivitet" = "bedring_smerte_aktiv",
                                "Misfornøyd med behandling" = "misfornoyd",
                                "Fornøyd med behandling" = "fornoyd",
                                "Hopkins symptom checklist" = "HSCL10.Score",
                                "Smertevarighet > 2 år" = "smerter_2aar"
                  )),
      dateRangeInput(inputId=ns("datovalg"),
                     label = "Dato fra og til",
                     min = '2014-01-01',
                     max = Sys.Date(),
                     start  = Sys.Date() %m-% months(12) + 1,
                     end = Sys.Date(),
                     language = "nb",
                     separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"),
                  label = "Kjør rapport for",
                  choices = c('Hele landet'=0,
                              'Egen avd. mot landet forøvrig'=1,
                              'Egen avd.'=2)),
      shiny::uiOutput(ns("sykehus_ui")),
      sliderInput(inputId=ns("alder"),
                  label = "Alder",
                  min = 0,
                  max = 120,
                  value = c(0, 120)),
      selectInput(inputId = ns("erMann"),
                  label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("tverrfaglig"),
                  label = "Tverrfaglig behandlet",
                  choices = c('--'=99, 'Nei'=0, 'Ja'=1)),
      sliderInput(inputId=ns("HSCL"),
                  label = "HSCL score",
                  min = 1,
                  max = 4,
                  value = c(1, 4),
                  step = 0.01),
      shiny::checkboxGroupInput(inputId = ns("medikamenter"),
                                label = "Medikamenter (Bruker minst én av de avkryssede)",
                                choices = c("A-preparat" = "MedicationA",
                                            "B-preparat" = "MedicationB",
                                            "C-preparat" = "MedicationC")),
      shiny::selectInput(inputId = ns("smerte"),
                         label = "Smertevarighet",
                         choices = c('Ingen smerter' = 1,
                                     'Mindre enn 3 måneder' = 2,
                                     '3 til 12 måneder' = 3,
                                     '1-2 år' = 4,
                                     'Mer enn 2' = 5,
                                     'Ikke svart' = 99),
                         multiple = TRUE),
      shiny::selectInput(inputId = ns("tolk"),
                           label = "Tolk",
                          choices = c("--"=99, "Ja" = 1, "Nei"=0)),
      # uiOutput(ns("slider")),
      selectInput(inputId = ns("bildeformat"),
                  label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      tags$hr(),
      actionButton(ns("reset_input"),
                   "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur",
                           value = "fig",
                           verbatimTextOutput(ns("value")),
                           plotOutput(ns("Figur1"),
                                      height="auto"),
                           downloadButton(ns("lastNedBilde"),
                                          "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           uiOutput(ns("utvalg")),
                           br(),
                           tableOutput(ns("Tabell1")),
                           downloadButton(ns("lastNed"),
                                          "Last ned tabell"))
      )
    )
  )

}


#' Server-modul for fordelingsfigurer i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
fordelingsfigServer <- function(id, reshID, RegData, userRole, hvd_session){
  moduleServer(
    id,

    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_fordeling_panel")
      })

      sykehus <- RegData$UnitId[match(sort(unique(RegData$SykehusNavn)), RegData$SykehusNavn)]
      names(sykehus) <- sort(unique(RegData$SykehusNavn))

      output$sykehus_ui <- shiny::renderUI({
        ns <- session$ns
        selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                    choices = sykehus, multiple = T)
      })

      # output$slider <- renderUI({
      #
      #   # args       <- list(inputId="foo", label="slider :", ticks=c(90,95,99,99.9), value=c(2,3))
      #   args       <- list(inputId="foo", label="slider :", ticks=c(1, 1.5, 1.85, 2, 2.5, 3, 3.5, 4),
      #                      value=c(1,4), step=0.1)
      #
      #   args$min   <- 1
      #   args$max   <- 4
      #
      #   if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {
      #     # this part works with shiny 1.5.0
      #     ticks <- paste0(args$ticks, collapse=',')
      #     args$ticks <- T
      #     html  <- do.call('sliderInput', args)
      #
      #     html$children[[2]]$attribs[['data-values']] <- ticks;
      #     # html$children[[2]]$attribs[['data-values']] <- paste0(seq(from = 1, to = 4, by = 0.1), collapse=',');
      #
      #   } else {
      #     html  <- do.call('sliderInput', args)
      #   }
      #
      #   html
      # })

      observe(
        if (userRole != 'SC') {
          shinyjs::hide(id = 'valgtShus')
        }
      )

      # observe(
      #   print(paste0(input$tolk))
      # )

      tabellReager <- reactive({
        TabellData <- nnrr::nnrrBeregnAndeler(RegData = RegData,
                                              valgtVar=input$valgtVar,
                                              datoFra=input$datovalg[1],
                                              datoTil=input$datovalg[2],
                                              minald=as.numeric(input$alder[1]),
                                              maxald=as.numeric(input$alder[2]),
                                              erMann=as.numeric(input$erMann),
                                              reshID=reshID,
                                              tverrfaglig = as.numeric(input$tverrfaglig),
                                              minHSCL = input$HSCL[1],
                                              maxHSCL = input$HSCL[2],
                                              medikamenter = input$medikamenter,
                                              smerte = as.numeric(input$smerte),
                                              tolk = as.numeric(input$tolk),
                                              enhetsUtvalg=input$enhetsUtvalg)
      })

      output$Figur1 <- renderPlot({
        nnrr::nnrrSoyleplot(plotdata = tabellReager(),
                            outfile='')
      }, width = 700, height = 700)

      output$utvalg <- renderUI({
        TabellData <- tabellReager()
        tagList(
          h3(HTML(paste0(TabellData$Tittel, '<br />'))),
          h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
        )})

      output$Tabell1 <- function() {
        TabellData <- tabellReager()
        if (input$enhetsUtvalg == 1) {
          Tabell1 <- data.frame("Kategori" = TabellData$GruppeTekst,
                                "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
                                "Antall totalt" = unname(TabellData$N[,1]),
                                "Andel (%)" = unname(TabellData$Andeler[1,]),
                                "Antall i kategori" = round(unname(TabellData$Andeler[2,])*unname(TabellData$N[,2])/100),
                                "Antall totalt" = unname(TabellData$N[,2]),
                                "Andel (%)" = unname(TabellData$Andeler[2,]))
          names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
          Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
            kableExtra::kable_styling("hover", full_width = F) %>%
            kableExtra::add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
        } else {
          Tabell1 <- data.frame("Kategori" = TabellData$GruppeTekst,
                                "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
                                "Antall totalt" = unname(TabellData$N[,1]),
                                "Andel (%)" = unname(TabellData$Andeler[1,]))
          names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
          Tabell1 %>%
            knitr::kable("html", digits = c(0,0,0,1)) %>%
            kableExtra::kable_styling("hover", full_width = F)
        }
      }

      output$lastNed <- downloadHandler(
        filename = function(){
          fs::path_sanitize(paste0(input$valgtVar, Sys.time(), '.csv'))
        },

        content = function(file){
          TabellData <- tabellReager()
          if (input$enhetsUtvalg == 1) {
            Tabell1 <- dplyr::tibble("Kategori" = TabellData$GruppeTekst,
                                     "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
                                     "Antall totalt" = unname(TabellData$N[,1]),
                                     "Andel (%)" = unname(TabellData$Andeler[1,]),
                                     "Antall i kategori (resten)" = round(unname(TabellData$Andeler[2,])*unname(TabellData$N[,2])/100),
                                     "Antall totalt (resten)" = unname(TabellData$N[,2]),
                                     "Andel (%) (resten)" = unname(TabellData$Andeler[2,]))
          } else {
            Tabell1 <- dplyr::tibble("Kategori" = TabellData$GruppeTekst,
                                     "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
                                     "Antall totalt" = unname(TabellData$N[,1]),
                                     "Andel (%)" = unname(TabellData$Andeler[1,]))
          }
          write.csv2(Tabell1, file, row.names = F, fileEncoding = 'Latin1')
        }
      )

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          fs::path_sanitize(paste0(input$valgtVar, Sys.time(), '.', input$bildeformat))
        },

        content = function(file){
          nnrr::nnrrSoyleplot(plotdata = tabellReager(),
                              outfile=file)
        }
      )

      shiny::observe({
        # if (rapbase::isRapContext()) {
          if (req(input$tab) == "fig") {
            mld_fordeling <- paste0(
              "NoRGast: Figur - fordeling, variabel - ",
              input$valgtVar)
          }
          if (req(input$tab) == "tab") {
            mld_fordeling <- paste(
              "NoRGast: tabell - fordeling. variabel - ",
              input$valgtVar)
          }
          rapbase::repLogger(
            session = hvd_session,
            msg = mld_fordeling
          )
          mldLastNedFig <- paste(
            "NoRGast: nedlasting figur - fordeling. variabel -",
            input$valgtVar
          )
          mldLastNedTab <- paste(
            "NoRGast: nedlasting tabell - fordeling. variabel -",
            input$valgtVar
          )
          shinyjs::onclick(
            "lastNedBilde",
            rapbase::repLogger(
              session = hvd_session,
              msg = mldLastNedFig
            )
          )
          shinyjs::onclick(
            "lastNed",
            rapbase::repLogger(
              session = hvd_session,
              msg = mldLastNedTab
            )
          )
        # }
      })

    }
  )
}
