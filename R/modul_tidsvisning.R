#' UI-modul for tidsvisninger i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
tidsvisning_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_panel"),
      selectInput(inputId = ns("valgtVar"),
                  label = "Velg variabel",
                  choices =   c("Tverrfaglig behandlet" = "tverrfaglig_behandlet",
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
                     language = "nb",
                     max = Sys.Date(),
                     start  = lubridate::floor_date(lubridate::today() - lubridate::years(1), unit = "year"),
                     end = Sys.Date(), separator = " til "),
      selectInput(inputId = ns("tidsenhet"),
                  label = "Velg tidsenhet",
                  choices = c('Aar', 'Mnd', 'Kvartal', 'Halvaar'),
                  selected = 'Kvartal'),
      selectInput(inputId = ns("inkl_konf"),
                  label = "Inkluder konfidensintervall",
                  choices = c("Ja" = 1, "Nei" = 0)),
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

#' Server-modul for tidsvisninger i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
tidsvisningServer <- function(id, reshID, RegData, userRole, hvd_session){
  moduleServer(
    id,

    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_panel")
      })

      sykehus <- RegData$UnitId[match(sort(unique(RegData$SykehusNavn)), RegData$SykehusNavn)]
      names(sykehus) <- sort(unique(RegData$SykehusNavn))

      output$sykehus_ui <- shiny::renderUI({
        ns <- session$ns
        selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                    choices = sykehus, multiple = T)
      })

      observe(
        if (userRole != 'SC') {
          shinyjs::hide(id = 'valgtShus')
        }
      )

      datovar <- shiny::reactive({
        switch(input$valgtVar,
               tverrfaglig_behandlet = "Besoksdato",
               individuell_oppfolging = "Besoksdato",
               fabq11 = "Besoksdato",
               Oppfolging_utfylt = "dato_oppfolg",
               opplevd_nytte_beh = "dato_oppfolg",
               odi_klinisk_viktig = "dato_oppfolg",
               odi_klinisk_viktig_v2 = "dato_oppfolg",
               bedring_smerte_hvile = "dato_oppfolg",
               bedring_smerte_aktiv = "dato_oppfolg",
               misfornoyd = "dato_oppfolg",
               fornoyd = "dato_oppfolg",
               HSCL10.Score = "dato_oppfolg",
               smerter_2aar = "dato_oppfolg"
               )
      })

      tabellReager <- reactive({
        TabellData <- nnrr::nnrrBeregnAndelTid(RegData = RegData,
                                              valgtVar=input$valgtVar,
                                              datovar = datovar(),
                                              datoFra=input$datovalg[1],
                                              datoTil=input$datovalg[2],
                                              minald=as.numeric(input$alder[1]),
                                              maxald=as.numeric(input$alder[2]),
                                              erMann=as.numeric(input$erMann),
                                              reshID=reshID,
                                              tidsenhet=input$tidsenhet,
                                              tverrfaglig = as.numeric(input$tverrfaglig),
                                              minHSCL = input$HSCL[1],
                                              maxHSCL = input$HSCL[2],
                                              medikamenter = input$medikamenter,
                                              smerte = as.numeric(input$smerte),
                                              tolk = as.numeric(input$tolk),
                                              enhetsUtvalg=input$enhetsUtvalg)
      })

      output$Figur1 <- renderPlot({
        nnrr::nnrrTidsplot(plotdata = tabellReager(),
                           outfile='', inkl_konf = as.numeric(input$inkl_konf))
      }, width = 700, height = 700)

      output$utvalg <- renderUI({
        TabellData <- tabellReager()
        tagList(
          h3(HTML(paste0(TabellData$Tittel, '<br />'))),
          h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
        )})

      output$Tabell1 <- function() {

        utdata <- tabellReager()
        if (input$enhetsUtvalg == 1) {
          Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                               N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                               Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                               N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                               Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
          names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel (%)', 'KI_nedre', 'KI_ovre', 'Antall', 'N', 'Andel (%)',
                                 'KI_nedre', 'KI_ovre')
          Tabell_tid %>% knitr::kable("html", digits = c(0,0,0,1,1,1,0,0,1,1,1)) %>%
            kableExtra::kable_styling("hover", full_width = F) %>%
            kableExtra::add_header_above(c(" ", "Din avdeling" = 5, "Landet forøvrig" = 5))
        } else {
          Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$Tidtxt,
                               Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                               N = utdata$NTid$NTidHoved, 'Andel (%)'= utdata$Andeler$AndelHoved, KI_nedre = utdata$KonfInt$Konf[1,],
                               KI_ovre = utdata$KonfInt$Konf[2,])
          Tabell_tid %>%
            knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
            kableExtra::kable_styling("hover", full_width = F)
        }
      }

      #
      # output$lastNed <- downloadHandler(
      #   filename = function(){
      #     paste0(input$valgtVar, Sys.time(), '.csv')
      #   },
      #
      #   content = function(file){
      #     TabellData <- tabellReager()
      #     if (input$enhetsUtvalg == 1) {
      #       Tabell1 <- dplyr::tibble("Kategori" = TabellData$GruppeTekst,
      #                                "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
      #                                "Antall totalt" = unname(TabellData$N[,1]),
      #                                "Andel (%)" = unname(TabellData$Andeler[1,]),
      #                                "Antall i kategori (resten)" = round(unname(TabellData$Andeler[2,])*unname(TabellData$N[,2])/100),
      #                                "Antall totalt (resten)" = unname(TabellData$N[,2]),
      #                                "Andel (%) (resten)" = unname(TabellData$Andeler[2,]))
      #     } else {
      #       Tabell1 <- dplyr::tibble("Kategori" = TabellData$GruppeTekst,
      #                                "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
      #                                "Antall totalt" = unname(TabellData$N[,1]),
      #                                "Andel (%)" = unname(TabellData$Andeler[1,]))
      #     }
      #     write.csv2(Tabell1, file, row.names = F, fileEncoding = 'latin1')
      #   }
      # )
      #
      # output$lastNedBilde <- downloadHandler(
      #   filename = function(){
      #     paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
      #   },
      #
      #   content = function(file){
      #     nnrr::nnrrSoyleplot(plotdata = tabellReager(),
      #                         outfile=file)
      #   }
      # )

      # shiny::observe({
      #   if (rapbase::isRapContext()) {
      #     if (req(input$tab) == "fig") {
      #       mld_fordeling <- paste0(
      #         "NoRGast: Figur - fordeling, variabel - ",
      #         input$valgtVar)
      #     }
      #     if (req(input$tab) == "tab") {
      #       mld_fordeling <- paste(
      #         "NoRGast: tabell - fordeling. variabel - ",
      #         input$valgtVar)
      #     }
      #     raplog::repLogger(
      #       session = hvd_session,
      #       msg = mld_fordeling
      #     )
      #     mldLastNedFig <- paste(
      #       "NoRGast: nedlasting figur - fordeling. variabel -",
      #       input$valgtVar
      #     )
      #     mldLastNedTab <- paste(
      #       "NoRGast: nedlasting tabell - fordeling. variabel -",
      #       input$valgtVar
      #     )
      #     shinyjs::onclick(
      #       "lastNedBilde",
      #       raplog::repLogger(
      #         session = hvd_session,
      #         msg = mldLastNedFig
      #       )
      #     )
      #     shinyjs::onclick(
      #       "lastNedTabell",
      #       raplog::repLogger(
      #         session = hvd_session,
      #         msg = mldLastNedTab
      #       )
      #     )
      #   }
      # })

    }
  )
}
