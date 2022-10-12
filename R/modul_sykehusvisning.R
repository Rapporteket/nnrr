#' UI-modul for sykehusvise figurer i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
sykehusvisning_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_sykehusvisning_panel"),
      selectInput(inputId = ns("valgtVar"),
                  label = "Velg variabel",
                  choices = c("ODI_PrePost",
                              "NDI_PrePost",
                              "EQ5D_PrePost",
                              "PainExperiencesNoActivity",
                              "PainExperiencesActivity")),
      selectInput(inputId = ns("sammenlign"),
                  label = "Vis verdi:",
                  choices = c("Kun ved utredning" = 0,
                              "Ved utredning og etter 6mnd." = 1,
                              "Ved utredning og etter 6 og 12 mnd." = 2)),
      dateRangeInput(inputId=ns("datovalg"),
                     label = "Dato fra og til",
                     min = '2014-01-01',
                     max = Sys.Date(),
                     start  = Sys.Date() %m-% months(12) + 1,
                     end = Sys.Date(),
                     language = "nb",
                     separator = " til "),
      sliderInput(inputId=ns("alder"),
                  label = "Alder",
                  min = 0,
                  max = 120,
                  value = c(0, 120)),
      selectInput(inputId = ns("erMann"),
                  label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
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



#' Server-modul for sykehusvise figurer i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
sykehusvisningServer <- function(id, RegData, userRole, hvd_session){
  moduleServer(
    id,

    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_sykehusvisning_panel")
      })

      tabellReager <- reactive({
        TabellData <- nnrr::nnrrBeregnGjsnPrePostGrVar(RegData = RegData,
                                                       valgtVar=input$valgtVar,
                                                       datoFra=input$datovalg[1],
                                                       datoTil=input$datovalg[2],
                                                       minald=as.numeric(input$alder[1]),
                                                       maxald=as.numeric(input$alder[2]),
                                                       erMann=as.numeric(input$erMann),
                                                       sammenlign = as.numeric(input$sammenlign),
                                                       gr_var='SykehusNavn')
      })

      output$Figur1 <- renderPlot({
        nnrr::nnrrPlotGjsnPrePostGrVar(plotparams = tabellReager(),
                                       outfile='')
      }, width = 700, height = 700)
      #
      # output$utvalg <- renderUI({
      #   TabellData <- tabellReager()
      #   tagList(
      #     h3(HTML(paste0(TabellData$Tittel, '<br />'))),
      #     h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
      #   )})
      #
      # output$Tabell1 <- function() {
      #   TabellData <- tabellReager()
      #   if (input$enhetsUtvalg == 1) {
      #     Tabell1 <- data.frame("Kategori" = TabellData$GruppeTekst,
      #                           "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
      #                           "Antall totalt" = unname(TabellData$N[,1]),
      #                           "Andel (%)" = unname(TabellData$Andeler[1,]),
      #                           "Antall i kategori" = round(unname(TabellData$Andeler[2,])*unname(TabellData$N[,2])/100),
      #                           "Antall totalt" = unname(TabellData$N[,2]),
      #                           "Andel (%)" = unname(TabellData$Andeler[2,]))
      #     names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
      #     Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
      #       kableExtra::kable_styling("hover", full_width = F) %>%
      #       kableExtra::add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
      #   } else {
      #     Tabell1 <- data.frame("Kategori" = TabellData$GruppeTekst,
      #                           "Antall i kategori" = round(unname(TabellData$Andeler[1,])*unname(TabellData$N[,1])/100),
      #                           "Antall totalt" = unname(TabellData$N[,1]),
      #                           "Andel (%)" = unname(TabellData$Andeler[1,]))
      #     names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
      #     Tabell1 %>%
      #       knitr::kable("html", digits = c(0,0,0,1)) %>%
      #       kableExtra::kable_styling("hover", full_width = F)
      #   }
      # }
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