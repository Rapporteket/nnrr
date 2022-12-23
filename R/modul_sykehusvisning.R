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
        TabellData <- nnrr::nnrrBeregnGjsnPrePostGrVar(
          RegData = RegData,
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

      output$utvalg <- renderUI({
        TabellData <- tabellReager()
        tagList(
          h3(HTML(paste0(TabellData$tittel, '<br />'))),
          h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
        )})

      output$Tabell1 <- function() {
        TabellData <- tabellReager()
        if (as.numeric(input$sammenlign) == 0) {
          Tabell1 <-
            dplyr::tibble(
              Sykehus = TabellData$grtxt, gj.sn = as.numeric(TabellData$PlotMatrise),
              KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed)), '-',
                          sprintf("%.1f", as.numeric(TabellData$KIOpp))),
              N = TabellData$Ngr) %>%
            knitr::kable("html", digits = c(0,1,0,0)) %>%
            kableExtra::kable_styling("hover", full_width = F)
        } else {
          if (as.numeric(input$sammenlign) == 1) {
            Tabell1 <-
              dplyr::tibble(
                Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[1,])), '-',
                            sprintf("%.1f", as.numeric(TabellData$KIOpp[1,]))),
                gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[2,])), '-',
                            sprintf("%.1f", as.numeric(TabellData$KIOpp[2,]))),
                N = TabellData$Ngr, .name_repair = "minimal") %>%
              knitr::kable("html", digits = c(0,1,0,1,0,0)) %>%
              kableExtra::kable_styling("hover", full_width = F) %>%
              kableExtra::add_header_above(c(" ", "Før intervensjon" = 2, "6 mnd." = 2, " "))
          } else {
            if (as.numeric(input$sammenlign) == 2) {
              Tabell1 <-
                dplyr::tibble(
                  Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                  KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[1,])), '-',
                              sprintf("%.1f", as.numeric(TabellData$KIOpp[1,]))),
                  gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                  KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[2,])), '-',
                              sprintf("%.1f", as.numeric(TabellData$KIOpp[2,]))),
                  gj.sn. = as.numeric(TabellData$PlotMatrise[3,]),
                  KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[3,])), '-',
                              sprintf("%.1f", as.numeric(TabellData$KIOpp[3,]))),
                  N = TabellData$Ngr, .name_repair = "minimal") %>%
                knitr::kable("html", digits = c(0,1,0,1,0,1,0,0)) %>%
                kableExtra::kable_styling("hover", full_width = F) %>%
                kableExtra::add_header_above(c(" ", "Før intervensjon" = 2,
                                               "6 mnd." = 2, "12 mnd." = 2, " "))
            }
          }
        }

      }

      output$lastNed <- downloadHandler(
        filename = function(){
          fs::path_sanitize(paste0(input$valgtVar, Sys.time(), '.csv'))
        },

        content = function(file){
          TabellData <- tabellReager()
          if (as.numeric(input$sammenlign) == 0) {
            Tabell1 <-
              dplyr::tibble(
                Sykehus = TabellData$grtxt, gj.sn = as.numeric(TabellData$PlotMatrise),
                KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed)), '-',
                            sprintf("%.1f", as.numeric(TabellData$KIOpp))),
                N = TabellData$Ngr)
          } else {
            if (as.numeric(input$sammenlign) == 1) {
              Tabell1 <-
                dplyr::tibble(
                  Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                  KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[1,])), '-',
                              sprintf("%.1f", as.numeric(TabellData$KIOpp[1,]))),
                  gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                  KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[2,])), '-',
                              sprintf("%.1f", as.numeric(TabellData$KIOpp[2,]))),
                  N = TabellData$Ngr, .name_repair = "minimal")
            } else {
              if (as.numeric(input$sammenlign) == 2) {
                Tabell1 <-
                  dplyr::tibble(
                    Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                    KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[1,])), '-',
                                sprintf("%.1f", as.numeric(TabellData$KIOpp[1,]))),
                    gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                    KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[2,])), '-',
                                sprintf("%.1f", as.numeric(TabellData$KIOpp[2,]))),
                    gj.sn. = as.numeric(TabellData$PlotMatrise[3,]),
                    KI = paste0(sprintf("%.1f", as.numeric(TabellData$KINed[3,])), '-',
                                sprintf("%.1f", as.numeric(TabellData$KIOpp[3,]))),
                    N = TabellData$Ngr, .name_repair = "minimal")
              }
            }
          }
          write.csv2(Tabell1, file, row.names = F, fileEncoding = 'latin1')
        }
      )

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          fs::path_sanitize(paste0(input$valgtVar, Sys.time(), '.', input$bildeformat))
        },

        content = function(file){
          nnrr::nnrrPlotGjsnPrePostGrVar(plotparams = tabellReager(),
                                         outfile=file)
        }
      )

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
