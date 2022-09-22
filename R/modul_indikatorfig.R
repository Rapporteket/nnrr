#' UI-modul for indikatorfigurer i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
indikatorfig_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
                 id = ns("id_indikator_panel"),
                 selectInput(inputId = ns("valgtVar"), label = "Velg indikator",
                             choices = c("Andel tverrfaglig behandlet" = "nnrr_tverrfaglig_behandling",
                                        "Klinisk viktig endring i ODI" = "nnrr_bedret_funksjon",
                                        "Andel med minimal funksjonsnedsettelse" = "nnrr_funksjons_nedsettelse",
                                        "Bedring av smerte i hvile" = "nnrr_bedring_smerte_hvile",
                                        "Bedring av smerte i aktivitet" = "nnrr_bedring_smerte_aktiv",
                                        "Andel tilbake i jobb" = "nnrr_jobb_ny_v1",
                                        "Andel tilbake i jobb el. lavere grad sykemelding" = "nnrr_jobb_ny_v2",
                                        "Pasientopplevd bedring av behandling" = "nnrr_bedring_av_behandling",
                                        "Andel fornøyd med behandling" = "nnrr_misfornoeyd"
                             )
                 ),
                 uiOutput(outputId = ns('tilAar_ui')),
                 # selectInput(inputId = ns("valgtShus"), label = "Fjern sykehus pga. lav dekningsgrad",
                 #             choices = BrValg$sykehus, multiple = TRUE),
                 # sliderInput(ns("skriftStr"), "Skriftstørrelse sykehusnavn", min = 0.5, max = 1.8,
                 #             value = 1.3, step = 0.05, ticks = F),
                 selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                 tags$hr(),
                 actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur"))#,
                  # tabPanel("Tabell", value = "tab",
                  #          uiOutput(ns("utvalg")),
                  #          br(),
                  #          DTOutput(ns("tabell"))
                  # )
      )
    )
  )

}

#' Server-modul for indikatorfigurer i NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
indikatorfigServer <- function(id, RegData, userRole, hvd_session){
  moduleServer(
    id,

    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_indikator_panel")
      })

      # observe(
      #   if (userRole != 'SC') {
      #     shinyjs::hide(id = 'valgtShus')
      #   })


      output$tilAar_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("tilAar"), label = "T.o.m. år",
                    choices = rev((min(RegData$Aar)+2):max(RegData$Aar)))
      })


      tabellReager <- reactive({
        indikatordata <- nnrr::nnrrBeregnIndikator(RegData = RegData,
                                                ind_id = input$valgtVar)
        TabellData <- indikatordata$indikator
        TabellData <- TabellData[which(TabellData$year <= as.numeric(req(input$tilAar))), ]
        indikatordata$indikator <- TabellData
        indikatordata
      })

      output$Figur1 <- renderPlot({
        nnrr::nnrrPlotIndikator(indikatordata = tabellReager(),
                                graaUt=NA, outfile = '',
                                lavDG=NA, inkl_konf=F)
      }, width = 700, height = 700)

    }
  )
}


