#' Server logic for the nnrr app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  rapbase::appLogger(session = session,
                     msg = "Starting nnrr application")

  # Last data
  RegData <- nnrr::nnrrHentRegData()

  map_avdeling <- data.frame(
    UnitId = unique(RegData$UnitId),
    orgname = RegData$SykehusNavn[match(unique(RegData$UnitId),
                                        RegData$UnitId)])

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "nnrr",
    caller = "nnrr",
    map_orgname = shiny::req(map_avdeling)
  )

  # Legg til SC-spesifikke faner, og fjern dem for andre roller
  tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() == "SC") {
        if (!tabs_added()) {
          shiny::insertTab(
            "nnrr_app_id",
            tab = shiny::tabPanel(
              "Sykehusvisning",
              nnrr::sykehusvisning_UI("sykehusvisning_id"),
              value = "sykehusvisning_id"),
            target = "Fordelinger", position = "after"
          )
          shiny::insertTab(
            "nnrr_app_id",
            tab = shiny::tabPanel(
              "Indikatorer",
              nnrr::indikatorfig_UI("indikatorfig_id"),
              value = "indikatorfig_id"),
            target = "Andeler over tid", position = "after"
          )
          tabs_added(TRUE)
        }
      } else {
        if (tabs_added()) {
          shiny::removeTab("nnrr_app_id", target = "sykehusvisning_id")
          shiny::removeTab("nnrr_app_id", target = "indikator_id")
          tabs_added(FALSE)
        }
      }
    }
  )

  # Legg til verktøy-fanen for SC-brukere, og fjern den for andre roller
  tool_tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(shiny::req(user$role()), {
    if (user$role() == "SC") {
      if (!tool_tabs_added()) {
        shiny::appendTab(
          inputId = "nnrr_app_id",
          tab = shiny::navbarMenu(
            "Verktøy",
            shiny::tabPanel(
              "Utsending",
              shiny::sidebarLayout(
                shiny::sidebarPanel(
                  rapbase::autoReportOrgInput("nnrrDispatch"),
                  rapbase::autoReportInput("nnrrDispatch")
                ),
                shiny::mainPanel(
                  rapbase::autoReportUI("nnrrDispatch")
                )
              )
            ),
            shiny::tabPanel(
              "Metadata",
              shiny::sidebarLayout(
                shiny::sidebarPanel(shiny::uiOutput("metaControl")),
                shiny::mainPanel(shiny::htmlOutput("metaData"))
              )
            ),
            shiny::tabPanel(
              "Eksport",
              shiny::sidebarLayout(
                shiny::sidebarPanel(
                  rapbase::exportUCInput("nnrrExport")
                ),
                shiny::mainPanel(
                  rapbase::exportGuideUI("nnrrExportGuide")
                )
              )
            ),
            shiny::tabPanel(
              "Bruksstatistikk",
              shiny::sidebarLayout(
                shiny::sidebarPanel(rapbase::statsInput("nnrrStats")),
                shiny::mainPanel(
                  rapbase::statsUI("nnrrStats"),
                  rapbase::statsGuideUI("nnrrStatsGuide")
                )
              )
            )
          )
        )
        tool_tabs_added(TRUE)
      }
    } else {
      if (tool_tabs_added()) {
        shiny::removeTab("nnrr_app_id", target = "Verktøy")
        tool_tabs_added(FALSE)
      }
    }
  })



  fordelingsfigServer("fordelingsfig_id", reshID = user$org,
                      RegData = RegData, userRole = user$role,
                      hvd_session = session)

  sykehusvisningServer("sykehusvisning_id",
                       RegData = RegData, userRole = user$role,
                       hvd_session = session)

  tidsvisningServer("tidsvisning_id", reshID = user$org,
                    RegData = RegData, userRole = user$role,
                    hvd_session = session)

  indikatorfigServer("indikatorfig_id",
                     RegData = RegData, userRole = user$role,
                     hvd_session = session)

  datadump_Server("datadump_id", reshID = user$org,
                  RegData = RegData, userRole = user$role,
                  hvd_session = session)

  samledok_server("samledok", reshID = user$org,
                  RegData = RegData, userRole = user$role,
                  hvd_session = session)

  # Administrative tabeller
  # nnrr::admtab_server("admtabell", SkjemaOversikt)
  #

  ##############################################################################
  ################ Subscription, Dispatchment and Stats ########################

  ## Objects currently shared among subscription and dispathcment
  orgs <- as.list(setNames(map_avdeling$UnitId, map_avdeling$orgname))
  org <- rapbase::autoReportOrgServer("nnrrDispatch", orgs)

  subParamNames <- shiny::reactive(c("reshID"))
  subParamValues <- shiny::reactive(user$org())

  ## Subscription

  rapbase::autoReportServer(
    id = "nnrrSubscription",
    registryName = "nnrr",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = list(
      Kvartalsrapport = list(
        synopsis = "NNRR: Kvartalsrapport",
        fun = "strikkRnwAbo",
        paramNames = c("baseName", "reshID"),
        paramValues = c("KvartalsrapportNNRR_rapporteket", 999999)
      )
    ),
    orgs = orgs,
    freq = "quarter",
    user = user,
    runAutoReportButton = TRUE
  )

  ## Dispatchment


  vis_rapp <- reactiveVal(FALSE)
  observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  disParamNames <- shiny::reactive(c("reshID"))
  disParamValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "nnrrDispatch",
    registryName = "nnrr",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = list(
      Kvartalsrapport = list(
        synopsis = "NNRR: Kvartalsrapport",
        fun = "strikkRnwAbo",
        paramNames = c("baseName", "reshID"),
        paramValues = c("KvartalsrapportNNRR_rapporteket", 999999)
      )
    ),
    orgs = orgs,
    eligible = vis_rapp,
    freq = "quarter",
    user = user,
    runAutoReportButton = TRUE
  )

  ## Metadata
  meta <- shiny::reactive({
    rapbase::describeRegistryDb("data")
  })

  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })


  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(lengthMenu=c(25, 50, 100, 200, 400))
  )

  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  ## Stats

  rapbase::statsServer("nnrrStats",
                       registryName = "nnrr",
                       app_id = Sys.getenv("FALK_APP_ID"))
  rapbase::statsGuideServer("nnrrStatsGuide", registryName = "nnrr")


  ##############################################################################
  # Eksport  ###################################################################
  # brukerkontroller
  rapbase::exportUCServer(id = "nnrrExport",
                          dbName = "data",
                          teamName = "nnrr")

  ## veileding
  rapbase::exportGuideServer("nnrrExportGuide", "nnrr")

  ##############################################################################

}
