#' Server logic for the nnrr app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  rapbase::appLogger(session = session, msg = "Starting nnrr application")

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

  fordelingsfigServer("fordelingsfig_id", reshID = user$org,
                      RegData = RegData, userRole = user$role, hvd_session = session)

  sykehusvisningServer("sykehusvisning_id",
                      RegData = RegData, userRole = user$role, hvd_session = session)

  tidsvisningServer("tidsvisning_id", reshID = user$org,
                    RegData = RegData, userRole = user$role, hvd_session = session)

  indikatorfigServer("indikatorfig_id",
                     RegData = RegData, userRole = user$role, hvd_session = session)

  datadump_Server("datadump_id", reshID = user$org,
                  RegData = RegData, userRole = user$role, hvd_session = session)

  samledok_server("samledok", reshID = user$org,
                  RegData = RegData, userRole = user$role, hvd_session = session)

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
  observe(
    rapbase::statsServer("nnrrStats",
                         registryName = "nnrr",
                         app_id = Sys.getenv("FALK_APP_ID"),
                         eligible = (user$role() == "SC"))
  )
  rapbase::statsGuideServer("nnrrStatsGuide", registryName = "nnrr")


  ##############################################################################
  # Eksport  ###################################################################
  # brukerkontroller
  rapbase::exportUCServer("nnrrExport", "nnrr")

  ## veileding
  rapbase::exportGuideServer("nnrrExportGuide", "nnrr")

  ##############################################################################

}
