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
  RegData <- rapbase::loadStagingData("nnrr", "RegData") #Benyttes i appen
  if (isFALSE(RegData)) {
    RegData <- nnrr::nnrrHentRegData()
    rapbase::saveStagingData("nnrr", "RegData", RegData)
  }

  registryName <- "nnrr"

  userFullName <- rapbase::getUserFullName(session)
  userRole <- rapbase::getUserRole(session)
  userReshId <- rapbase::getUserReshId(session)
  hospitalName <- RegData$SykehusNavn[match(userReshId, RegData$UnitId)]

  # rapbase::navbarWidgetServer("nnrrNavbarWidget", "nnrr",
  #                             caller = "nnrr")

  fordelingsfigServer("fordelingsfig_id", reshID = userReshId,
                      RegData = RegData, userRole = userRole, hvd_session = session)

  sykehusvisningServer("sykehusvisning_id",
                      RegData = RegData, userRole = userRole, hvd_session = session)

  tidsvisningServer("tidsvisning_id", reshID = userReshId,
                    RegData = RegData, userRole = userRole, hvd_session = session)

  indikatorfigServer("indikatorfig_id",
                     RegData = RegData, userRole = userRole, hvd_session = session)

  datadump_Server("datadump_id", reshID = userReshId,
                  RegData = RegData, userRole = userRole, hvd_session = session)

  samledok_server("samledok", reshID = userReshId,
                  RegData = RegData, userRole = userRole, hvd_session = session)

  # Administrative tabeller
  # nnrr::admtab_server("admtabell", SkjemaOversikt)
  #
  # #forløpstype brukerkontroll
  # output$forlopstype_ui <- shiny::renderUI({
  #
  #   forlopstyper <- sort(unique(as.numeric(ForlopsOversikt$ForlopsType1Num)))
  #   names(forlopstyper) <- ForlopsOversikt$ForlopsType1[match(forlopstyper, ForlopsOversikt$ForlopsType1Num)]
  #   names(forlopstyper)[forlopstyper==0] <- "Ingen"
  #   selectInput(inputId = "forlopstype", label = "Velg forløpstype(r)",
  #               choices = forlopstyper, multiple = T, selected = c(3, 5, 7, 4, 6, 8))
  # })
  #
  # #Registeringer per enhet----
  # output$tabell_id <- shiny::renderTable({
  #   data <- ForlopsOversikt %>%
  #     dplyr::filter(HovedDato >= input$dato_id[1] & HovedDato <= input$dato_id[2]) %>%
  #     dplyr::filter(ForlopsType1Num %in% input$forlopstype) %>%
  #     dplyr::filter(BasisRegStatus %in% input$regstatus) %>%
  #     dplyr::select("SykehusNavn", "ForlopsType1") %>%
  #     table() %>%
  #     addmargins() %>%
  #     as.data.frame.matrix() %>%
  #     tidyr::as_tibble(rownames = "Enhet")
  # }, digits = 0)

  # Eksempelrapport
  # output$exReport <- shiny::renderUI({
  #   rapbase::renderRmd(
  #     system.file("eksSamlerapport.Rmd", package = "nnrr"),
  #     outputType = "html_fragment",
  #     params = list(
  #       author = userFullName,
  #       hospitalName = hospitalName,
  #       tableFormat = "html",
  #       reshId = userReshId,
  #       registryName = registryName,
  #       userRole = userRole
  #     )
  #   )
  # })
  #
  # output$downloadReport <- shiny::downloadHandler(
  #   filename = function() {
  #     basename(tempfile(pattern = "nnrr_eksRapport",
  #                       fileext = paste0(".", input$formatReport)))
  #   },
  #   content = function(file) {
  #     fn <- rapbase::renderRmd(
  #       system.file("eksSamlerapport.Rmd", package = "nnrr"),
  #       outputType = input$formatReport,
  #       params = list(
  #         author = userFullName,
  #         hospitalName = hospitalName,
  #         tableFormat = input$formatReport,
  #         reshId = userReshId,
  #         registryName = registryName,
  #         userFullName = userFullName,
  #         userRole = userRole
  #       )
  #     )
  #     file.rename(fn, file)
  #   }
  # )



  # dummy report and orgs to subscribe and dispatch
  orgs <- list(
    TestOrg = 999999
  )
  report <- list(
    Veiledning = list(
      synopsis = "Testrapport kun for illustrasjon",
      fun = "reportProcessor",
      paramNames = c("report", "outputFormat", "title"),
      paramValues = c("veiledning", "pdf", "Testrapport")
    )#,
    # Eksempelrapport = list(
    #   synopsis = "Eksempelrapport med data fra nnrr",
    #   fun = "reportProcessor",
    #   paramNames = c("report", "outputFormat", "title"),
    #   paramValues = c("eksSamlerapport", "pdf", "Eksempelrapport")
    # )
  )

  # subscribe
  rapbase::autoReportServer(
    "nnrrSubscription", registryName = registryName, type = "subscription",
    reports = report, orgs = orgs
  )

  # dispatch
  org <- rapbase::autoReportOrgServer("nnrrDispatchOrg", orgs)
  fileFormat <- rapbase::autoReportFormatServer("nnrrDispatchFormat")
  paramNames <- shiny::reactive(c("outputFormat"))
  paramValues <- shiny::reactive(c(fileFormat()))
  rapbase::autoReportServer(
    "nnrrDispatch", registryName = registryName, type = "dispatchment",
    org = org$value,
    paramNames = paramNames, paramValues = paramValues, reports = report,
    orgs = orgs
  )

  # use stats
  rapbase::statsServer("nnrrStats", registryName = registryName)

  # export
  # rapbase::exportGuideServer("nnrrExport", registryName)
  # rapbase::exportUCServer("nnrrExport", registryName)
}
