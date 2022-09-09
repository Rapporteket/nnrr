library(nnrr)
library(kableExtra)
library(DT)
library(shiny)
library(shinyjs)
# library(shinyBS)
library(shinyalert)
library(tidyverse)
library(lubridate)


addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "NNRR"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))

RegData <- nnrr::nnrrHentRegData()

brvalg_fordeling <- c("tverrfaglig_behandlet", "PatientAge", "FABQ.Score1", "FABQ.Score2", "HSCL10.Score", "PainDurationNow",
                      "AarsakSmerte_PasRap", "beh_kommunalt", "beh_spesialist", "pasrapp_beh_klinikk", "Eq5dSatisfactionTreatment")
names(brvalg_fordeling) <- c("Tverrfaglig behandling", "Alder ved konsultasjon", "FABQ fysisk aktivitet", "FABQ arbeid",
                             "Hopkins symptom checklist", "Varighet av nåværende smerter", "Årsak til smerte, pasientrapportert",
                             "Behandling i kommunalhelsetjenesten", "Behandling i spesialisthelsetjenesten",
                             "Pasientrapportert behandling på nakke- og ryggpoliklinikk", "Behandlingstilfredshet EQ5D")
sykehus <- RegData$UnitId[match(sort(unique(RegData$SykehusNavn)), RegData$SykehusNavn)]
names(sykehus) <- sort(unique(RegData$SykehusNavn))

BrValg <- list(varvalg = brvalg_fordeling, sykehus = sykehus)

source(system.file("shinyApps/nnrr/R/modul_startside.R", package = "nnrr"), encoding = 'UTF-8')
source(system.file("shinyApps/nnrr/R/modul_fordelingsfig.R", package = "nnrr"), encoding = 'UTF-8')

# Define UI for application
ui <- navbarPage(id = "norgast_app_id",

                 title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                             regTitle),
                 windowTitle = regTitle,
                 theme = "rap/bootstrap.css",

                 shiny::tabPanel("Startside",
                                 shinyjs::useShinyjs(),
                                 shinyalert::useShinyalert(),
                                 rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                              organization = uiOutput("appOrgName"),
                                                              addUserInfo = TRUE),
                                 tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                                 startside_UI("startside")
                 ),
                 shiny::tabPanel("Fordelinger",
                          fordelingsfig_UI(id = "fordelingsfig_id", BrValg = BrValg)
                 )
)


server <- function(input, output, session) {

  if (rapbase::isRapContext()) {
    raplog::appLogger(session = session, msg = 'Starter NNRR')
    reshID <- rapbase::getUserReshId(session)
    userRole <- rapbase::getUserRole(session)
  } else {
    reshID <- 601032
    userRole <- 'SC'
  }

  # if (userRole != 'SC') {
  #   shiny::hideTab("norgast_app_id", target = "Sykehusvisning")
  #   shinyjs::hide(id = 'valgtShus')
  #   shinyjs::hide(id = 'file1')
  # }

  shiny::callModule(startside, "startside", usrRole=userRole)

  #################################################################################################################################
  ################ Fordelingsfigurer ##############################################################################################

  shiny::callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)



  #Navbarwidget
  output$appUserName <- renderText(rapbase::getUserFullName(session))
  output$appOrgName <- renderText(rapbase::getUserReshId(session))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })




}


# Run the application
shinyApp(ui = ui, server = server)








