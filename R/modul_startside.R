#' UI-del av startside for NNRR app på Rapporteket
#'
#' @export
#'
startside_UI <- function(id) {
  ns <- NS(id)
  shiny::bootstrapPage(
    div(
      class = "container",
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading", style = "background-color : #E0E0E0 ",
          h2("Velkommen til Rapporteket for Norsk nakke- og ryggregister", align = "center")
        ),
        div(
          class = "panel-body", style = "background-color:#F0F0F0",
          div(
            class = "panel-text",
            br(),
            h4("Du er nå inne på Rapporteket for NNRR, registerets resultattjeneste.
                Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                       På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                       på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                       Dette medfører at nyere data ikke er kvalitetssikret ennå."),
            h4("Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                       Hvis ikke annet oppgis så gjøres alle datovalg basert på besøksdato. Alle figurer og
                       tabeller kan lastes ned."),
            br(),
            h4(tags$b(tags$u("Innhold i de ulike fanene:"))),

            div(
              class = "container",
              uiOutput(ns("fanebeskrivelse"))
            ),
            br(),
            br(),
            div(
              class = "container",
              fixedRow(
                column(
                  width = 4, offset = 1,
                  h4(
                    "Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:", # helpText
                    a("NNRR",
                      href = "https://www.kvalitetsregistre.no/registers/532/resultater",
                      target = "_blank", align = "center"
                    )
                  )
                ),
                column(
                  width = 4, offset = 2,
                  h4("Mer informasjon om registeret finnes på NNRR sin hjemmeside: ",
                     align = "center",
                     a("NNRR hjemmeside",
                       href = "https://unn.no/fag-og-forskning/medisinske-kvalitetsregistre/norsk-nakke-og-ryggregister",
                       target = "_blank"
                     )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Serverdel av startside for NNRR app på Rapporteket
#'
#' @export
#'
startside <- function(id, userRole) {
  moduleServer(
    id,
    function(input, output, session) {



      output$fanebeskrivelse <- renderUI({

        innhold <- list(

          h4(
            tags$b("Fordelinger "),
            "viser fordelinger (figur/tabell) av ulike variabler.
        Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer."
          )

        )

        if (userRole() == "SC") {

          innhold <- c(
            innhold,
            list(
              h4(
                tags$b("Sykehusvisning "),
                "viser resultater per sykehus.
            Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt,
            andeler eller stablede andeler."
              )
            )
          )

        }

        innhold <- c(
          innhold,
          list(

            h4(
              tags$b("Andeler over tid "),
              "viser tidsutviklingen for valgt variabel for ønsket
              datointervall og tidsenhet."
            )
          )
        )

        if (userRole() == "SC") {

          innhold <- c(
            innhold,
            list(
              h4(
                tags$b("Indikatorer "),
                "viser registerets kvalitetsindikatorer."
              ),

              h4(
                tags$b("Datadump "),
                "gir mulighet til å laste ned din egen avdelings registreringer.
          Man kan velge hvilke variabler man vil inkludere og for hvilket tidsrom
          og hvilke reseksjonsgrupper."
              ),

              h4(
                tags$b("Administrative tabeller "),
                "er en samling oversikter over antall registreringer."
              )
            )
          )

        }

        innhold <- c(
          innhold,
          list(
            h4(
              tags$b("Kvartalsrapport "),
              "lar deg laste ned kvartalsrapport for ønsket kvartal"
            ),

            h4(
              tags$b("Abonnement "),
              "lar brukeren bestille regelmessig utsendelse av
              rapporter til sin registrerte e-post."
            )

          )
        )

        if (userRole() == "SC") {

          innhold <- c(
            innhold,
            list(
              h4(
                tags$b("Verktøy "),
                "gir SC-bruker tilgang til en del nyttige administrative
                verktøy."
              )
            )
          )

        }

        tagList(innhold)

      })
    }
  )
}

