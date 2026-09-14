#' UI-modul for Administrative tabeller-fane i NNRR sin
#' shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Administrative tabeller
#'
#' @export
#'
admtab_ui <- function(id) {
  ns <- shiny::NS(id)

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        id = ns("id_adm_panel"),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'id_ant_skjema'",
            ns("admtabeller")
          ),
          dateRangeInput(
            inputId = ns("datovalg_adm"),
            label = "Dato fra og til",
            min = "2014-01-01", language = "nb",
            max = Sys.Date(),
            start = lubridate::floor_date(
              lubridate::today() -
                lubridate::years(2),
              unit = "year"
            ),
            end = Sys.Date(), separator = " til "
          ),
          tags$hr(),
          actionButton(ns("reset_input"), "Nullstill valg")
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'id_ant_tid'",
            ns("admtabeller")
          ),
          selectInput(
            inputId = ns("adm_tidsenhet"),
            label = "Velg tidsenhet",
            choices = c("Måneder" = 1, "År" = 2),
            selected = 1
          ),
          uiOutput(ns("tid_valg_ui")),
          selectInput(
            inputId = ns("regstatus_tid"),
            label = "Skjemastatus",
            choices = c(
              "Ferdig basisreg." = 1,
              "Ferdig 6-mnd." = 2,
              "Ferdig 12-mnd." = 3,
              "Komplette forløp" = 4
            )
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          id = ns("admtabeller"),
          tabPanel(
            "Antall skjema",
            value = "id_ant_skjema",
            h4("Alle datofiltreringer, også for oppfølginger,
               gjøres på dato for konsultasjon"),
            br(),
            br(),
            DT::DTOutput(ns("Tabell_adm1")),
            downloadButton(ns("lastNed_adm1"), "Last ned tabell")
          ),
          tabPanel(
            "Registreringer over tid",
            value = "id_ant_tid",
            h4("Alle datofiltreringer, også for oppfølginger,
               gjøres på dato for konsultasjon"),
            DT::DTOutput(ns("Tabell_adm2")),
            downloadButton(ns("lastNed_adm2"), "Last ned tabell")
          )
        )
      )
    )
  )
}

#' Serverdel av modul for Administrative tabeller-fane i
#' NNRR sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Administrative tabeller
#'
#' @export
#'
admtab_server <- function(id, RegData, userRole,
                          hvd_session) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$reset_input, {
        shinyjs::reset("id_adm_panel")
      })

      # output$sidebar_controls <- renderUI({
      #   ns <- session$ns
      #
      #   switch(
      #     input$admtabeller,
      #     "id_ant_skjema" = tagList(
      #
      #       dateRangeInput(
      #         inputId=ns("datovalg_adm"),
      #         label = "Dato fra og til",
      #         min = '2014-01-01', language = "nb",
      #         max = Sys.Date(),
      #         start  = lubridate::floor_date(lubridate::today() -
      #                                          lubridate::years(2),
      #                                        unit = "year"),
      #         end = Sys.Date(), separator = " til "),
      #       tags$hr(),
      #       actionButton(ns("reset_input"), "Nullstill valg")
      #     ),
      #
      #   "id_ant_tid" =
      #   tagList(
      #
      #     selectInput(
      #       inputId = ns("adm_tidsenhet"),
      #       label = "Velg tidsenhet",
      #       choices = c("Måneder" = 1, "År" = 2),
      #       selected = 1
      #     ),
      #
      #     uiOutput(ns("tid_valg_ui")),
      #
      #     selectInput(
      #       inputId = ns("regstatus_tid"),
      #       label = "Skjemastatus",
      #       choices = c('Ferdig basisreg.' = 1,
      #                   'Ferdig 6-mnd.' = 2,
      #                   'Ferdig 12-mnd.' = 3,
      #                   'Komplette forløp' = 4)
      #     )
      #   )
      #   )
      # }
      # )

      output$tid_valg_ui <- renderUI({
        ns <- session$ns
        req(input$adm_tidsenhet)

        if (input$adm_tidsenhet == "1") {
          tagList(
            shinyWidgets::airDatepickerInput(
              inputId = ns("datovalg_adm_tid_mnd"),
              label = "Vis til og med måned:",
              minDate = "2014-01-01",
              maxDate = Sys.Date(),
              value = Sys.Date(),
              view = "months",
              minView = "months",
              dateFormat = "MM yyyy",
              language = "da"
            ),
            sliderInput(
              inputId = ns("ant_mnd"),
              label = "Antall måneder",
              min = 1,
              max = 24,
              value = 12
            )
          )
        } else {
          tagList(
            shinyWidgets::airDatepickerInput(
              inputId = ns("datovalg_adm_tid_aar"),
              label = "Vis til og med år:",
              minDate = "2014-01-01",
              maxDate = Sys.Date(),
              value = Sys.Date(),
              view = "years",
              minView = "years",
              dateFormat = "yyyy",
              language = "da"
            ),
            sliderInput(
              inputId = ns("ant_aar"),
              label = "Antall år",
              min = 1,
              max = 10,
              value = 5
            )
          )
        }
      })

      antskjema <- function() {
        ant_skjema <- RegData |>
          dplyr::filter(
            S1b_DateOfCompletion >= shiny::req(input$datovalg_adm[1]),
            S1b_DateOfCompletion <= shiny::req(input$datovalg_adm[2])
          ) |>
          dplyr::summarise(
            "Basisreg." = sum(regstatus),
            "6-mnd. oppf." = sum(regstatus_post),
            "12-mnd. oppf." = sum(regstatus_post2),
            "Komplette forløp" = sum(regstatus_post & regstatus_post2),
            .by = SykehusNavn
          ) |>
          janitor::adorn_totals()

        sketch <- htmltools::withTags(table(
          DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
          DT::tableFooter(c("Sum", as.numeric(
            ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]]
          )))
        ))
        list(ant_skjema = ant_skjema, sketch = sketch)
      }

      output$Tabell_adm1 <- DT::renderDT(
        DT::datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
          container = antskjema()$sketch,
          rownames = F,
          options = list(pageLength = 40)
        )
      )

      output$lastNed_adm1 <- downloadHandler(
        filename = function() {
          paste0("Regoversikt", Sys.time(), ".csv")
        },
        content = function(file) {
          TabellData <- antskjema()$ant_skjema
          write.csv2(TabellData, file,
            row.names = F,
            fileEncoding = "Latin1"
          )
        }
      )

      andre_adm_tab <- function() {
        if (input$adm_tidsenhet == 1) {
          req(input$datovalg_adm_tid_mnd)
          tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
          fraDato <- lubridate::`%m-%`(
            tilDato,
            months(as.numeric(input$ant_mnd) - 1)
          ) %>%
            lubridate::floor_date(unit = "months")

          ant_skjema <- RegData |>
            dplyr::mutate(
              tid = factor(
                format(
                  S1b_DateOfCompletion,
                  format = "%b-%y"
                ),
                levels = format(seq(fraDato, tilDato,
                  by = "month"
                ), "%b-%y")
              )
            ) |>
            dplyr::filter(!is.na(tid)) |>
            dplyr::filter(
              switch(shiny::req(input$regstatus_tid),
                "1" = regstatus == 1,
                "2" = regstatus_post == 1,
                "3" = regstatus_post2 == 1,
                "4" = regstatus_post == 1 & regstatus_post2 == 1
              )
            ) |>
            dplyr::count(SykehusNavn, tid, .drop = FALSE) |>
            tidyr::pivot_wider(names_from = tid, values_from = n) |>
            janitor::adorn_totals(where = c("row", "col"))
        }

        if (input$adm_tidsenhet == 2) {
          req(input$datovalg_adm_tid_aar)
          tilDato <- as.Date(input$datovalg_adm_tid_aar)
          fraDato <- lubridate::`%m-%`(
            tilDato,
            lubridate::years(input$ant_aar - 1)
          ) %>%
            lubridate::floor_date(unit = "years")

          ant_skjema <- RegData |>
            dplyr::mutate(
              tid = factor(
                format(S1b_DateOfCompletion, format = "%Y"),
                levels = format(seq(as.Date(fraDato),
                  as.Date(input$datovalg_adm_tid_aar),
                  by = "year"
                ), "%Y")
              )
            ) |>
            dplyr::filter(!is.na(tid)) |>
            dplyr::filter(
              switch(shiny::req(input$regstatus_tid),
                "1" = regstatus == 1,
                "2" = regstatus_post == 1,
                "3" = regstatus_post2 == 1,
                "4" = regstatus_post == 1 & regstatus_post2 == 1
              )
            ) |>
            dplyr::count(SykehusNavn, tid, .drop = FALSE) |>
            tidyr::pivot_wider(names_from = tid, values_from = n) |>
            janitor::adorn_totals(where = c("row", "col"))
        }

        sketch <- htmltools::withTags(table(
          DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
          DT::tableFooter(
            c("Sum", as.numeric(ant_skjema[
              dim(ant_skjema)[1],
              2:dim(ant_skjema)[2]
            ]))
          )
        ))
        list(ant_skjema = ant_skjema, sketch = sketch)
      }

      output$Tabell_adm2 <- DT::renderDT(
        DT::datatable(
          andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
          container = andre_adm_tab()$sketch,
          rownames = F,
          options = list(pageLength = 40)
        )
      )

      output$lastNed_adm2 <- downloadHandler(
        filename = function() {
          paste0("Regoversikt_tid", Sys.time(), ".csv")
        },
        content = function(file) {
          TabellData <- andre_adm_tab()$ant_skjema
          write.csv3(TabellData, file, row.names = F)
        }
      )
    }
  )
}
