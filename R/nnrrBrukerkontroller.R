#' Brukerkontroller for bruk i shinyapp
#'
#' Definerer verdier til bruk i brukerkontroller for nnrr sin shinyapp.
#'
#' @param RegData nnrr sin rapporteringsdata
#'
nnrrBrukerkontroller <- function(RegData) {
  # brvalg_fordeling <- c("tverrfaglig_behandlet",
  #                       "PatientAge",
  #                       "FABQ.Score1",
  #                       "FABQ.Score2",
  #                       "HSCL10.Score",
  #                       "PainDurationNow",
  #                       "AarsakSmerte_PasRap",
  #                       "beh_kommunalt",
  #                       "beh_spesialist",
  #                       "pasrapp_beh_klinikk",
  #                       "Eq5dSatisfactionTreatment")
  # names(brvalg_fordeling) <- c("Tverrfaglig behandling",
  #                              "Alder ved konsultasjon",
  #                              "FABQ fysisk aktivitet",
  #                              "FABQ arbeid",
  #                              "Hopkins symptom checklist",
  #                              "Varighet av nåværende smerter",
  #                              "Årsak til smerte, pasientrapportert",
  #                              "Behandling i kommunalhelsetjenesten",
  #                              "Behandling i spesialisthelsetjenesten",
  #                              "Pasientrapportert behandling på nakke- og ryggpoliklinikk",
  #                              "Behandlingstilfredshet EQ5D")

  brvalg_fordeling <- c("Sivilstatus" = "FamilyStatus",
                        "Tverrfaglig behandlet" = "tverrfaglig_behandlet",
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
                        "Alder ved registrering" = "PatientAge",
                        "FABQ fysisk aktivitet" = "FABQScore1",
                        "FABQ arbeid" = "FABQScore2",
                        "Hopkins symptom checklist" = "HSCL10.Score",
                        "Sammenhengende varighet av nåværende smerter" = "PainDurationNow",
                        "Pasientrapportert årsak til smerte" = "AarsakSmerte_PasRap",
                        "Behandling i kommunalhelsetjenesten" = "beh_kommunalt",
                        "Behandling i spesialisthelsetjenesten" = "beh_spesialist",
                        "Pasientrapportert behandling på nakke- og ryggpoliklinikk v2" = "pasrapp_beh_klinikk_v2",
                        "EQ5D tilfredshet med behandling" = "Eq5dSatisfactionTreatment",
                        "Smertevarighet > 2 år" = "smerter_2aar"
  )

  # sykehus <- RegData$UnitId[match(sort(unique(RegData$SykehusNavn)), RegData$SykehusNavn)]
  # names(sykehus) <- sort(unique(RegData$SykehusNavn))

  # BrValg <- list(varvalg = brvalg_fordeling, sykehus = sykehus)
}
