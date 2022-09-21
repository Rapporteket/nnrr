#' Beregn indikatorer
#'
#' Inneholder logikken som definerer indikatorene til registeret
#'
#' @param RegData En dataramme med registerdata
#'
#' @export
#'
nnrrBeregnIndikator <- function(RegData, ind_id) {

  map_resh_orgnr <- data.frame(orgnr_sh = c(974589095, 974749025,
                                            974557746, 974795787,
                                            974795639, 974703300,
                                            974795477, 974733013,
                                            974754118, 974747138,
                                            974631326, 974575396),
                               resh = c(109834, 104293, 102959,
                                        601032, 700735, 4211588,
                                        102169, 114174, 105821,
                                        4212982, 103736, 700138))
  terskel=30
  minstekrav = NA
  maal = NA
  skriftStr=1.3
  pktStr=1.4
  legPlass="top"
  minstekravTxt="Min."
  maalTxt="Mål"
  decreasing=F
  width=800
  height=700
  maalretn="hoy"

  if (ind_id == "nnrr_tverrfaglig_behandling") {
    indikator <- RegData[RegData$regstatus==1, ]

    indikator$tverrfaglig_behandlet <- 0
    indikator$tverrfaglig_behandlet[indikator$Treatment_GroupInterdisciplinary2018 != 0 |
                                      indikator$Treatment_GroupInterdisciplinary != 0] <- 1
    indikator$tverrfaglig_behandlet[indikator$Treatment_InvidualInterdisciplinary != 0] <- 1

    indikator <- indikator[, c("UnitId", "Aar", "tverrfaglig_behandlet", "SykehusNavn")]
    names(indikator) <- c('ReshId', 'year', 'var', "SykehusNavn")
    indikator$denominator <- 1
    indikator$orgnr <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
    indikator$ind_id <- "nnrr_tverrfaglig_behandling"
    indikator$context <- "caregiver"
    tittel="Andel tverrfaglig behandlet"
    maal = 30
  }

  if (ind_id == "nnrr_bedret_funksjon") {
    indikator <- RegData[which(RegData$regstatus==1 &
                                 RegData$regstatus_post==1), ]
    indikator <- indikator[!is.na(indikator$OdiScore) &
                             !is.na(indikator$OdiScore_post), ]

    indikator$odidiff_klin_viktig <- 0
    indikator$odidiff_klin_viktig[(indikator$OdiScore - indikator$OdiScore_post)/
                                    indikator$OdiScore >= .3] <- 1
    indikator <- indikator[, c("UnitId", "aar_oppfolg",
                               "odidiff_klin_viktig", "SykehusNavn")]
    names(indikator) <- c('ReshId', 'year', 'var', "SykehusNavn")
    indikator$denominator <- 1
    indikator$orgnr <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
    indikator$ind_id <- "nnrr_bedret_funksjon"
    indikator$context <- "caregiver"
    tittel="Klinisk viktig endring i ODI"
    maal = 30
    minstekrav = 25
  }

  if (ind_id == "nnrr_funksjons_nedsettelse") {
    indikator <- RegData[which(RegData$regstatus==1 &
                                 RegData$regstatus_post==1), ]
    indikator <- indikator[!is.na(indikator$OdiScore) &
                             !is.na(indikator$OdiScore_post), ]

    indikator$minimal_funksjonsnedsettelse <- 0
    indikator$minimal_funksjonsnedsettelse[indikator$OdiScore_post <= 23] <- 1
    indikator <- indikator[, c("UnitId", "aar_oppfolg",
                               "minimal_funksjonsnedsettelse", "SykehusNavn")]
    names(indikator) <- c('ReshId', 'year', 'var', "SykehusNavn")
    indikator$denominator <- 1
    indikator$orgnr <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
    indikator$ind_id <- "nnrr_funksjons_nedsettelse"
    indikator$context <- "caregiver"
    tittel="Andel med minimal funksjonsnedsettelse"
    maal = 30
    minstekrav = 25

  }



  indikatordata <- list(indikator=indikator, tittel=tittel, terskel=terskel,
                        minstekrav=minstekrav, maal=maal, skriftStr=skriftStr,
                        pktStr=pktStr, legPlass=legPlass, minstekravTxt=minstekravTxt,
                        maalTxt=maalTxt, decreasing=decreasing,
                        width=width, height=height, maalretn=maalretn)

}
