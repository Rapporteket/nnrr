#' Fremstill tabell med indikator over år og fordelt på sykehus
#'
#' Tar som input en dataramme som minimum inneholder kolonnene Aar, SykehusNavn samt
#' en indikatorkolonne med navn man angir i funksjonskallet
#'
#' @param RegData Dataramme
#' @param indikator Navn på indikatorkolonne. Kolonne består av 0 og 1.
#'
#' @return Tabell
#'
#' @export
#'

nnrrIndikatorTabell <- function(RegData, indikator='Indikator') {

  RegData$Indikator <- RegData[, indikator]
  tabell <- RegData %>% group_by(Aar, SykehusNavn) %>% summarise(antall = sum(Indikator==1), N=n())
  tmp <- tabell %>% group_by(Aar) %>% summarise(antall = sum(antall), N = sum(N))
  tmp$SykehusNavn <- 'Zmp'
  tmp <- tmp[, c(1,4,2,3)]
  tabell <- bind_rows(tabell, tmp)
  tabell <- tabell[order(tabell$Aar, decreasing = F), ]
  tabell$SykehusNavn[tabell$SykehusNavn == 'Zmp'] <- 'Totalt'

  tabell$andel <- tabell$antall/tabell$N*100
  tabell$Aar[-match(sort(unique(tabell$Aar), decreasing=F), tabell$Aar)] <- NA
  names(tabell) <- c('År', 'Avdeling', 'Antall', 'N', 'Andel (%)')
  return(invisible(tabell))

}
