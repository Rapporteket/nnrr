#' Tidstrend av rate/andel for en gitt variabel
#'
#' Årlige, kvartalsmessige eller månedlige rater for valgt variabel.
#' Konfidensintervall kan inkluderes hvis ønskelig.
#'
#' Konfidensintervallet er basert på Clopper Pearsons "eksakte" metode for binominalfordelt data.
#'
#' @inheritParams nnrrFigAndeler
#' @param inkl_konf Inkluder konfidensintervall i figur
#'                  0: Nei
#'                  1: Ja
#'                  99: Bruk default som definert i nnrrPrepVar
#' @param tidsenhet Tidsenhet for figur
#'                  'Aar' (Default)
#'                  'Kvartal'
#'                  'Mnd'
#'
#' @return En figur med tidsutvikling av andel over år
#'
#' @export
#'
nnrrFigAndelTid <- function(RegData, valgtVar, datoFra='2014-01-01', datoTil='2050-12-31', enhetsUtvalg=1,
                               minald=0, maxald=130, erMann=99, outfile='', reshID)
{

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

















}
