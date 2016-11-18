#' Gjør utvalg av dataen
#'
#' Denne funksjonen gjør utvalg av dataen basert på brukervalg
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return Et filtrert datasett, samt en tekst som angir utvalget som er gjort
#'
#' @export
#'
nnrrUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]

  indAld <- which(RegData$PatientAge >= minald & RegData$PatientAge <= maxald)
  indDato <- which(RegData$Besoksdato >= as.POSIXlt(datoFra) & RegData$Besoksdato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indMed <- indAld %i% indDato %i% indKj
  RegData <- RegData[indMed,]

  utvalgTxt <- c(paste('Besøksdato: ',
                       min(RegData$Besoksdato, na.rm=T), ' til ', max(RegData$Besoksdato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$PatientAge, na.rm=T), ' til ', max(RegData$PatientAge, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')}
  )


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
  return(invisible(UtData))
}
