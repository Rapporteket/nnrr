#' Trekk ut icd-10 kode fra fritekst, og rydd i navn
#'
#' Denne funksjonen forsøker å trekke ut relevante icd-10 koder fra en vektor som også inneholder fritekst
#'
#' Her kan detaljer skrives
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return diagnoser_vasket En vektor av vaskede diagnoser
#'
#' @export
#'
vask_diagnoser <- function(diagnoser_raa){

  diagnoser_vasket <- diagnoser_raa
  # Kun store bokstaver
  diagnoser_vasket <- toupper(diagnoser_vasket)
  # Fjern alle mellomrom
  diagnoser_vasket <- gsub(' ', '', diagnoser_vasket)
  # sett inn M der det mangler ledende bokstav
  indManglerM <- which(!is.na(as.numeric(substr(diagnoser_vasket, 1, 1))))
  diagnoser_vasket[indManglerM] <- paste0('M', diagnoser_vasket[indManglerM])
  # Erstatt 'I' og 'L' med 1 og O med 0.
  diagnoser_vasket <- gsub('I', '1', diagnoser_vasket)
  diagnoser_vasket <- gsub('L', '1', diagnoser_vasket)
  diagnoser_vasket <- gsub('O', '0', diagnoser_vasket)
  # Fjern punktum
  diagnoser_vasket <- gsub('\\.', '', diagnoser_vasket)

  # Finn første sammenhengende tallet
  tmp <- strsplit(diagnoser_vasket, "[^0-9]+")
  tmp <- as.numeric(unlist(lapply(tmp, '[', 2)))
  # Bruk første bokstav og første sammenhengende tall som kode
  diagnoser_vasket <- paste0(substr(diagnoser_vasket, 1, 1), tmp)
  # Hvis tallet er mindre enn 10 eller ikke finnes, erstatt med opprinnelig tekst
  diagnoser_vasket[tmp < 10 | is.na(tmp)] <- diagnoser_raa[tmp < 9 | is.na(tmp)]

  return(diagnoser_vasket)
}
