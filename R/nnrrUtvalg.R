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
nnrrUtvalg <- function(RegData,
                       datoFra = "2000-01-01",
                       datoTil = "2100-01-01",
                       minald = 0,
                       maxald = 120,
                       erMann = 99,
                       datovar = "Besoksdato",
                       tverrfaglig = 99,
                       minHSCL = 1,
                       maxHSCL = 4,
                       medikamenter = NA,
                       smerte = NA,
                       tolk = 99,
                       fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  RegData$Dato <- RegData[, datovar]
  datotxt <- switch(datovar,
                    "Besoksdato"="Besoksdato: ",
                    "dato_oppfolg"="Oppfølgingsdato: ")

  Ninn <- dim(RegData)[1]
  indAlle <- 1:Ninn

  indAld <- if (minald > 0 | maxald < 120) {
    which(RegData$PatientAge >= minald & RegData$PatientAge <= maxald)
  } else indAlle
  indDato <- which(RegData$Dato >= datoFra & RegData$Dato <= datoTil)
  indKj <- if (erMann %in% 0:1) {
    which(RegData$ErMann == erMann)
  } else indAlle
  indTverr <- if (tverrfaglig %in% 0:1) {
    which(RegData$Tverrfaglig_vurdering == tverrfaglig)
  } else indAlle
  indHSCL <- if (minHSCL > 1 | maxHSCL < 4) {
    which(RegData$HSCL10.Score >= minHSCL & RegData$HSCL10.Score <= maxHSCL)
  } else indAlle
  indMedikament <- if (length(medikamenter %i% c("MedicationA", "MedicationB", "MedicationC"))>=1) {
    # which(rowSums(RegData[, medikamenter]) > 0)
    which(RegData %>% select(medikamenter) %>% rowSums() > 0)
  } else indAlle
  indSmerte <- if (smerte[1] %in% c(1,2,3,4,5,99)) {
    which(RegData$SmerteNum %in% smerte)
  } else indAlle
  indTolk <- if (tolk %in% c(0,1)) {
    which(RegData$Interpreter %in% tolk)
  } else indAlle



  indMed <- indAld %i% indDato %i% indKj %i% indTverr %i% indHSCL %i%
    indMedikament %i% indSmerte %i% indTolk
  RegData <- RegData[indMed,]

  utvalgTxt <- c(paste0(datotxt,
                        min(RegData$Dato, na.rm=T), ' til ',
                        max(RegData$Dato, na.rm=T)),
                 if ((minald>0) | (maxald<120)) {
                   paste0('Pasienter fra ', min(RegData$PatientAge, na.rm=T), ' til ',
                          max(RegData$PatientAge, na.rm=T), ' år')},
                 if (erMann %in% 0:1) {paste0('Kjønn: ',
                                              c('Kvinner', 'Menn')[erMann+1])},
                 if (tverrfaglig %in% 0:1) {paste0("Tverrfaglig behandlet: ",
                                                   c("Nei", "Ja")[tverrfaglig+1])},
                 if (minHSCL > 1 | maxHSCL < 4) {paste0("HSCL fra ", minHSCL,
                                                        " til ", maxHSCL)},
                 if (length(medikamenter %i% c("MedicationA", "MedicationB", "MedicationC"))>=1) {
                   paste0("Medikamenter: ", paste(medikamenter, collapse = ", "))
                 },
                 if (smerte[1] %in% c(1,2,3,4,5,99)) {
                   paste0("Varighet av nåværende smerter: ",
                          paste(RegData$PainDurationNow[match(smerte, RegData$SmerteNum)], collapse = ", " ))
                 },
                 if (tolk %in% c(0,1)) {paste0("Tolk: ", c("Nei", "Ja")[tolk+1])}
  )


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
  return(invisible(UtData))
}
