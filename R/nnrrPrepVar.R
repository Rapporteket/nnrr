#' Preparer variabler for plotting
#'
#' Denne funksjonen grupperer og klargjør variabler for andelsplot
#'
#' Her kan detaljer skrives
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return PrepData En liste med plotrelevante størrelser
#'
#' @export
#'
nnrrPrepVar <- function(RegData, valgtVar)
{
  retn= 'V'; tittel <- ''; AntVar <- NA; NVar <- NA; stabel <- 1
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- ''; incl_N=F

# endre

  if (valgtVar=='PatientAge') {
    RegData$Variabel <- RegData[, valgtVar]
    tittel <- 'Alder ved registrering'
    gr <- c(0, seq(10, 80, 10), 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar=='FABQ.Score1') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- 'FABQ fysisk aktivitet'
    gr <- c(0, 14, 17, 24)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- paste0(levels(RegData$VariabelGr), c(' ubetydelig', ' moderat', ' høy'))
    subtxt <- 'Score'
  }

  if (valgtVar=='FABQ.Score2') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- 'FABQ arbeid'
    gr <- c(0, 20, 25, 42)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- paste0(levels(RegData$VariabelGr), c(' ubetydelig', ' moderat', ' høy'))
    subtxt <- 'Score'
  }

  if (valgtVar=='HSCL10.Score') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- 'Hopkins symptom checklist'
    gr <- c(0, 1.85, 4)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- levels(RegData$VariabelGr)
    subtxt <- 'Score'
  }

  if (valgtVar=='Sammenhengende.varighet.av.naavaerende.smerter') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- c('Sammenhengende varighet av', 'nåværende smerter')
    gr <- 0:5
    grtxt <- c('Ikke svart', 'Ingen smerter', 'Mindre enn 3 måneder', '3 til 12 måneder', '1-2 år', 'Mer enn 2 år')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }



  if (valgtVar=='AarsakSmerte_PasRap') {
    tittel <- c('Hva tror du selv er årsak til smertene dine?', '(flere kryss er mulig)')
    # N <- dim(RegData)[1]
    AntVar <- apply(RegData[,c('Arbeidsbelastning', 'Hjemmebelastning', 'Folelsesmessig.belastning', 'Fritidsaktivitet',
                               'Skade.i.skjelett', 'Skade.i.muskulatur', 'Skade.i.nerve', 'Vet.ikke')], 2, function(x){sum(as.numeric(x), na.rm = T)})
    NVar<- apply(RegData[,c('Arbeidsbelastning', 'Hjemmebelastning', 'Folelsesmessig.belastning', 'Fritidsaktivitet',
                            'Skade.i.skjelett', 'Skade.i.muskulatur', 'Skade.i.nerve', 'Vet.ikke')], 2, function(x){length(which(!is.na(x)))})
    # NVar<-rep(N, length(AntVar))
    N <- max(NVar)
    grtxt <- c('Arbeidsbelastning', 'Hjemmebelastning', 'Følelsesmessig \nbelastning', 'Fritidsaktivitet',
               'Skade i skjelett', 'Skade i \nmuskulatur', 'Skade i nerve', 'Vet ikke')

  }

  if (valgtVar=='Hvor.fornoyd.er.du.med.behandlingen.du.har.faatt.for.de.aktuelle.plagene.inntil.i.dag.') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- c('Behandlingstilfredshet. Hvor fornøyd er du', 'med behandlingen du har fått for de', 'aktuelle plagene inntil i dag?')
    subtxt <- 'Hvor fornøyd er du med behandlingen du har fått for de aktuelle plagene inntil i dag?'
    gr <- c(1:5, 0)
    grtxt <- c('Fornøyd', 'Litt fornøyd', 'Hverken fornøyd \neller misfornøyd', 'Litt misfornøyd', 'Misfornøyd', 'Ikke svart')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='SmertestillendeResept') {
    RegData <- RegData[which(RegData$VarPre %in% 1:4 & RegData$VarPost %in% 1:4), ]
    grtxt <- c('Ikke brukt siste 4 uker', 'Sjeldnere enn hver uke', 'Hver uke men ikke daglig', 'Daglig')
    RegData$VarPre <- factor(RegData$VarPre, levels = c(1:4), labels = grtxt)
    RegData$VarPost <- factor(RegData$VarPost, levels = c(1:4), labels = grtxt)
    tittel <- c('Smertestillende på resept')
    retn <- 'H'
    cexgr <- 0.8
  }

  if (valgtVar=='SmertestillendeUtenResept') {
    RegData <- RegData[which(RegData$VarPre %in% 1:4 & RegData$VarPost %in% 1:4), ]
    grtxt <- c('Ikke brukt siste 4 uker', 'Sjeldnere enn hver uke', 'Hver uke men ikke daglig', 'Daglig')
    RegData$VarPre <- factor(RegData$VarPre, levels = c(1:4), labels = grtxt)
    RegData$VarPost <- factor(RegData$VarPost, levels = c(1:4), labels = grtxt)
    tittel <- c('Smertestillende uten resept')
    retn <- 'H'
    cexgr <- 0.8
  }

#   if (valgtVar=='SmertestillendeResept') {
#     grtxt <- c('Ikke brukt siste 4 uker', 'Sjeldnere enn hver uke', 'Hver uke men ikke daglig', 'Daglig', 'Ikke registrert')
#     RegData$VarPre <- factor(RegData$VarPre, levels = c(1:4, 0), labels = grtxt)
#     RegData$VarPost <- factor(RegData$VarPost, levels = c(1:4, 0), labels = grtxt)
#     tittel <- c('Smertestillende på resept')
#     retn <- 'H'
#     cexgr <- 0.8
#   }
#
#   if (valgtVar=='SmertestillendeUtenResept') {
#     grtxt <- c('Ikke brukt siste 4 uker', 'Sjeldnere enn hver uke', 'Hver uke men ikke daglig', 'Daglig', 'Ikke registrert')
#     RegData$VarPre <- factor(RegData$VarPre, levels = c(1:4, 0), labels = grtxt)
#     RegData$VarPost <- factor(RegData$VarPost, levels = c(1:4, 0), labels = grtxt)
#     tittel <- c('Smertestillende uten resept')
#     retn <- 'H'
#     cexgr <- 0.8
#   }
#

  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar, stabel=stabel, incl_N=incl_N)

  return(invisible(PlotParams))


}

#
# if (valgtVar == 'Ergoterapi') {
#   tittel <- 'Andel som får ergoterapi'
#   RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
#   RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
#   RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
#   grtxt <- c('Nei, men behov', 'Ja', 'Ikke behov', 'Ukjent')
#   gr <- c(0:2, 9)
#   RegData$Variabel <- RegData[, valgtVar]
#   RegData <- RegData[which(RegData$Variabel %in% gr), ]
#   RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
#   RegData$Gr <- factor(RegData$Diagnosegr_label)
# }
#
# if (valgtVar == 'AndelGenVerifisertSpesUndergr') { # per pasient
#   tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnose')
#   subtxt <- 'Diagnoser'
#   grtxt <- sort(c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G12.0', 'G12.1', 'G60.0'))
#   RegData <- RegData[which(as.character(RegData$DiagICD10) %in% grtxt), ]
#   SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
#                            by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
#                            {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
#   names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
#   RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
#   RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
#   RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
#   RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = c('Ja', 'Nei', 'Ukjent', 'Ikke registrert'))
#   RegData$Gr <- as.factor(as.character(RegData$DiagICD10))
# }




