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
  if (valgtVar=='tverrfaglig_behandlet') {
    tittel <- "Tverrfaglig behandlet"
    RegData <- RegData[RegData$regstatus==1, ]
    RegData$Variabel <- 0
    RegData$Variabel[RegData$Treatment_GroupInterdisciplinary2018 != 0 | RegData$Treatment_GroupInterdisciplinary != 0] <- 1
    RegData$Variabel[RegData$Treatment_InvidualInterdisciplinary != 0] <- 1
    grtxt <- c("Nei", "Ja")
    RegData$VariabelGr <- factor(RegData$Variabel, levels = 0:1, labels = grtxt)

  }

  if (valgtVar=='PatientAge') {
    RegData$Variabel <- RegData[, valgtVar]
    tittel <- 'Alder ved registrering'
    gr <- c(0, seq(20, 80, 10)+1, 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    # grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    grtxt <- c('0-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '80+')
    RegData <- RegData[order(RegData$S1b_DateOfCompletion, decreasing = TRUE), ] # pr. pasient, behold nyeste
    RegData <- RegData[match(unique(RegData$PasientGUID), RegData$PasientGUID), ]
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

  if (valgtVar=='PainDurationNow') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- c('Sammenhengende varighet av', 'nåværende smerter')
    # gr <- 0:5
    grtxt <- levels(RegData$PainDurationNow)
    RegData$VariabelGr <- RegData$PainDurationNow
    #   c('Ikke svart', 'Ingen smerter', 'Mindre enn 3 måneder', '3 til 12 måneder', '1-2 år', 'Mer enn 2 år')
    # RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }



  if (valgtVar=='AarsakSmerte_PasRap') {
    tittel <- c('Hva tror du selv er årsak til smertene dine?', '(flere kryss er mulig)')
    # N <- dim(RegData)[1]
    AntVar <- apply(RegData[,c('PainCausesWork', 'PainCausesHome', 'PainCausesMental', 'PainCausesLeisure',
                               'PainCausesSkeleton', 'PainCausesMuscle', 'PainCausesNerve', 'PainCausesWrongTreatmen', 'PainCausesUknown')],
                    2, function(x){sum(as.numeric(x), na.rm = T)})
    NVar<- apply(RegData[,c('PainCausesWork', 'PainCausesHome', 'PainCausesMental', 'PainCausesLeisure',
                            'PainCausesSkeleton', 'PainCausesMuscle', 'PainCausesNerve', 'PainCausesWrongTreatmen', 'PainCausesUknown')],
                 2, function(x){length(which(!is.na(x)))})
    # NVar<-rep(N, length(AntVar))
    N <- max(NVar)
    grtxt <- c('Arbeidsbelastning', 'Hjemmebelastning', 'Følelsesmessig \nbelastning', 'Fritidsaktivitet',
               'Skade i skjelett', 'Skade i \nmuskulatur', 'Skade i nerve', 'Feilbehandling', 'Vet ikke')
    retn <- 'H'

  }

  if (valgtVar=='beh_kommunalt') {
    tittel <- c('Behandling i kommunalhelsetjenesten')
    # N <- dim(RegData)[1]
    AntVar <- apply(RegData[,c("Treatment_NoTreatment", "Treatment_FollowUpMunicipality", "Treatment_FollowUpFysioterapeut",
                               "Treatment_FollowUpManuellTerapeut", "Treatment_FollowUpKiropraktor", "Treatment_FollowUpPsykolog",
                               "Treatment_FollowUpKommunalt", "Treatment_FollowUpMunicipalityOther")],
                    2, function(x){sum(as.numeric(x), na.rm = T)})
    NVar<- apply(RegData[,c("Treatment_NoTreatment", "Treatment_FollowUpMunicipality", "Treatment_FollowUpFysioterapeut",
                            "Treatment_FollowUpManuellTerapeut", "Treatment_FollowUpKiropraktor", "Treatment_FollowUpPsykolog",
                            "Treatment_FollowUpKommunalt", "Treatment_FollowUpMunicipalityOther")],
                 2, function(x){length(which(!is.na(x)))})
    # NVar<-rep(N, length(AntVar))
    N <- max(NVar)
    grtxt <- c('Ingen behandling', 'Oppfølging av lege', 'Oppfølging av fysioterapeut',
               'Oppfølging av manuell terapeut',
               'Oppfølging av kiropraktor', 'Oppfølging av psykolog',
               'Oppfølging i kommunehelsetjenesten', 'Arbeidsrettet oppfølging')
    retn <- 'H'

  }

  if (valgtVar=='beh_spesialist') {
    tittel <- c('Behandling i spesialisthelsetjenesten')
    # N <- dim(RegData)[1]
    RegData$Treatment_InvidualInterdisciplinary1 <- 0
    RegData$Treatment_InvidualInterdisciplinary1[RegData$Treatment_InvidualInterdisciplinary==1] <- 1
    RegData$Treatment_InvidualInterdisciplinary2 <- 0
    RegData$Treatment_InvidualInterdisciplinary2[RegData$Treatment_InvidualInterdisciplinary==2] <- 1
    RegData$Treatment_InvidualInterdisciplinary3 <- 0
    RegData$Treatment_InvidualInterdisciplinary3[RegData$Treatment_InvidualInterdisciplinary==3] <- 1
    RegData$Treatment_GroupInterdisciplinary1 <- 0
    RegData$Treatment_GroupInterdisciplinary1[RegData$Treatment_GroupInterdisciplinary2018==1 | RegData$Treatment_GroupInterdisciplinary==1] <- 1
    RegData$Treatment_GroupInterdisciplinary2 <- 0
    RegData$Treatment_GroupInterdisciplinary2[RegData$Treatment_GroupInterdisciplinary2018 %in% 2:3 | RegData$Treatment_GroupInterdisciplinary==2] <- 1
    RegData$Treatment_GroupInterdisciplinary3 <- 0
    RegData$Treatment_GroupInterdisciplinary3[RegData$Treatment_GroupInterdisciplinary2018==4 | RegData$Treatment_GroupInterdisciplinary==3] <- 1

    AntVar <- apply(RegData[,c("Treatment_TreatmentInSpecialistServices", "Treatment_FollowUpOperation",
                               "Treatment_TreatmentOtherRehabCentre", "Treatment_TreatmentOwnSpecialistServices",
                               "Treatment_ControlAfterReviewOrTreatment", "Treatment_IndividualFollowUp1to2Times",
                               "Treatment_InvidualInterdisciplinary1", "Treatment_InvidualInterdisciplinary2",
                               "Treatment_InvidualInterdisciplinary3", "Treatment_GroupInterdisciplinary1",
                               "Treatment_GroupInterdisciplinary2", "Treatment_GroupInterdisciplinary3")],
                    2, function(x){sum(as.numeric(x), na.rm = T)})
    NVar<- apply(RegData[,c("Treatment_TreatmentInSpecialistServices", "Treatment_FollowUpOperation",
                            "Treatment_TreatmentOtherRehabCentre", "Treatment_TreatmentOwnSpecialistServices",
                            "Treatment_ControlAfterReviewOrTreatment", "Treatment_IndividualFollowUp1to2Times",
                            "Treatment_InvidualInterdisciplinary1", "Treatment_InvidualInterdisciplinary2",
                            "Treatment_InvidualInterdisciplinary3", "Treatment_GroupInterdisciplinary1",
                            "Treatment_GroupInterdisciplinary2", "Treatment_GroupInterdisciplinary3")],
                 2, function(x){length(which(!is.na(x)))})
    # NVar<-rep(N, length(AntVar))
    N <- max(NVar)
    grtxt <- c('Behandling i\n spesialhelsetjenesten', 'Henvist til vurdering\n av operasjon',
               'Anbefaler henvisning til annet\n opptrenings /rehabiliteringssenter',
               'Behandling som settes i verk\n i egen spesialisthelsetjeneste',
               'Kontroll etter vurdering \n eller behandling', 'Individuell oppfølging\n 1-2 ganger',
               'Individuell tverrfaglig \n behandling 1-3 ganger', 'Individuell tverrfaglig\n behandling 4-10 ganger',
               'Individuell tverrfaglig\n behandling mer enn 10 ganger', 'Tverrfaglig behandling\n i gruppe 1-3 ganger',
               'Tverrfaglig behandling i\n gruppe 4-10 ganger', 'Tverrfaglig behandling\n i gruppe mer enn 10 ganger')
    retn <- 'H'

  }

  if (valgtVar=='pasrapp_beh_klinikk') {
    tittel <- c('Pasientrapportert behandling på', 'nakke- og ryggpoliklinikk')
    RegData <- RegData[RegData$regstatus_post==1, ]
    N <- dim(RegData)[1]
    RegData$ingen_beh <- !RegData$TreatmentPostHospital & !RegData$TreatmentPostHospitalSurgery
    RegData$beh_ukjent <- 0
    RegData$beh_ukjent[is.na(RegData$TreatmentPostHospital) & is.na(RegData$TreatmentPostHospitalSurgery)] <- 1
    AntVar <- apply(RegData[,c("ingen_beh", "TreatmentPostHospital", "TreatmentPostIndividual",
                               "TreatmentPostGroupbased", "TreatmentPostHospitalExtendedExamination", "beh_ukjent")],
                    2, function(x){sum(as.numeric(x), na.rm = T)})
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Ingen behandling', 'Operasjon', 'Individuelt behandlingstilbud', 'Gruppebasert behandling',
               'Tilbud om videre kontroller', 'Ukjent/Ikke besvart')
    retn <- 'H'

  }

  if (valgtVar=='Eq5dSatisfactionTreatment') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(!is.na(RegData$Variabel)), ]
    tittel <- c('Behandlingstilfredshet. Hvor fornøyd er du', 'med behandlingen du har fått for de', 'aktuelle plagene inntil i dag?')
    subtxt <- 'Hvor fornøyd er du med behandlingen du har fått for de aktuelle plagene inntil i dag?'
    gr <- c(1:5, 0)
    grtxt <- c('Fornøyd', 'Litt fornøyd', 'Hverken fornøyd \neller misfornøyd', 'Litt misfornøyd', 'Misfornøyd', 'Ikke svart')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }


########################################################################################
########################## Pre-post variabler ##########################################

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


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar, stabel=stabel, incl_N=incl_N)

  return(invisible(PlotParams))


}
