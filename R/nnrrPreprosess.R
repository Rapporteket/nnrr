#' Preprosesser data for bruk i NNRR sine rapporter
#'
#' Denne funksjonen gjor nodvendig preprosessering av NNRR sin data for bruk i rapporter
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return Et preprosessert datasett
#'
#' @export
#'

nnrrPreprosess <- function(RegData)
{

  RegData$Besoksdato <- as.POSIXlt(RegData$Besoksdato, format="%d.%m.%Y")
  RegData$Besoksdato_pre <- as.POSIXlt(RegData$Besoksdato_pre, format="%d.%m.%Y")
  RegData$Besoksdato_post <- as.POSIXlt(RegData$Besoksdato_post, format="%d.%m.%Y")
  RegData$FormDate <- as.POSIXlt(RegData$FormDate, format="%d.%m.%Y")
  RegData$FormDate_pre <- as.POSIXlt(RegData$FormDate_pre, format="%d.%m.%Y")
  RegData$FormDate_post <- as.POSIXlt(RegData$FormDate_post, format="%d.%m.%Y")

  RegData$regstatus_pre[which(is.na(RegData$regstatus_pre))] <- 0
  RegData$regstatus_post[which(is.na(RegData$regstatus_post))] <- 0
  RegData$regstatus[which(is.na(RegData$regstatus))] <- 0

  # Ordne dato for pre og post

  #### Testing ###########
  # RegData$Besoksdato[RegData$regstatus_pre == 1] <- RegData$Besoksdato_pre[RegData$regstatus_pre == 1]
  #########################

  RegData$Besoksdato[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$Besoksdato_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$Besoksdato[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$Besoksdato_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData$PatientAge[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$PatientAge_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$PatientAge[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$PatientAge_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData$ReshId[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$ReshId_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$ReshId[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$ReshId_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]

  RegData$Aar <- RegData$Besoksdato$year+1900

  RegData$ErMann <- NA
  RegData$ErMann[which(RegData$PatientGender == 'Female')] <- 0
  RegData$ErMann[which(RegData$PatientGender == 'Male')] <- 1

  RegData$SoktUforetrygd <- factor(RegData$PainDisable,
                                   levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))
  RegData$SoktErstatning <- factor(RegData$PainCompensation,
                                   levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))
  RegData$OnsketTilbake <- factor(RegData$ProfessionWantedBack,
                                  levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))
  RegData$DagligRoyk <- factor(RegData$Smoking,
                               levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))

  RegData$HSCL10.Score <- as.numeric(sapply(as.character(RegData$HSCL10.Score), gsub, pattern = ",", replacement= "."))
  RegData$OdiScore <- as.numeric(sapply(as.character(RegData$OdiScore), gsub, pattern = ",", replacement= "."))
  RegData$OdiScore_post <- as.numeric(sapply(as.character(RegData$OdiScore_post), gsub, pattern = ",", replacement= "."))
  RegData$NdiScore <- as.numeric(sapply(as.character(RegData$NdiScore), gsub, pattern = ",", replacement= "."))
  RegData$NdiScore_post <- as.numeric(sapply(as.character(RegData$NdiScore_post), gsub, pattern = ",", replacement= "."))
  RegData$Eq5dScore <- as.numeric(sapply(as.character(RegData$Eq5dScore), gsub, pattern = ",", replacement= "."))
  RegData$Eq5dScore_post <- as.numeric(sapply(as.character(RegData$Eq5dScore_post), gsub, pattern = ",", replacement= "."))


  RegData$SykehusNavn <- NA
  RegData$SykehusNavn[RegData$ReshId == 102959] <- 'Haukeland'
  RegData$SykehusNavn[RegData$ReshId == 104293] <- 'St. Olavs'
  RegData$SykehusNavn[RegData$ReshId == 109834] <- 'OUS'
  RegData$SykehusNavn[RegData$ReshId == 601032] <- 'UNN'


  # names(RegData)[which(names(RegData) == 'Paa.en.skala.fra.0..verst.tenkelig.tilstand..til.100..best.tenkelige.tilstand...angi.din.naavaerende.helsetilstand..')] <- 'EQ5D.VAS'

  names(RegData)[which(names(RegData) == 'Eq5dHealthLevel')] <- 'EQ5D.VAS'
  names(RegData)[which(names(RegData) == 'Eq5dHealthLevel_post')] <- 'EQ5D.VAS_post'

  RegData$FamilyStatus[RegData$FamilyStatus==0] <- 99
  RegData$FamilyStatus <- factor(RegData$FamilyStatus, levels = c(1:3,99),
                                 labels = c('Gift/Reg. partner','Samboende', 'Enslig', 'Ikke svart'))
  RegData$EducationLevel[RegData$EducationLevel==0] <- 99
  RegData$EducationLevel <- factor(RegData$EducationLevel, levels = c(1:5,99),
                                   labels = c('Grunnskole 7-10 år, framhaldskole eller folkehøyskole'
                                              , 'Yrkesfaglig videregående skole, yrkesskole eller realskole'
                                              , 'Allmennfaglig videregående skole eller gymnas'
                                              , 'Høyskole eller universitet (mindre en 4 år)'
                                              , 'Høyskole eller universitet (4 år eller mer)', 'Ikke svart'))

  RegData$PainDurationNow[RegData$PainDurationNow==0] <- 99
  RegData$PainDurationNow <- factor(RegData$PainDurationNow, levels = c(1:5,99),
                                 labels = c('Ingen smerter', 'Mindre enn 3 måneder', '3 til 12 måneder',
                                            '1-2 år', 'Mer enn 2', 'Ikke svart'))


  RegData$NeckSurgery[RegData$NeckSurgery==0] <- 99
  RegData$NeckSurgery <- factor(RegData$NeckSurgery, levels = c(1:2,99),
                                labels = c('Ja', 'Nei', 'Ikke svart'))

  mapping_ny_gml <- data.frame(gammel=c('Yes', 'No', 'Unknown','None'), ny=c(1:3,99))
  RegData$BackSurgery <- mapping_ny_gml$ny[match(RegData$BackSurgery, mapping_ny_gml$gammel)]
  RegData$BackSurgery[RegData$BackSurgery==0] <- 99
  RegData$BackSurgery <- factor(RegData$BackSurgery, levels = c(1:3,99),
                                labels = c('Ja', 'Nei', 'Ukjent', 'Ikke svart'))


  return(RegData)
}


#
#
#   RegData[, c('PainCausesWork', 'PainCausesHome', 'PainCausesMental',
#               'PainCausesLeisure', 'PainCausesSkeleton', 'PainCausesMuscle',
#               'PainCausesNerve', 'PainCausesUknown', 'Hode', 'Nakke', 'Ovre.del.mage', "Venstre.skulder", "Hoyre.albue", "Venstre.albue", "Mage",
#               "Hoyre.haandledd", "Venstre.haandledd", "Hoyre.hofte.laar", "Venstre.hofte.laar", "Hoyre.kne", "Venstre.kne",
#               "Hoyre.ankel.fot", "Venstre.ankel.fot")] <-
#     apply(RegData[, c('Arbeidsbelastning', 'Hjemmebelastning', 'Folelsesmessig.belastning',
#                       'Fritidsaktivitet', 'Skade.i.skjelett', 'Skade.i.muskulatur',
#                       'Skade.i.nerve', 'Vet.ikke', 'Hode', 'Nakke', 'Ovre.del.mage', "Venstre.skulder", "Hoyre.albue", "Venstre.albue", "Mage",
#                       "Hoyre.haandledd", "Venstre.haandledd", "Hoyre.hofte.laar", "Venstre.hofte.laar", "Hoyre.kne", "Venstre.kne",
#                       "Hoyre.ankel.fot", "Venstre.ankel.fot")],
#           2, function(x){as.logical(x)})


