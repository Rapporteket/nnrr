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
  boolske_var1b <- as.character(varnavn_1b$Variabelnavn)[which(as.character(varnavn_1b$Felttype) == 'Avkrysning')]
  boolske_var1a <- as.character(varnavn_1a$Variabelnavn)[which(as.character(varnavn_1a$Felttype) == 'Avkrysning')]
  boolske_var2 <- as.character(varnavn_2$Variabelnavn)[which(as.character(varnavn_2$Felttype) == 'Avkrysning')]
  RegData[, c(boolske_var1b, boolske_var1a, boolske_var2)] <-
    apply(RegData[, c(boolske_var1b, boolske_var1a, boolske_var2)], 2, as.logical)
  # dato_var <- c(as.character(varnavn_1b$Variabelnavn)[which(as.character(varnavn_1b$Felttype) == 'Dato/tid')],
  #               as.character(varnavn_1a$Variabelnavn)[which(as.character(varnavn_1a$Felttype) == 'Dato/tid')],
  #               as.character(varnavn_2$Variabelnavn)[which(as.character(varnavn_2$Felttype) == 'Dato/tid')])

  # RegData$Besoksdato <- as.POSIXct(RegData$Besoksdato, format="%d.%m.%Y")
  # RegData$Besoksdato_pre <- as.POSIXct(RegData$Besoksdato_pre, format="%d.%m.%Y")
  # RegData$Besoksdato_post <- as.POSIXct(RegData$Besoksdato_post, format="%d.%m.%Y")
  # RegData$FormDate <- as.POSIXct(RegData$FormDate, format="%d.%m.%Y")
  # RegData$FormDate_pre <- as.POSIXct(RegData$FormDate_pre, format="%d.%m.%Y")
  # RegData$FormDate_post <- as.POSIXct(RegData$FormDate_post, format="%d.%m.%Y")

  RegData[, c("FormDate", "FormDate_pre", "FormDate_post", "S1b_DateOfCompletion", "S1b_DateOfCompletion_pre",
              "S1b_DateOfCompletion_post", "DateOfCompletion", "DateOfCompletion_post")] <-
    mutate_all(RegData[, c("FormDate", "FormDate_pre", "FormDate_post", "S1b_DateOfCompletion",
                           "S1b_DateOfCompletion_pre","S1b_DateOfCompletion_post", "DateOfCompletion", "DateOfCompletion_post")],
               funs(as.POSIXct(., format="%d.%m.%Y")))
  RegData$Besoksdato <- RegData$S1b_DateOfCompletion

  RegData <- RegData[order(RegData$DateOfCompletion, decreasing = T, na.last = T), ] # Hvis flere pasientskjema, bruk nyeste
  RegData <- RegData[match(unique(RegData$SkjemaGUID), RegData$SkjemaGUID), ]

  RegData$regstatus_pre[which(is.na(RegData$regstatus_pre))] <- 0
  RegData$regstatus_post[which(is.na(RegData$regstatus_post))] <- 0
  RegData$regstatus[which(is.na(RegData$regstatus))] <- 0

  # Ordne dato for pre og post

  RegData$Besoksdato[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$Besoksdato_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$Besoksdato[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$Besoksdato_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData$PatientAge[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$PatientAge_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$PatientAge[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$PatientAge_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData$UnitId[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$UnitId_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$UnitId[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$UnitId_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData <- RegData[!is.na(RegData$Besoksdato), ]

  # RegData$Aar <- RegData$Besoksdato$year+1900
  RegData$Aar <- as.numeric(format(RegData$Besoksdato, '%Y'))

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
  RegData$SykehusNavn[RegData$UnitId == 102959] <- 'Haukeland'
  RegData$SykehusNavn[RegData$UnitId == 104293] <- 'St. Olavs'
  RegData$SykehusNavn[RegData$UnitId == 109834] <- 'OUS'
  RegData$SykehusNavn[RegData$UnitId == 601032] <- 'UNN'

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
  RegData$NeckSurgery <- factor(RegData$NeckSurgery, levels = c(1:3,99),
                                labels = c('Ja', 'Nei', 'Ukjent', 'Ikke utfylt'))

  RegData$BackSurgery[RegData$BackSurgery==0] <- 99
  RegData$BackSurgery <- factor(RegData$BackSurgery, levels = c(1:3,99),
                                labels = c('Ja', 'Nei', 'Ukjent', 'Ikke utfylt'))

  navn2 <- varnavn_2$Variabelnavn[which(!is.na(varnavn_2$Variabelnavn))]
  indekser_kodebok <- which(varnavn_2$Variabelnavn == 'UseOfTreatment'):(which(varnavn_2$Variabelnavn == navn2[which(navn2=='UseOfTreatment')+1])-1)
  RegData$UseOfTreatmentLabel <- factor(RegData$UseOfTreatment, levels = varnavn_2$kode[c(indekser_kodebok[-1], indekser_kodebok[1])],
                                          labels = varnavn_2$label[c(indekser_kodebok[-1], indekser_kodebok[1])])

  navn1a <- varnavn_1a$Variabelnavn[which(!is.na(varnavn_1a$Variabelnavn))]
  indekser_kodebok <- which(varnavn_1a$Variabelnavn == 'PhysicalActivity'):(which(varnavn_1a$Variabelnavn == navn1a[which(navn1a=='PhysicalActivity')+1])-1)

  RegData$PhysicalActivityLabel <- factor(RegData$PhysicalActivity, levels = varnavn_1a$kode[c(indekser_kodebok[-1], indekser_kodebok[1])],
                                          labels = varnavn_1a$label[c(indekser_kodebok[-1], indekser_kodebok[1])])


  return(RegData)
}

# mapping_ny_gml <- data.frame(gammel=c('Yes', 'No', 'Unknown','None'), ny=c(1:3,99))
# RegData$BackSurgery <- mapping_ny_gml$ny[match(RegData$BackSurgery, mapping_ny_gml$gammel)]
# RegData$BackSurgery[RegData$BackSurgery==0] <- 99
# RegData$BackSurgery <- factor(RegData$BackSurgery, levels = c(1:3,99),
#                               labels = c('Ja', 'Nei', 'Ukjent', 'Ikke svart'))
