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
  boolske_var2 <- c(boolske_var2, paste0(as.character(varnavn_2$Variabelnavn)[which(as.character(varnavn_2$Felttype) == 'Avkrysning')], "_post"))
  boolske_var <- intersect(c(boolske_var1b, boolske_var1a, boolske_var2), names(RegData))
  RegData[, boolske_var] <-
    apply(RegData[, boolske_var], 2, as.logical)
  RegData$TreatmentOperation <- as.logical(RegData$TreatmentOperation)
  # RegData[, c(boolske_var1b, boolske_var1a, boolske_var2)] <-
  #   apply(RegData[, c(boolske_var1b, boolske_var1a, boolske_var2)], 2, as.logical)
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
  RegData$Mnd <- as.numeric(format(RegData$Besoksdato, '%m')) # RegData$OperasjonsDato$mon +1
  RegData$Kvartal <- floor((RegData$Mnd - 1)/3)+1
  RegData$Halvaar <- floor((RegData$Mnd - 1)/6)+1

  # library(lubridate)
  RegData$dato_oppfolg <- RegData$Besoksdato %m+% months(6)
  RegData$aar_oppfolg <- as.numeric(format(RegData$dato_oppfolg, '%Y'))

  RegData$ErMann <- NA
  RegData$ErMann[which(RegData$PatientGender == 2)] <- 0
  RegData$ErMann[which(RegData$PatientGender == 1)] <- 1

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
  RegData$SykehusNavn[RegData$UnitId == 601032] <- 'UNN-Tromsø'
  RegData$SykehusNavn[RegData$UnitId == 700735] <- 'UNN-Harstad'
  RegData$SykehusNavn[RegData$UnitId == 102169] <- 'Sandnessjøen'
  RegData$SykehusNavn[RegData$UnitId == 114174] <- 'Kristiansand'
  RegData$SykehusNavn[RegData$UnitId == 4211588] <- 'Stavanger'
  RegData$SykehusNavn[RegData$UnitId == 4212982] <- 'Ålesund'
  RegData$SykehusNavn[RegData$UnitId == 105821] <- 'Levanger'
  RegData$SykehusNavn[RegData$UnitId == 103736] <- 'Drammen'
  # 102169 Sandnessjøen
  # 114174 Kristiansand
  # 4211588 Stavanger
  # 4212982 Helse Møre og Romsdal

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

  RegData$WorkingStatus_label <- factor(RegData$WorkingStatus, levels = 0:8, labels = c('Ikke utfylt', 'Inntektsgivende arbeid',
                                                                                        'Student/skoleelev', 'Arbeidsledig',
                                                                                        'Alderspensjonist', 'Hjemmeværende',
                                                                                        'Sykemeldt', 'Arbeidsavklaringspenger',
                                                                                        'Permanent uførepensjon'))
  RegData$Tverrfaglig_vurdering_antall <- rowSums(RegData[, c("FirstMedExamDoctor", "FirstMedExamNurse", "FirstMedExamPhys", "FirstMedExamOther")])
  RegData$Tverrfaglig_vurdering <- 0
  RegData$Tverrfaglig_vurdering[RegData$Tverrfaglig_vurdering_antall >= 2] <- 1

  RegData$beh_spes <- 0
  RegData$beh_spes[(RegData$Treatment_IndividualFollowUp1to2Times | RegData$Treatment_InvidualInterdisciplinary != 0) &
                     (RegData$Treatment_GroupInterdisciplinary == 0 & RegData$Treatment_GroupInterdisciplinary2018 == 0)] <- 1
  RegData$beh_spes[(RegData$Treatment_GroupInterdisciplinary != 0 | RegData$Treatment_GroupInterdisciplinary2018 != 0) &
                     (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 2
  RegData$beh_spes[(RegData$Treatment_GroupInterdisciplinary != 0 | RegData$Treatment_GroupInterdisciplinary2018 != 0) &
                     (RegData$Treatment_IndividualFollowUp1to2Times | RegData$Treatment_InvidualInterdisciplinary != 0)] <- 3
  RegData$beh_spes <- factor(RegData$beh_spes, levels = 0:3, labels = c("Ingen", "Individuell", "Gruppe", "Begge"))


  RegData$beh_spes_v2 <- 0
  RegData$beh_spes_v2[(RegData$Treatment_IndividualFollowUp1to2Times | RegData$Treatment_InvidualInterdisciplinary != 0) &
                     (RegData$Treatment_GroupInterdisciplinary == 0 & RegData$Treatment_GroupInterdisciplinary2018 == 0)] <- 1
  RegData$beh_spes_v2[(RegData$Treatment_GroupInterdisciplinary == 1 | RegData$Treatment_GroupInterdisciplinary2018 == 1) &
                     (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 2
  RegData$beh_spes_v2[(RegData$Treatment_GroupInterdisciplinary == 2 | RegData$Treatment_GroupInterdisciplinary2018 %in% 2:3) &
                        (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 3
  RegData$beh_spes_v2[(RegData$Treatment_GroupInterdisciplinary == 3 | RegData$Treatment_GroupInterdisciplinary2018 == 4) &
                        (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 4
  RegData$beh_spes_v2[(RegData$Treatment_GroupInterdisciplinary != 0 | RegData$Treatment_GroupInterdisciplinary2018 != 0) &
                     (RegData$Treatment_IndividualFollowUp1to2Times | RegData$Treatment_InvidualInterdisciplinary != 0)] <- 5
  RegData$beh_spes_v2 <- factor(RegData$beh_spes_v2, levels = 0:5, labels = c("Ingen", "Individuell", "Gruppe, 1-3 ganger",
                                                                              "Gruppe, 4-10 ganger", "Gruppe, >10 ganger", "Begge"))


  RegData$beh_spes_v3 <- 0
  RegData$beh_spes_v3[(RegData$Treatment_IndividualFollowUp1to2Times | RegData$Treatment_InvidualInterdisciplinary != 0) &
                        (RegData$Treatment_GroupInterdisciplinary2018 == 0)] <- 1
  RegData$beh_spes_v3[(RegData$Treatment_GroupInterdisciplinary2018 == 1) &
                        (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 2
  RegData$beh_spes_v3[(RegData$Treatment_GroupInterdisciplinary2018 %in% 2) &
                        (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 3
  RegData$beh_spes_v3[(RegData$Treatment_GroupInterdisciplinary2018 %in% 3) &
                        (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 4
  RegData$beh_spes_v3[(RegData$Treatment_GroupInterdisciplinary2018 == 4) &
                        (!RegData$Treatment_IndividualFollowUp1to2Times & RegData$Treatment_InvidualInterdisciplinary == 0)] <- 5
  RegData$beh_spes_v3[(RegData$Treatment_GroupInterdisciplinary2018 != 0) &
                        (RegData$Treatment_IndividualFollowUp1to2Times | RegData$Treatment_InvidualInterdisciplinary != 0)] <- 6
  RegData$beh_spes_v3 <- factor(RegData$beh_spes_v3, levels = 0:6, labels = c("Ingen", "Individuell", "Gruppe, 1-3 ganger",
                                                                              "Gruppe, 4-6 ganger", "Gruppe, 7-10 ganger",
                                                                              "Gruppe, >10 ganger", "Begge"))

  return(RegData)
}

# mapping_ny_gml <- data.frame(gammel=c('Yes', 'No', 'Unknown','None'), ny=c(1:3,99))
# RegData$BackSurgery <- mapping_ny_gml$ny[match(RegData$BackSurgery, mapping_ny_gml$gammel)]
# RegData$BackSurgery[RegData$BackSurgery==0] <- 99
# RegData$BackSurgery <- factor(RegData$BackSurgery, levels = c(1:3,99),
#                               labels = c('Ja', 'Nei', 'Ukjent', 'Ikke svart'))
