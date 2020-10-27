rm(list=ls())
library(nnrr)
# library(tidyverse)

rap_aar <- 2019

pasientsvar_pre <- read.table('I:/nnrr/DataDump_MRS-PROD_1a_Spørreskjema+før+behandling_2020-10-27_red.csv', sep=';', header=T, stringsAsFactors = F)
legeskjema <- read.table('I:/nnrr/DataDump_MRS-PROD_1b_Registreringsskjema+poliklinikk_2020-08-04.csv', sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post <- read.table('I:/nnrr/DataDump_MRS-PROD_2_Spørreskjema+etter+behandling_2020-08-04.csv', sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

flere_hovedskjemaGuid <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
pasientsvar_pre <- pasientsvar_pre[!(pasientsvar_pre$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]

icd10 <- read.table('C:/GIT/nnrr/doc/icd10.csv', sep=';', header=T, stringsAsFactors = F, fileEncoding = 'UTF-8')

legeskjema$regstatus <- 1
pasientsvar_pre$regstatus <- 1
pasientsvar_post$regstatus <- 1

names(pasientsvar_pre)[names(pasientsvar_pre)=='SkjemaGUID'] <- 'SkjemaGUID_pre'
names(pasientsvar_post)[names(pasientsvar_post)=='SkjemaGUID'] <- 'SkjemaGUID_post'

RegData <- merge(legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'), all.x = TRUE)
RegData <- merge(RegData, pasientsvar_post, by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', suffixes = c('', '_post'), all.x = TRUE)

RegData$DiagnosticNumber1 <- trimws(RegData$DiagnosticNumber1)

RegData <- nnrrPreprosess(RegData = RegData)
RegDataAll <- RegData
RegData <- RegData[which(RegData$Aar >= 2015 & RegData$Aar <= rap_aar), ]
Oppf_data <- RegDataAll[which(RegDataAll$aar_oppfolg>= 2015 & RegDataAll$aar_oppfolg <= rap_aar), ]
Oppf_data$Aar <- Oppf_data$aar_oppfolg

rm(list = c('pasientsvar_pre', 'legeskjema', 'pasientsvar_post'))

kobling_resh_shusnavn <- RegData[match(unique(RegData$UnitId), RegData$UnitId), c("UnitId", "SykehusNavn")]
# write.csv2(kobling_resh_shusnavn, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/kobling_resh_shusnavn.csv', row.names = F)

map_resh_orgnr <- data.frame(orgnr_sh = c(974589095, 974749025, 974557746, 974795787),
                             resh = c(109834, 104293, 102959, 601032))

format_ut <- ".wmf"


########### Indikator 3: andel med tverrfaglig behandling

skjema1b <- RegData[RegData$regstatus==1, ]

skjema1b$tverrfaglig_behandlet <- 0
skjema1b$tverrfaglig_behandlet[skjema1b$Treatment_GroupInterdisciplinary2018 != 0 | skjema1b$Treatment_GroupInterdisciplinary != 0] <- 1
skjema1b$tverrfaglig_behandlet[skjema1b$Treatment_InvidualInterdisciplinary != 0] <- 1

######### Indikator #####################################################################
indikator <- skjema1b[, c("UnitId", "Aar", "tverrfaglig_behandlet", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind1_Tverrfaglig behandling_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_tverrfaglig_behandling"
Indikatorer <- indikator


########### Indikator 7 & 8: andel med klinisk viktig endring i ODI/NDI

aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]
aux <- aux[!is.na(aux$OdiScore) & !is.na(aux$OdiScore_post), ]
aux$odidiff_klin_viktig <- 0
aux$odidiff_klin_viktig[aux$OdiScore - aux$OdiScore_post >= 10] <- 1

######### Indikator #####################################################################
indikator <- aux[, c("UnitId", "aar_oppfolg", "odidiff_klin_viktig", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator <- indikator[indikator$Aar <= rap_aar, ]
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind2_Bedret funksjon_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_bedret_funksjon"
Indikatorer <- bind_rows(Indikatorer, indikator)


aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]
aux <- aux[!is.na(aux$OdiScore) & !is.na(aux$OdiScore_post), ]
aux$minimal_funksjonsnedsettelse <- 0
aux$minimal_funksjonsnedsettelse[aux$OdiScore_post <= 20] <- 1

indikator <- aux[, c("UnitId", "aar_oppfolg", "minimal_funksjonsnedsettelse", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator <- indikator[indikator$Aar <= rap_aar, ]
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind3_Funksjonsnedsettelse_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_funksjons_nedsettelse"
Indikatorer <- bind_rows(Indikatorer, indikator)


############### Indikator 9: andel med klinisk viktig endring smerte

aux <- Oppf_data %>% filter(regstatus_pre == 1 & regstatus_post == 1 & !is.na(PainExperiencesNoActivity) &
                              !is.na(PainExperiencesNoActivity_post) & PainExperiencesNoActivity != 0)

aux$pstEndringSmerteHvile <- (aux$PainExperiencesNoActivity - aux$PainExperiencesNoActivity_post)/aux$PainExperiencesNoActivity*100

aux$Indikator <- 0
aux$Indikator[aux$pstEndringSmerteHvile >= 20 ] <- 1

######### Indikator #####################################################################
indikator <- aux[, c("UnitId", "Aar", "Indikator", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind4_Bedring smerte_hvile_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_bedring_smerte_hvile"
Indikatorer <- bind_rows(Indikatorer, indikator)

#### Smerte i aktivitet

aux <- Oppf_data %>% filter(regstatus_pre == 1 & regstatus_post == 1 & !is.na(PainExperiencesActivity) &
                              !is.na(PainExperiencesActivity_post) & PainExperiencesActivity != 0)

aux$pstEndringSmerteAktiv <- (aux$PainExperiencesActivity - aux$PainExperiencesActivity_post)/aux$PainExperiencesActivity*100

aux$Indikator <- 0
aux$Indikator[aux$pstEndringSmerteAktiv >= 20 ] <- 1

######### Indikator #####################################################################
indikator <- aux[, c("UnitId", "Aar", "Indikator", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind4_Bedring smerte_aktiv_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_bedring_smerte_aktiv"
Indikatorer <- bind_rows(Indikatorer, indikator)


############ Indikator 10: andel tilbake i arbeid

Oppf_data$arbeid_ikke_besvart <- !(Oppf_data$Working | Oppf_data$SickLeave | Oppf_data$Stay_at_home | Oppf_data$Student |
                                     Oppf_data$Unemployed | Oppf_data$NAV | Oppf_data$Pension | Oppf_data$RetirementPension)
aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]

# aux <- aux[which(aux$SickLeave | aux$NAV), ]
aux <- aux[which((aux$SickLeave | aux$NAV) & aux$WorkingPercent != 100), ]
aux$tilbake_i_arbeid <- 0
aux$tilbake_i_arbeid[which(aux$WorkingPercent_post==100 & aux$WorkingStatus == 1)] <- 1

######### Indikator #####################################################################
indikator <- aux[, c("UnitId", "Aar", "tilbake_i_arbeid", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind5_Jobb_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_jobb"
Indikatorer <- bind_rows(Indikatorer, indikator)


############# Indikator 11: Pasientopplevd nytte av behandling

aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]

aux <- aux[aux$UseOfTreatment %in% 1:7, ]
aux$NytteAvBehandling <- 0
aux$NytteAvBehandling[aux$UseOfTreatment %in% 1:3] <- 1

######### Indikator #####################################################################
indikator <- aux[, c("UnitId", "Aar", "NytteAvBehandling", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind6_Bedring av behandling_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_bedring_av_behandling"
Indikatorer <- bind_rows(Indikatorer, indikator)


########### Indikator 12: Fornøydhet med behandlingen

aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1 & Oppf_data$TreatmentSatisfaction != 0 & !is.na(Oppf_data$TreatmentSatisfaction)), ]
aux$Fornoyd <- 0
aux$Fornoyd[which(aux$TreatmentSatisfaction %in% 4:5)] <- 1

######### Indikator #####################################################################
indikator <- aux[, c("UnitId", "Aar", "Fornoyd", "SykehusNavn")]
names(indikator) <- c('ReshId', 'Aar', 'Teller', "SykehusNavn")
indikator$Nevner <- 1
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]

# write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/6. NNRR/Indikatorer/Ind7_Misfornøyd_NNRR.csv', row.names = F)

indikator$ind_id <- "nnrr_misfornoeyd"
Indikatorer <- bind_rows(Indikatorer, indikator)



Indikatorer <- Indikatorer[, c(6,2,3,5,7)]
names(Indikatorer) <- c("orgnr",	"year",	"var",	"denominator",	"ind_id")

# write.csv2(Indikatorer, 'I:/nnrr/nnrr_ind_27102020.csv', row.names = F, fileEncoding = "UTF-8")


dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NNRR.xlsx",
                         sheet = 1)






















