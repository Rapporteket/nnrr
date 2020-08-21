rm(list=ls())
library(nnrr)
library(tidyverse)

pasientsvar_pre <- read.table('I:/nnrr/DataDump_Prod_1a_Spørreskjema+før+behandling_2019-06-11_red.csv', sep=';', header=T, stringsAsFactors = F)
legeskjema <- read.table('I:/nnrr/DataDump_Prod_1b_Registreringsskjema+poliklinikk_2019-06-11.csv', sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post <- read.table('I:/nnrr/DataDump_Prod_2_Spørreskjema+etter+behandling_2019-06-11.csv', sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

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
# RegData <- RegData[RegData$DiagnosticNumber1 != '', ]
# RegData <- RegData[RegData$DiagnosticNumber1 != '.', ]

RegData <- nnrrPreprosess(RegData = RegData)
RegDataAll <- RegData


RegData %>% group_by(Aar, SykehusNavn) %>% summarise('antall lege' = sum(FirstMedExamDoctor),
                                        'antall sykepleier' = sum(FirstMedExamNurse),
                                        'antall fysio' = sum(FirstMedExamPhys),
                                        'antall andre' = sum(FirstMedExamOther),
                                        N =n())

tmp <- RegData %>% group_by(Aar, SykehusNavn, PasientGUID) %>% summarise('antall_konsult' = FirstMedExamDoctor[1]+FirstMedExamNurse[1]+FirstMedExamPhys[1]+FirstMedExamOther[1],
                                                     N =n())

tmp %>% group_by(Aar) %>% summarise('Mer enn 1 konsult' = sum(antall_konsult>1),
                                    'Andel' = sum(antall_konsult>1)/n()*100,
                                    N = n())

tmp %>% group_by(Aar, SykehusNavn) %>% summarise('Mer enn 1 konsult' = sum(antall_konsult>1),
                                    'Andel' = sum(antall_konsult>1)/n()*100,
                                    N = n())

write.csv2(tmp, 'konsultasjoner_aar_shus.csv', row.names = F)


RegData[RegData$Aar>=2018,] %>% group_by(SykehusNavn, Aar) %>% summarise('Tverrfaglig behandling i gruppe 1-3 ganger' = sum(Treatment_GroupInterdisciplinary2018 == 1),
                                                                         'Tverrfaglig behandling i gruppe 4-6 ganger' = sum(Treatment_GroupInterdisciplinary2018 == 2),
                                                                         'Tverrfaglig behandling i gruppe 7-10 ganger' = sum(Treatment_GroupInterdisciplinary2018 == 3),
                                                                         N = n())






