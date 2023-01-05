rm(list=ls())
library(nnrr)

######### Tabeller Kjetil 05.01.2023  ########################################

RegData <- nnrr::nnrrHentRegData()

RegData %>%
  dplyr::filter(Aar > 2019) %>%
  dplyr::group_by(SykehusNavn, Aar) %>%
  dplyr::summarise(
    var = paste0(sum(Interpreter, na.rm = T), " av ", sum(!is.na(Interpreter)))
  ) %>% tidyr::spread(key = Aar, value = var, fill = "") %>%


RegData %>%
  dplyr::filter(Aar > 2019) %>%
  dplyr::group_by(SykehusNavn, Aar) %>%
  dplyr::summarise(
    var = paste0(sum(NationalInterpreter, na.rm = T), " av ", sum(!is.na(NationalInterpreter)))
  ) %>% tidyr::spread(key = Aar, value = var, fill = "")

RegData %>%
  dplyr::filter(Aar > 2019) %>%
  dplyr::group_by(SykehusNavn, Aar) %>%
  dplyr::summarise(
    var = length(intersect(NationalCountry, 1:7))
  ) %>% tidyr::spread(key = Aar, value = var, fill = "")





########## Utlevering hianor 02.12.2022 #######################################
# pasientsvar_pre <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+for+behandling_2022-12-09_1116_red.csv',
#                               sep=';', header=T, stringsAsFactors = F)
pasientsvar_pre <- readr::read_csv2('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+for+behandling_2022-12-09_1116_v2.csv')
legeskjema <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-12-09_1116.csv',
                         sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+6+maneder+etter+behandling_2022-12-09_1116.csv',
                               sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

legeskjema <- legeskjema[which(as.Date(legeskjema$S1b_DateOfCompletion, format="%d.%m.%Y") >= "2016-01-01"), ]
pasientsvar_pre <- pasientsvar_pre[pasientsvar_pre$HovedskjemaGUID %in% legeskjema$SkjemaGUID, ]
pasientsvar_post <- pasientsvar_post[pasientsvar_post$HovedskjemaGUID %in% legeskjema$SkjemaGUID, ]

ikkemed <- c("BackSurgery", "NeckSurgery", "PelvisSurgery", "Radiological_None", "RadiologicalUS_CT", "RadiologicalUS_MR",
             "RadiologicalUS_Radikulgraphy", "RadiologicalUS_Discography", "RadiologicalUS_LS_C_Columna",
             "RadiologicalUS_FlexionExtention", "RadiologicalF_Normal", "RadiologicalF_DiscHernitation",
             "RadiologicalF_CentralSpinalCord", "RadiologicalF_RecesStenosis", "RadiologicalF_Spondylolisthesis2018",
             "RadiologicalF_Spondylolisthesis", "RadiologicalF_Scoliosis", "RadiologicalF_Scoliosis_Subcategory",
             "RadiologicalF_Modicchanges", "RadiologicalF_Modicchanges1", "RadiologicalF_Modicchanges2",
             "RadiologicalF_Modicchanges3", "RadiologicalF_ModicchangesUnspecified", "RadiologicalF_Other",
             "OtherSupplementaryDiagnostic_DiagnosticInjection", "OtherSupplementaryDiagnostic_Radikulgraphy",
             "RadiologicalUS_DiagnosticBlock", "OtherSupplementaryDiagnostic_Emg", "OtherSupplementaryDiagnostic_Nevrografi")
legeskjema <- legeskjema[, -which(names(legeskjema) %in% ikkemed)]

flere_hovedskjemaGuid_pre <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
flere_hovedskjemaGuid_post <- names(table(pasientsvar_post$HovedskjemaGUID))[table(pasientsvar_post$HovedskjemaGUID)>1]

write.csv2(legeskjema, "C:/GIT/data/nnrr/skjema1b_09122022.csv", row.names = F, fileEncoding = "Latin1")
# write.csv2(pasientsvar_pre, "C:/GIT/data/nnrr/skjema1a_09122022.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(pasientsvar_post, "C:/GIT/data/nnrr/skjema2_09122022.csv", row.names = F, fileEncoding = "Latin1")

readr::write_csv2(pasientsvar_pre, "C:/GIT/data/nnrr/skjema1a_09122022_v2.csv")


########## Utlevering HiaNor forberedelser #######################################

var1 <- readxl::read_xlsx("~/nnrr/doc/HIANOR Variabler kun variabelnavn.xlsx", sheet = 1)
var2 <- readxl::read_xlsx("~/nnrr/doc/HIANOR Variabler kun variabelnavn.xlsx", sheet = 2)
var3 <- readxl::read_xlsx("~/nnrr/doc/HIANOR Variabler kun variabelnavn.xlsx", sheet = 3)


pasientsvar_pre <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Pasientskjema+før+behandling_2022-11-07_1221.csv',
                              sep=';', header=T, stringsAsFactors = F, fileEncoding = "UTF-8-BOM")
legeskjema <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-11-07_1221.csv', sep=';',
                         header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Pasientskjema+6+måneder+etter+behandling_2022-11-07_1221.csv',
                               sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post2 <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Pasientskjema+12+måneder+etter+behandling_2022-11-07_1221.csv',
                                sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)



var3$Variabelnavn[match(c("Eq5d-L-spm1", "Eq5d-L-spm2", "Eq5d-L-spm3",
                          "Eq5d-L-spm4", "Eq5d-L-spm5", "Eq5d-L-spm6"),
                        var3$Variabelnavn)] <- c("Eq5d_L_spm1", "Eq5d_L_spm2", "Eq5d_L_spm3",
                                                 "Eq5d_L_spm4", "Eq5d_L_spm5", "Eq5d_L_spm6")
var1$Variabelnavn[match(c("Eq5d-L-spm1", "Eq5d-L-spm2", "Eq5d-L-spm3",
                          "Eq5d-L-spm4", "Eq5d-L-spm5", "Eq5d-L-spm6"),
                        var1$Variabelnavn)] <- c("Eq5d_L_spm1", "Eq5d_L_spm2", "Eq5d_L_spm3",
                                                 "Eq5d_L_spm4", "Eq5d_L_spm5", "Eq5d_L_spm6")
var2$Variabelnavn[match(c("Eq5d-L-spm1", "Eq5d-L-spm2", "Eq5d-L-spm3",
                          "Eq5d-L-spm4", "Eq5d-L-spm5", "Eq5d-L-spm6"),
                        var2$Variabelnavn)] <- c("Eq5d_L_spm1", "Eq5d_L_spm2", "Eq5d_L_spm3",
                                                 "Eq5d_L_spm4", "Eq5d_L_spm5", "Eq5d_L_spm6")

setdiff(var3$Variabelnavn, names(pasientsvar_pre))
setdiff(var2$Variabelnavn, names(pasientsvar_post))
setdiff(var1$Variabelnavn, names(pasientsvar_post2))


########## Utlevering AID spine 10.05.2022 #######################################


# pasientsvar_pre <- readr::read_csv2('I:/nnrr/DataDump_MRS-PROD_Pasientskjema+før+behandling_2022-05-09_1545_red.csv',
#                                     locale = readr::locale(encoding = "latin1"))
pasientsvar_6mnd <- read.table('I:/nnrr/DataDump_MRS-PROD_Pasientskjema+6+måneder+etter+behandling_2022-05-09_1545.csv',
                               sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
# pasientsvar_12mnd <- read.table('I:/nnrr/DataDump_MRS-PROD_Pasientskjema+12+måneder+etter+behandling_2022-05-09_1545.csv',
#                                sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
legeskjema <- read.table('I:/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-05-09_1544.csv',
                         sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

var_behandler <- readxl::read_xlsx('I:/nnrr/AID-Spine­_kodebok_nnrr.xlsx', sheet = 7)
var_6mnd <- readxl::read_xlsx('I:/nnrr/AID-Spine­_kodebok_nnrr.xlsx', sheet = 5)

legeskjema$S1b_DateOfCompletion <- as.Date(legeskjema$S1b_DateOfCompletion, format="%d.%m.%Y")
pasientsvar_pre$S1b_DateOfCompletion <- as.Date(pasientsvar_pre$S1b_DateOfCompletion, format="%d.%m.%Y")
legeskjema <- legeskjema[which(legeskjema$S1b_DateOfCompletion < "2017-01-01"), ]
legeskjema <- legeskjema[which(legeskjema$S1b_DateOfCompletion >= "2015-01-01" |
                                 legeskjema$S1b_DateOfCompletion >= "2007-01-01" &
                                 legeskjema$S1b_DateOfCompletion < "2009-01-01"), ]
pasientsvar_6mnd <- pasientsvar_6mnd[pasientsvar_6mnd$HovedskjemaGUID %in% legeskjema$SkjemaGUID, ]

var_behandler <- intersect(var_behandler$Variabelnavn, names(legeskjema))
var_6mnd <- intersect(var_6mnd$Variabelnavn, names(pasientsvar_6mnd))

########## Utlevering Bjørneboe 31.03.2022 #######################################
pasientsvar_pre <- read.table('I:/nnrr/DataDump_MRS-PROD_Pasientskjema+før+behandling_2022-03-31_1351.csv',
                              sep=';', header=T, stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
legeskjema <- read.table('I:/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-03-31_1351.csv',
                         sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

legeskjema <- legeskjema[lubridate::year(as.Date(legeskjema$S1b_DateOfCompletion, format="%d.%m.%Y")) %in% 2021, ]
pasientsvar_pre <- pasientsvar_pre[pasientsvar_pre$HovedskjemaGUID %in% legeskjema$SkjemaGUID, ]
legeskjema <- legeskjema[legeskjema$SkjemaGUID %in% pasientsvar_pre$HovedskjemaGUID, ]

write.csv2(legeskjema, "I:/nnrr/skjema1b_2021.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(pasientsvar_pre, "I:/nnrr/skjema1a_2021.csv", row.names = F, fileEncoding = "Latin1")


########## Utlevering 02.12.2020 #######################################
pasientsvar_pre <- read.table('I:/nnrr/DataDump_MRS-PROD_1a_Spørreskjema+før+behandling_2020-10-27_red.csv',
                              sep=';', header=T, stringsAsFactors = F)
legeskjema <- read.table('I:/nnrr/DataDump_MRS-PROD_1b_Registreringsskjema+poliklinikk_2020-10-27.csv',
                         sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post <- read.table('I:/nnrr/DataDump_MRS-PROD_2_Spørreskjema+etter+behandling_2020-10-27.csv',
                               sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

legeskjema <- legeskjema[which(as.Date(legeskjema$S1b_DateOfCompletion, format="%d.%m.%Y") >= "2016-01-01"), ]
pasientsvar_pre <- pasientsvar_pre[pasientsvar_pre$HovedskjemaGUID %in% legeskjema$SkjemaGUID, ]
pasientsvar_post <- pasientsvar_post[pasientsvar_post$HovedskjemaGUID %in% legeskjema$SkjemaGUID, ]

ikkemed <- c("BackSurgery", "NeckSurgery", "PelvisSurgery", "Radiological_None", "RadiologicalUS_CT", "RadiologicalUS_MR",
             "RadiologicalUS_Radikulgraphy", "RadiologicalUS_Discography", "RadiologicalUS_LS_C_Columna",
             "RadiologicalUS_FlexionExtention", "RadiologicalF_Normal", "RadiologicalF_DiscHernitation",
             "RadiologicalF_CentralSpinalCord", "RadiologicalF_RecesStenosis", "RadiologicalF_Spondylolisthesis2018",
             "RadiologicalF_Spondylolisthesis", "RadiologicalF_Scoliosis", "RadiologicalF_Scoliosis_Subcategory",
             "RadiologicalF_Modicchanges", "RadiologicalF_Modicchanges1", "RadiologicalF_Modicchanges2",
             "RadiologicalF_Modicchanges3", "RadiologicalF_ModicchangesUnspecified", "RadiologicalF_Other",
             "OtherSupplementaryDiagnostic_DiagnosticInjection", "OtherSupplementaryDiagnostic_Radikulgraphy",
             "RadiologicalUS_DiagnosticBlock", "OtherSupplementaryDiagnostic_Emg", "OtherSupplementaryDiagnostic_Nevrografi")
legeskjema <- legeskjema[, -which(names(legeskjema) %in% ikkemed)]

flere_hovedskjemaGuid_pre <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
flere_hovedskjemaGuid_post <- names(table(pasientsvar_post$HovedskjemaGUID))[table(pasientsvar_post$HovedskjemaGUID)>1]

write.csv2(legeskjema, "I:/nnrr/skjema1b_03122020.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(pasientsvar_pre, "I:/nnrr/skjema1a_03122020.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(pasientsvar_post, "I:/nnrr/skjema2_03122020.csv", row.names = F, fileEncoding = "Latin1")

########### Utlevering Karin Abeler 17.03.2020 ########################

pasientsvar_pre <- read.table('I:/nnrr/DataDump_Prod_1a_Spørreskjema+før+behandling_2019-06-11_red.csv', sep=';', header=T, stringsAsFactors = F)
legeskjema <- read.table('I:/nnrr/DataDump_Prod_1b_Registreringsskjema+poliklinikk_2019-06-11.csv', sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_post <- read.table('I:/nnrr/DataDump_Prod_2_Spørreskjema+etter+behandling_2019-06-11.csv', sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

flere_hovedskjemaGuid <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
names(table(pasientsvar_post$HovedskjemaGUID))[table(pasientsvar_post$HovedskjemaGUID)>1]
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
RegData <- RegData[RegData$Besoksdato >= '2015-01-01' & RegData$Besoksdato <= '2016-11-01', ]
RegData <- RegData[RegData$SykehusNavn == 'UNN', ]
write.csv2(RegData, 'I:/nnrr/utforskdata.csv', row.names = F)

RegData <- RegData[ , c("Besoksdato", "PatientAge", "PatientGender", "EducationLevel",
                        "DiagnosticNumber1", "DiagnosticNumber1Name", "DiagnosticNumber2", "DiagnosticNumber2Name",
                        "DiagnosticNumber3", "DiagnosticNumber3Name", "PainExperiencesActivity", "PainExperiencesNoActivity",
                        "HSCL10.Score")]

write.csv2(RegData, 'I:/nnrr/utlevering17032020.csv', row.names = F)

########### Utlevering Kjetil Samuelsen 08.01.2020  #########################################

skjema1a <- read.table('I:/nnrr/DataDump_Prod_1a_Spørreskjema+før+behandling_2020-01-07.csv', sep=';',
                       header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
skjema1b <- read.table('I:/nnrr/DataDump_Prod_1b_Registreringsskjema+poliklinikk_2020-01-07.csv', sep=';',
                       header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
skjema1a <- skjema1a[skjema1a$UnitId == 601032, ]
skjema1b <- skjema1b[skjema1b$UnitId == 601032, ]

var1a <- read.table('C:/GIT/nnrr/doc/Variabler 1A.csv', sep=';',
                    header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
var1b <- read.table('C:/GIT/nnrr/doc/Variabler 1B.csv', sep=';',
                    header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
var1b <- var1b[-(1:3),]

utlevering <- merge(skjema1a[, c("HovedskjemaGUID", var1a$Skjema.1A)], skjema1b[, c("SkjemaGUID", var1b)],
                    by.x = "HovedskjemaGUID", by.y = "SkjemaGUID", all.x = T)
utlevering <- utlevering[,-1]

write.csv2(utlevering, 'I:/nnrr/utlevering_k_samuelsen08012020.csv', row.names = F, na = '')




########### Utlevering Mari Tyrdal 13.05.2019 ####################################################
kodebok1a <- read.table('I:/nnrr/Kodebok1a_feb2019.csv', sep=';', header=T, stringsAsFactors=F)
kodebok1b <- read.table('I:/nnrr/Kodebok1b_feb2019.csv', sep=';', header=T, stringsAsFactors=F)

variabler <- read.table('C:/GIT/nnrr/doc/NNRR_variabelliste_utlevering_tyrdal.csv', sep=';', header=T, stringsAsFactors=F)
skjema1a <- read.table('I:/nnrr/DataDump_Prod_1a_Spørreskjema+før+behandling_2019-05-14_red.csv', sep=';',
                       header=T, stringsAsFactors = F)
skjema1b <- read.table('I:/nnrr/DataDump_Prod_1b_Registreringsskjema+poliklinikk_2019-05-14.csv', sep=';',
                       header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
flere_hovedskjemaGuid <- names(table(skjema1a$HovedskjemaGUID))[table(skjema1a$HovedskjemaGUID)>1]
skjema1a <- skjema1a[!(skjema1a$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]

var1a <- c(intersect(names(skjema1a), variabler$Skjema.1a), 'FamilyStatus', 'PatientGender', 'PainDisable', 'ProfessionWantedBack')
var1b <- intersect(variabler$Skjema.1b, names(skjema1b))
var1b <- var1b[!(var1b %in% intersect(var1a, var1b))]
var1a <- c(var1a, 'HovedskjemaGUID')
var1b <- c(var1b, 'SkjemaGUID', 'S1b_DateOfCompletion')

skjema1a <- skjema1a[, var1a]
skjema1b <- skjema1b[, var1b]
skjema1b$S1b_DateOfCompletion <- as.Date(skjema1b$S1b_DateOfCompletion, format="%d.%m.%Y")
skjema1b <- skjema1b[skjema1b$S1b_DateOfCompletion <= '2018-12-31', ]

regdata <- merge(skjema1a, skjema1b, by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')
regdata <- regdata[, !(names(regdata) %in% c('HovedskjemaGUID', 'S1b_DateOfCompletion'))]

write.csv2(regdata, 'utleveringMari14052019.csv', row.names = F, na = '')

########### Utlevering Jorunn 18.022.2019 #################################################

pasientsvar_pre <- read.table('I:/nnrr/DataDump_Prod_1a_Spørreskjema+før+behandling_2019-01-30_red.csv', sep=';',
                              header=T, stringsAsFactors = F)
legeskjema <- read.table('I:/nnrr/DataDump_Prod_1b_Registreringsskjema+poliklinikk_2019-01-30.csv', sep=';',
                         header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
legeskjema <- legeskjema[which(legeskjema$UnitId == 102959), ]
legeskjema$S1b_DateOfCompletion <- as.Date(legeskjema$S1b_DateOfCompletion, format="%d.%m.%Y")
legeskjema <- legeskjema[which(as.numeric(format(legeskjema$S1b_DateOfCompletion, '%Y')) %in% 2016:2017), ]
legeskjema <- legeskjema[legeskjema$SickLeave == 'True' & !is.na(legeskjema$SickLeave), ]
pasientsvar_post <- read.table('I:/nnrr/DataDump_Prod_2_Spørreskjema+etter+behandling_2019-01-30.csv', sep=';',
                               header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
utlever1a <- read.table(system.file(file='extdata/datadumpnavn Jorunn NNRR skjema 1a.csv', package='nnrr'), sep=';',
                        header=F, stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
utlever1b <- read.table(system.file(file='extdata/datadumpnavn Jorunn NNRR skjema 1b.csv', package='nnrr'), sep=';',
                        header=F, stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
utlever2 <- read.table(system.file(file='extdata/datadumpnavn Jorunn NNRR skjema 2.csv', package='nnrr'), sep=';',
                       header=F, stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

pasientsvar_pre <- pasientsvar_pre[, c("HovedskjemaGUID", utlever1a$V1)]
legeskjema <- legeskjema[, c("SkjemaGUID", utlever1b$V1)]
pasientsvar_post <- pasientsvar_post[, c("HovedskjemaGUID", utlever2$V1)]

RegData <- merge(legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'))
RegData <- merge(RegData, pasientsvar_post, by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', suffixes = c('', '_post'))

write.csv2(RegData, 'utleveringJorunn1802019.csv', row.names = F, na = '')

# max.len<-max(length(names(pasientsvar_pre)),length(names(legeskjema)), length(names(pasientsvar_post)))
#
# varnavn <- data.frame(skjema1a = c(names(pasientsvar_pre), rep(NA, max.len - length(names(pasientsvar_pre)))),
#            skjema1b = c(names(legeskjema), rep(NA, max.len - length(names(legeskjema)))),
#            skjema2 = c(names(pasientsvar_post), rep(NA, max.len - length(names(pasientsvar_post)))))
# write.csv2(varnavn, 'datadumpnavn.csv', row.names = F, na = '')


# rm(list = c('pasientsvar_pre', 'legeskjema', 'pasientsvar_post'))
