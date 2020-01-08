rm(list=ls())
library(nnrr)

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
