rm(list=ls())
library(nnrr)
legeskjema <- read.table('P:/MinData/nnrr/DataDump_1b%3aRegistreringsskjema+poliklinikk_2017-09-27.csv', sep=';',
                         header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_pre <- read.table('P:/MinData/nnrr/DataDump_1a%3aSpørreskjema+før+behandling_2017-09-22.csv', sep=';',
                              header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
pasientsvar_pre2 <- read.table('P:/MinData/nnrr/DataDump_1a%3aSpørreskjema+før+behandling_2017-09-27.csv', sep=';',
                              header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
# # pasientsvar_pre3 <- read.table('P:/MinData/nnrr/DataDump_1a%3aSpørreskjema+før+behandling_2017-09-27_2.csv', sep=';',
#                               header=T, stringsAsFactors = F)

pasientsvar_post <- read.table('P:/MinData/nnrr/DataDump_2%3aSpørreskjema+etter+behandling_2017-09-27.csv', sep=';',
                               header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

varnavn_1b <- read.table('P:/MinData/nnrr/Kodebok1b.csv', sep=';', header=T, stringsAsFactors = F)
boolske_var1b <- as.character(varnavn_1b$DataDumpnavn)[which(as.character(varnavn_1b$Felttype) == 'Avkrysning')]
legeskjema[, boolske_var1b] <- apply(legeskjema[, boolske_var1b], 2, as.logical)
varnavn_1a <- read.table('P:/MinData/nnrr/Kodebok1a.csv', sep=';', header=T, stringsAsFactors = F)
boolske_var1a <- as.character(varnavn_1a$DataDumpnavn)[which(as.character(varnavn_1a$Felttype) == 'Avkrysning')]
pasientsvar_pre[, boolske_var1a] <- apply(pasientsvar_pre[, boolske_var1a], 2, as.logical)
varnavn_2 <- read.table('P:/MinData/nnrr/Kodebok2.csv', sep=';', header=T, stringsAsFactors = F)
boolske_var2 <- as.character(varnavn_2$DataDumpnavn)[which(as.character(varnavn_2$Felttype) == 'Avkrysning')]
pasientsvar_post[, boolske_var2] <- apply(pasientsvar_post[, boolske_var2], 2, as.logical)

legeskjema$regstatus <- 1
pasientsvar_pre$regstatus <- 1
pasientsvar_post$regstatus <- 1

names(pasientsvar_pre)[names(pasientsvar_pre)=='SkjemaGUID'] <- 'SkjemaGUID_pre'
names(pasientsvar_post)[names(pasientsvar_post)=='SkjemaGUID'] <- 'SkjemaGUID_post'
names(legeskjema)[names(legeskjema)=='S1b_DateOfCompletion'] <- 'Besoksdato'
names(pasientsvar_pre)[names(pasientsvar_pre)=='DateOfCompletion'] <- 'Besoksdato'
names(pasientsvar_post)[names(pasientsvar_post)=='DateOfCompletion'] <- 'Besoksdato'

RegData <- merge(legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'), all = TRUE)
RegData <- merge(RegData, pasientsvar_post, by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', suffixes = c('', '_post'), all = TRUE)

RegData <- nnrrPreprosess(RegData = RegData)
RegData$Aar_pre <- RegData$Besoksdato_pre$year+1900
RegData$Aar_post <- RegData$Besoksdato_post$year+1900

legeskjema2016 <- RegData[which(RegData$Aar == 2016 & RegData$regstatus==1), ]
pasientsvar_pre2016 <- RegData[which(RegData$Aar_pre == 2016 & RegData$regstatus_pre==1), ]
pasientsvar_post2016 <- RegData[which(RegData$Aar_post == 2016 & RegData$regstatus_post==1), ]


# Antall basert på besøksdato 2016 i klinikerskjema
addmargins(table(legeskjema2016$Besoksdato$mon+1, legeskjema2016$SykehusNavn, useNA = 'ifany'))
addmargins(table(legeskjema2016$Besoksdato$mon[legeskjema2016$regstatus_pre==1]+1,
                 legeskjema2016$SykehusNavn[legeskjema2016$regstatus_pre==1], useNA = 'ifany'))
addmargins(table(legeskjema2016$Besoksdato$mon[legeskjema2016$regstatus_post==1]+1,
                 legeskjema2016$SykehusNavn[legeskjema2016$regstatus_post==1], useNA = 'ifany'))

# legeskjema2016$guid



# Pasient pre besøksdato 2016
addmargins(table(pasientsvar_pre2016$Besoksdato_pre$mon+1, pasientsvar_pre2016$SykehusNavn, useNA = 'ifany'))
# Pasient post besøksdato 2016
addmargins(table(pasientsvar_post2016$Besoksdato_post$mon+1, pasientsvar_post2016$SykehusNavn, useNA = 'ifany'))












legeskjema$Besoksdato <- as.POSIXlt(legeskjema$Besoksdato, format="%d.%m.%Y")
pasientsvar_pre$Besoksdato <- as.POSIXlt(pasientsvar_pre$Besoksdato, format="%d.%m.%Y")
pasientsvar_post$Besoksdato <- as.POSIXlt(pasientsvar_post$Besoksdato, format="%d.%m.%Y")
legeskjema$Aar <- legeskjema$Besoksdato$year+1900
pasientsvar_pre$Aar <- pasientsvar_pre$Besoksdato$year+1900
pasientsvar_post$Aar <- pasientsvar_post$Besoksdato$year+1900
legeskjema$SykehusNavn <- NA
legeskjema$SykehusNavn[legeskjema$ReshId == 102959] <- 'Haukeland'
legeskjema$SykehusNavn[legeskjema$ReshId == 104293] <- 'St. Olavs'
legeskjema$SykehusNavn[legeskjema$ReshId == 109834] <- 'OUS'
legeskjema$SykehusNavn[legeskjema$ReshId == 601032] <- 'UNN'


legeskjema2016 <- legeskjema[legeskjema$Aar==2016, ]
table(legeskjema2016$SykehusNavn)














