setwd('c:/GIT/nnrr/doc/')
rm(list=ls())
library(nnrr)

datoFra <- '2014-01-01'
datoTil <- '2050-01-01'

skjema1a <- read.table('c:/SVN/jasper/nnrr/data/DataDump_1a%3aSpørreskjema+før+behandling_2016-12-16.csv', sep=';', header=T, dec=",", fileEncoding = 'UTF-8-BOM')
skjema1b <- read.table('c:/SVN/jasper/nnrr/data/DataDump_1b%3aRegistreringsskjema+poliklinikk_2016-12-16.csv', sep=';', header=T, dec=",", fileEncoding = 'UTF-8-BOM')
skjema2 <- read.table('c:/SVN/jasper/nnrr/data/DataDump_2%3aSpørreskjema+etter+behandling_2016-12-16.csv', sep=';', header=T, dec=",", fileEncoding = 'UTF-8-BOM')

# skjema1b <- read.table('c:/SVN/jasper/nnrr/data/DataDump_1b%3aRegistreringsskjema+poliklinikk_2016-12-12.csv', sep=';', header=T, dec=",", fileEncoding = 'UTF-8-BOM')

NA_oppsummert <- sort(apply(skjema1b, 2, function(x){length(which(is.na(x)))}), decreasing = T)
skjema1b$Hoveddato <- as.POSIXlt(skjema1b$S1b_DateOfCompletion, format="%d.%m.%Y")
skjema1b <- skjema1b[which(skjema1b$Hoveddato >= datoFra & skjema1b$Hoveddato <= datoTil), ]
skjema1b <- skjema1b[order(skjema1b$Hoveddato), ]
skjema1a$Hoveddato <- as.POSIXlt(skjema1a$DateOfCompletion, format="%d.%m.%Y")
skjema1a <- skjema1a[which(skjema1a$Hoveddato >= datoFra & skjema1a$Hoveddato <= datoTil), ]
skjema1a <- skjema1a[order(skjema1a$Hoveddato), ]
skjema2$Hoveddato <- as.POSIXlt(skjema2$DateOfCompletion, format="%d.%m.%Y")
skjema2 <- skjema2[which(skjema2$Hoveddato >= datoFra & skjema2$Hoveddato <= datoTil), ]
skjema2 <- skjema2[order(skjema2$Hoveddato), ]




varnavn_1a <- read.table('c:/GIT/nnrr/doc/kodebok16122016_1a.csv', sep=';', header=T)
varnavn_1b <- read.table('c:/GIT/nnrr/doc/kodebok16122016_1b.csv', sep=';', header=T)
varnavn_2 <- read.table('c:/GIT/nnrr/doc/kodebok16122016_2.csv', sep=';', header=T)
boolske_var1a <- as.character(varnavn_1a$DataDumpnavn)[which(as.character(varnavn_1a$Felttype) == 'Avkrysning')]
ikkeoblig_var_1a <- as.character(varnavn_1a$DataDumpnavn)[which(as.character(varnavn_1a$Obligatorisk) == 'Nei')]
skjema1a[, boolske_var1a] <- apply(skjema1a[, boolske_var1a], 2, as.logical)
boolske_var1b <- as.character(varnavn_1b$DataDumpnavn)[which(as.character(varnavn_1b$Felttype) == 'Avkrysning')]
ikkeoblig_var_1b <- as.character(varnavn_1b$DataDumpnavn)[which(as.character(varnavn_1b$Obligatorisk) == 'Nei')]
skjema1b[, boolske_var1b] <- apply(skjema1b[, boolske_var1b], 2, as.logical)
boolske_var2 <- as.character(varnavn_2$DataDumpnavn)[which(as.character(varnavn_2$Felttype) == 'Avkrysning')]
ikkeoblig_var_2 <- as.character(varnavn_2$DataDumpnavn)[which(as.character(varnavn_2$Obligatorisk) == 'Nei')]
skjema2[, boolske_var2] <- apply(skjema2[, boolske_var2], 2, as.logical)

numvar1a <-  as.character(varnavn_1a$DataDumpnavn)[which(substr(as.character(varnavn_1a$Felttype), 1, 8) == 'Numerisk')]
numvar1b <-  as.character(varnavn_1b$DataDumpnavn)[which(substr(as.character(varnavn_1b$Felttype), 1, 8) == 'Numerisk')]
numvar2 <-  as.character(varnavn_2$DataDumpnavn)[which(substr(as.character(varnavn_2$Felttype), 1, 8) == 'Numerisk')]
head(skjema1a[, numvar1a])
head(skjema1b[, numvar1b])
head(skjema2[, numvar2])

varnavn_1a <- varnavn_1a$DataDumpnavn[varnavn_1a$DataDumpnavn != '']
varnavn_1b <- varnavn_1b$DataDumpnavn[varnavn_1b$DataDumpnavn != '']
varnavn_2 <- varnavn_2$DataDumpnavn[varnavn_2$DataDumpnavn != '']

setdiff(as.character(varnavn_1a), names(skjema1a))
setdiff(names(skjema1a), as.character(varnavn_1a))
setdiff(as.character(varnavn_1b), names(skjema1b))
setdiff(names(skjema1b), as.character(varnavn_1b))
setdiff(as.character(varnavn_2), names(skjema2))
setdiff(names(skjema2), as.character(varnavn_2))


intersect(intersect(setdiff(names(skjema1a), as.character(varnavn_1a)), setdiff(names(skjema1b), as.character(varnavn_1b))), setdiff(names(skjema2), as.character(varnavn_2)))
intersect(setdiff(names(skjema1a), as.character(varnavn_1a)), setdiff(names(skjema2), as.character(varnavn_2)))
intersect(setdiff(names(skjema1b), as.character(varnavn_1b)), setdiff(names(skjema2), as.character(varnavn_2)))

##### Oppsummering skjema 1b ######################################

table(skjema1a$DateOfCompletion, useNA = 'ifany')
table(skjema1b$DiagICD1, useNA = 'ifany')
table(skjema1b$DiagICD2, useNA = 'ifany')
table(skjema1b$DiagICD3, useNA = 'ifany')
table(skjema1b$DiagName1, useNA = 'ifany')
table(skjema1b$DiagName2, useNA = 'ifany')
table(skjema1b$DiagName3, useNA = 'ifany')
# table(skjema1b$DiagnosticNumber1Name, skjema1b$DiagnosticNumber1, useNA = 'ifany')


table(skjema1a$NationalCountry, useNA = 'ifany')
table(skjema1a$FamilyStatus, useNA = 'ifany')
table(skjema1a$NationalInterpreter, useNA = 'ifany')
table(skjema1b$Working, useNA = 'ifany')

# Ingen avkrysninger på "Arbeidsstatus"
length(which(apply(skjema1b[, c('Working', 'Unemployed', 'SickLeave', 'Stay_at_home', 'Student', 'NAV', 'RetirementPension', 'Pension')], 1, sum) == 0))

# Ingen avkrysninger på "Første konsultasjon på poliklinikk"
length(which(apply(skjema1b[, c('FirstMedExamDoctor', 'FirstMedExamNurse', 'FirstMedExamPhys', 'FirstMedExamOther')], 1, sum) == 0))

# Bildediagnostikk
length(which(apply(skjema1b[, c('Radiological_None', 'RadiologicalUS_CT', 'RadiologicalUS_MR', 'RadiologicalUS_Radikulgraphy', 'RadiologicalUS_Discography',
                                'RadiologicalUS_LS_C_Columna', 'RadiologicalUS_FlexionExtention')], 1, sum) == 0))

########### skjema 1a ############################


# OSWESTRY
tmp <- aggregate(skjema1a[, c('Pain', 'PersonalCare', 'Lift', 'Walk', 'Sit',
                        'Stand', 'Sleep', 'Sex', 'Social', 'Travel')], by = list(SkjemaGUID = skjema1a$SkjemaGUID), function(x){x!=0})
tmp <- merge(tmp, skjema1a[, c('SkjemaGUID', 'ODI.Score')], by = 'SkjemaGUID')

UtfyllingsgradODI <- apply(tmp[, c('Pain', 'PersonalCare', 'Lift', 'Walk', 'Sit',
                   'Stand', 'Sleep', 'Sex', 'Social', 'Travel')], 1, sum)


UtfyllingsgradODI <- data.frame(UtfyllingsgradODI, ODI.Score=tmp$ODI.Score)

aux <- UtfyllingsgradODI[is.na(UtfyllingsgradODI$ODI.Score), ]

table(UtfyllingsgradODI$UtfyllingsgradODI, useNA = 'ifany')

########### skjema 2 ############################

## ODI
tmp <- aggregate(skjema2[, c('Pain', 'PersonalCare', 'Lift', 'Walk', 'Sit',
                              'Stand', 'Sleep', 'Sex', 'Social', 'Travel')], by = list(SkjemaGUID = skjema2$SkjemaGUID), function(x){x!=0})

tmp <- merge(tmp, skjema2[, c('SkjemaGUID', 'ODI.Score')], by = 'SkjemaGUID')

UtfyllingsgradODI <- apply(tmp[, c('Pain', 'PersonalCare', 'Lift', 'Walk', 'Sit',
                                   'Stand', 'Sleep', 'Sex', 'Social', 'Travel')], 1, sum)


UtfyllingsgradODI <- data.frame(UtfyllingsgradODI, ODI.Score=tmp$ODI.Score)

aux <- UtfyllingsgradODI[is.na(UtfyllingsgradODI$ODI.Score), ]
table(UtfyllingsgradODI$UtfyllingsgradODI, useNA = 'ifany')


## NDI

'Intensity'
'PersonalCare'
'Lift'
'Read'
'Headache'
'Concentration'
'Work'
'Driving'
'Sleep'
'Leisure'

# Finn duplikat varnavn

varnavn <- varnavn_2$DataDumpnavn[varnavn_2$DataDumpnavn != '']

duplikater <- sort(table(varnavn), decreasing = T)
duplikater <-duplikater[duplikater>1]
duplikater


varnavn <- varnavn_1a$DataDumpnavn[varnavn_1a$DataDumpnavn != '']
length(varnavn)
duplikater <- sort(table(varnavn), decreasing = T)
duplikater <-duplikater[duplikater>1]
duplikater
length(duplikater)

varnavn <- varnavn_1b$DataDumpnavn[varnavn_1b$DataDumpnavn != '']

duplikater <- sort(table(varnavn), decreasing = T)
duplikater <-duplikater[duplikater>1]
duplikater

