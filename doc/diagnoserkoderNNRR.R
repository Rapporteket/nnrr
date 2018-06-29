setwd('c:/GIT/nnrr/doc/')
rm(list=ls())
library(tidyverse)

# NNRR <- read.table('c:/SVN/jasper/nnrr/data/NNRRdiagnoser1feb.csv', sep=';', header=T, stringsAsFactors=F)
# NNRR <- read.table('c:/SVN/jasper/nnrr/data/DataDump_1b%3aRegistreringsskjema+poliklinikk_2017-02-01.csv', sep=';',
#                    header=T, stringsAsFactors=F, fileEncoding = 'UTF-8-BOM')
NNRR <- read.table('I:/nnrr/DataDump_1b%3aRegistreringsskjema+poliklinikk_2017-09-29.csv', sep=';',
                   header=T, stringsAsFactors=F, fileEncoding = 'UTF-8-BOM')
# NNRR <- read.table('P:/MinData/nnrr/DataDump_1b%3aRegistreringsskjema+poliklinikk_2017-09-29.csv', sep=';',
#                    header=T, stringsAsFactors=F, fileEncoding = 'UTF-8-BOM')
NNRR$S1b_DateOfCompletion <- as.POSIXct(NNRR$S1b_DateOfCompletion, format="%d.%m.%Y")
# Se bare på registreringer f.o.m. jan. 2016
NNRR <- NNRR[!is.na(NNRR$S1b_DateOfCompletion), ]
NNRR <- NNRR[NNRR$S1b_DateOfCompletion >= '2016-01-01' & NNRR$S1b_DateOfCompletion <= '2016-12-31', ]
NNRR <- NNRR[which(NNRR$ReshId != 109834), ]

NNRR$Diagnose1 <- NNRR$DiagnosticNumber1
# Kun store bokstaver
NNRR$Diagnose1 <- toupper(NNRR$Diagnose1)
# Fjern alle mellomrom
NNRR$Diagnose1 <- gsub(' ', '', NNRR$Diagnose1)
# sett inn M der det mangler ledende bokstav
indManglerM <- which(!is.na(as.numeric(substr(NNRR$Diagnose1, 1, 1))))
NNRR$Diagnose1[indManglerM] <- paste0('M', NNRR$Diagnose1[indManglerM])
# Erstatt 'I' og 'L' med 1 og O med 0.
NNRR$Diagnose1 <- gsub('I', '1', NNRR$Diagnose1)
NNRR$Diagnose1 <- gsub('L', '1', NNRR$Diagnose1)
NNRR$Diagnose1 <- gsub('O', '0', NNRR$Diagnose1)
# Fjern registreringer uten diagnose
Manglende <- NNRR[NNRR$Diagnose1 %in% c('', '.'), ]
NNRR <- NNRR[NNRR$Diagnose1 != '', ]
NNRR <- NNRR[NNRR$Diagnose1 != '.', ]
# Fjern punktum
NNRR$Diagnose1 <- gsub('\\.', '', NNRR$Diagnose1)
# Fjern endringene for ikke-koder
NNRR$Diagnose1[which(is.na(as.numeric(stringi::stri_sub(NNRR$Diagnose1,2))))] <-
  NNRR$DiagnosticNumber1[which(is.na(as.numeric(stringi::stri_sub(NNRR$Diagnose1,2))))]
NNRR$Diagnose1Formatert <- NNRR$Diagnose1

oversikt <- NNRR[, c("ReshId", "S1b_DateOfCompletion", "PasientGUID", "SkjemaGUID", "DiagnosticNumber1", "Diagnose1Formatert", "DiagnosticNumber1Name")]
oversikt <- oversikt[order(oversikt$Diagnose1Formatert), ]
# oversikt <- as_tibble(oversikt)

write.csv2(oversikt, 'DiagnosekoderNNRR.csv', row.names = F)

tabell1 <- as.data.frame(sort(table(NNRR$Diagnose1Formatert), decreasing = T))
names(tabell1) <- c('Diagnose1Formatert', 'Antall')

write.csv2(tabell1, 'SortertTabellDiagnosekoderNNRR.csv', row.names = F)


tabell2 <- as.data.frame(sort(table(NNRR$DiagnosticNumber1), decreasing = T))
names(tabell2) <- c('DiagnosticNumber1', 'Antall')

write.csv2(tabell2, 'SortertTabellDiagnosekoderNNRRuformatert.csv', row.names = F)

Manglende <- Manglende[, c("ReshId", "S1b_DateOfCompletion", "PasientGUID", "SkjemaGUID")]
Manglende <- arrange(Manglende, ReshId, S1b_DateOfCompletion)

SykehusNavn <- c('Helse Bergen', 'St Olavs Hospital', 'OUS', 'UNN')

Manglende$Shus <- NA
Manglende$Shus[Manglende$ReshId == 102959] <- 'Helse Bergen'
Manglende$Shus[Manglende$ReshId == 104293] <- 'St Olavs'
Manglende$Shus[Manglende$ReshId == 109834] <- 'OUS'
Manglende$Shus[Manglende$ReshId == 601032] <- 'UNN'

Manglende <- Manglende[, c(1,5,2,3,4)]
write.csv2(Manglende, 'ManglendeNNRRdiag.csv', row.names = F)

table(Manglende$ReshId, useNA = 'ifany')

sort(tapply(Manglende$ReshId, Manglende$PasientGUID, length), decreasing = T)



