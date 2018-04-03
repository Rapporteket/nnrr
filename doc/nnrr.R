setwd('c:/GIT/nnrr/doc/')
rm(list=ls())
library(nnrr)

datoFra <- '2015-01-01'
datoTil <- '2050-01-01'

skjema1a <- read.table('P:/MinData/nnrr/DataDump_1a%3aSpørreskjema+før+behandling_2017-05-24.csv', sep=';',
                       header=T, stringsAsFactors=F, fileEncoding = 'UTF-8-BOM')
skjema1b <- read.table('P:/MinData/nnrr/DataDump_1b%3aRegistreringsskjema+poliklinikk_2017-05-24.csv', sep=';',
                   header=T, stringsAsFactors=F, fileEncoding = 'UTF-8-BOM')
skjema2 <- read.table('P:/MinData/nnrr/DataDump_2%3aSpørreskjema+etter+behandling_2017-05-24.csv', sep=';',
                       header=T, stringsAsFactors=F, fileEncoding = 'UTF-8-BOM')

kodebok1a <- read.table('P:/MinData/nnrr/Kodebok1a.csv', sep=';', header=T, stringsAsFactors=F)
kodebok1b <- read.table('P:/MinData/nnrr/Kodebok1b.csv', sep=';', header=T, stringsAsFactors=F)
kodebok2 <- read.table('P:/MinData/nnrr/Kodebok2.csv', sep=';', header=T, stringsAsFactors=F)

boolske_var1a <- as.character(kodebok1a$DataDumpnavn)[which(as.character(kodebok1a$Felttype) == 'Avkrysning')]
enum_var1a <- as.character(kodebok1a$DataDumpnavn)[which(as.character(kodebok1a$Felttype) == 'Enum')]
boolske_var1b <- as.character(kodebok1b$DataDumpnavn)[which(as.character(kodebok1b$Felttype) == 'Avkrysning')]
enum_var1b <- as.character(kodebok1b$DataDumpnavn)[which(as.character(kodebok1b$Felttype) == 'Enum')]
boolske_var2 <- as.character(kodebok2$DataDumpnavn)[which(as.character(kodebok2$Felttype) == 'Avkrysning')]
enum_var2 <- as.character(kodebok2$DataDumpnavn)[which(as.character(kodebok2$Felttype) == 'Enum')]

skjema1a[, boolske_var1a] <- apply(skjema1a[, boolske_var1a], 2, as.logical)
skjema1b[, boolske_var1b] <- apply(skjema1b[, boolske_var1b], 2, as.logical)
skjema2[, boolske_var2] <- apply(skjema2[, boolske_var2], 2, as.logical)

skjema1a[, enum_var1a] <- apply(skjema1a[, enum_var1a], 2, function(x){as.numeric(!(x %in% c(-1,0)))})
skjema1b[, enum_var1b] <- apply(skjema1b[, enum_var1b], 2, function(x){as.numeric(!(x %in% c(-1,0)))})
skjema2[, enum_var2] <- apply(skjema2[, enum_var2], 2, function(x){as.numeric(!(x %in% c(-1,0)))})

table(cut(rowSums(skjema1a[, enum_var1a]), breaks = c(0,10,20,30,40,50,60,70,80,90)))
table(cut(rowSums(skjema1b[, enum_var1b]), breaks = c(0, 5, 10, 15, 20)))
table(cut(rowSums(skjema2[, enum_var2]), breaks = c(0,10,20,30,40,50)))


ingen_avkrysninger1a <- which(rowSums(skjema1a[, boolske_var1a])==0)
ingen_enum1a <- which(rowSums(skjema1a[, enum_var1a])<=1)
ingen_avkrysninger1b <- which(rowSums(skjema1b[, boolske_var1b])==0)
ingen_enum1b <- which(rowSums(skjema1b[, enum_var1b])<=1)
ingen_avkrysninger2 <- which(rowSums(skjema2[, boolske_var2])==0)
ingen_enum2 <- which(rowSums(skjema2[, enum_var2])<=1)

tapply(skjema1b$SkjemaGUID[ingen_avkrysninger1b], skjema1b$ReshId[ingen_avkrysninger1b], length)

# tmp <- skjema1a[ingen_enum1a, enum_var1a]



overlapp_ab <- skjema1a[which(skjema1a$HovedskjemaGUID %in% skjema1b$SkjemaGUID), ]

tapply(skjema1b$SkjemaGUID, skjema1b$ReshId, length)


# write.csv2(Potensielt_tomme_reg1a, 'tomme_reg1a.csv', row.names = F)
# write.csv2(Potensielt_tomme_reg1b, 'tomme_reg1b.csv', row.names = F)
# write.csv2(Potensielt_tomme_reg2, 'tomme_reg2.csv', row.names = F)
#
# write.csv2(Potensielt_tomme_reg[, c('SkjemaGUID', 'PasientGUID', 'FormDate', 'LastUpdate')], 'tomme_reg1b.csv', row.names = F)

# Potensielt_tomme_reg1a <- skjema1a[ingen_avkrysninger1a, ikkeboolske_var1a]
# Potensielt_tomme_reg1b <- skjema1b[ingen_avkrysninger1b, ikkeboolske_var1b]
# Potensielt_tomme_reg2 <- skjema2[ingen_avkrysninger2, ikkeboolske_var2]


# ikkeboolske_var1a <- setdiff(names(skjema1a), boolske_var1a)
# ikkeboolske_var1b <- setdiff(names(skjema1b), boolske_var1b)
# ikkeboolske_var2 <- setdiff(names(skjema2), boolske_var2)


# setdiff(ikkeboolske_var1b, names(skjema1b))
# ikkeboolske_var1b <- ikkeboolske_var1b[-11]
# ikkeboolske_var1b <- as.character(kodebok1b$DataDumpnavn)[which(!(as.character(kodebok1b$Felttype) %in% c('Avkrysning', '')))]

# NA_oppsummert <- sort(apply(skjema1b, 2, function(x){length(which(is.na(x)))}), decreasing = T)
# NA_oppsummert <- NA_oppsummert[NA_oppsummert>0]

# skjema1b$Hoveddato <- as.POSIXlt(skjema1b$S1b_DateOfCompletion, format="%d.%m.%Y")
# skjema1b <- skjema1b[which(skjema1b$Hoveddato >= datoFra & skjema1b$Hoveddato <= datoTil), ]
# skjema1b <- skjema1b[order(skjema1b$Hoveddato), ]


# varnavn_1a <- read.table('c:/GIT/nnrr/doc/kodebok29112016_1a.csv', sep=';', header=T)
# varnavn_1b <- read.table('c:/GIT/nnrr/doc/kodebok29112016_1b.csv', sep=';', header=T)
# varnavn_2 <- read.table('c:/GIT/nnrr/doc/kodebok29112016_2.csv', sep=';', header=T)
# boolske_var1a <- as.character(varnavn_1a$Variabelnavn)[which(as.character(varnavn_1a$Felttype) == 'Avkrysning')]
# ikkeoblig_var_1a <- as.character(varnavn_1a$Variabelnavn)[which(as.character(varnavn_1a$Obligatorisk) == 'Nei')]
# skjema1a[, boolske_var1a] <- apply(skjema1a[, boolske_var1a], 2, as.logical)
# boolske_var1b <- as.character(varnavn_1b$Variabelnavn)[which(as.character(varnavn_1b$Felttype) == 'Avkrysning')]
# ikkeoblig_var_1b <- as.character(varnavn_1b$Variabelnavn)[which(as.character(varnavn_1b$Obligatorisk) == 'Nei')]
# skjema1b[, boolske_var1b] <- apply(skjema1b[, boolske_var1b], 2, as.logical)
# boolske_var2 <- as.character(varnavn_2$Variabelnavn)[which(as.character(varnavn_2$Felttype) == 'Avkrysning')]
# ikkeoblig_var_2 <- as.character(varnavn_2$Variabelnavn)[which(as.character(varnavn_2$Obligatorisk) == 'Nei')]
# skjema2[, boolske_var2] <- apply(skjema2[, boolske_var2], 2, as.logical)
