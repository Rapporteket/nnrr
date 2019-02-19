rm(list=ls())
library(nnrr)

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
