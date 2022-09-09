#' Provide global dataframe for NNRR
#'
#' Provides NNRR data from staging
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return RegData data frame
#' @export

nnrrHentRegData <- function(datoFra = '2017-01-01', datoTil = '2099-01-01') {

  registryName <- "nnrr"
  dbType <- "mysql"

  # query <- paste0("SELECT
  #                 FROM AlleVariablerNum INNER JOIN ForlopsOversikt
  #                 ON AlleVariablerNum.ForlopsID = ForlopsOversikt.ForlopsID
  #                 WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")
  # RegData <- rapbase::LoadRegData(registryName, query, dbType)

  pasientsvar_pre <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Pasientskjema+før+behandling_2022-07-01_1313_redigert.csv',
                                sep=';', header=T, stringsAsFactors = F, fileEncoding = "Latin1")
  legeskjema <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-07-01_1313.csv', sep=';',
                           header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  pasientsvar_post <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Pasientskjema+6+måneder+etter+behandling_2022-07-01_1313.csv',
                                 sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  pasientsvar_post2 <- read.table('~/.ssh/data/nnrr/DataDump_MRS-PROD_Pasientskjema+12+måneder+etter+behandling_2022-07-01_1313.csv',
                                  sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

  flere_hovedskjemaGuid <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
  pasientsvar_pre <- pasientsvar_pre[!(pasientsvar_pre$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]

  icd10 <- read.table('~/.ssh/data/nnrr/icd10.csv', sep=';', header=T, stringsAsFactors = F, fileEncoding = 'UTF-8')
  legeskjema$regstatus <- 1
  pasientsvar_pre$regstatus <- 1
  pasientsvar_post$regstatus <- 1
  pasientsvar_post2$regstatus <- 1

  names(pasientsvar_pre)[names(pasientsvar_pre)=='SkjemaGUID'] <- 'SkjemaGUID_pre'
  names(pasientsvar_post)[names(pasientsvar_post)=='SkjemaGUID'] <- 'SkjemaGUID_post'
  names(pasientsvar_post2)[names(pasientsvar_post2)=='SkjemaGUID'] <- 'SkjemaGUID_post2'

  RegData <- merge(legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'), all.x = TRUE)
  RegData <- merge(RegData, pasientsvar_post, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_post'), all.x = TRUE)
  RegData <- merge(RegData, pasientsvar_post2, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_post2'), all.x = TRUE)

  RegData$DiagnosticNumber1 <- trimws(RegData$DiagnosticNumber1)
  RegData <- nnrr::nnrrPreprosess(RegData = RegData)
  rm(list = c('pasientsvar_pre', 'legeskjema', 'pasientsvar_post', 'pasientsvar_post2'))

  return(RegData)
}
