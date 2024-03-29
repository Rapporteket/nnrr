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

  # pasientsvar_pre <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+for+behandling_2022-12-09_1116_red.csv',
  #                               sep=';', header=T, stringsAsFactors = F, fileEncoding = "UTF-8-BOM")
  pasientsvar_pre <- readr::read_csv2('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+for+behandling_2022-12-09_1116_v2.csv')
  legeskjema <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Behandlerskjema_2022-12-09_1116.csv', sep=';',
                           header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  pasientsvar_post <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+6+maneder+etter+behandling_2022-12-09_1116.csv',
                                 sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  pasientsvar_post2 <- read.table('C:/GIT/data/nnrr/DataDump_MRS-PROD_Pasientskjema+12+maneder+etter+behandling_2022-12-09_1116.csv',
                                  sep=';', header=T, fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

  flere_hovedskjemaGuid <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
  if (!is.null(flere_hovedskjemaGuid)){
    pasientsvar_pre <- pasientsvar_pre[!(pasientsvar_pre$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]
  }

  # icd10 <- read.table('C:/GIT/data/nnrr/icd10.csv', sep=';', header=T, stringsAsFactors = F, fileEncoding = 'UTF-8')
  legeskjema$regstatus <- 1
  pasientsvar_pre$regstatus <- 1
  pasientsvar_post$regstatus <- 1
  pasientsvar_post2$regstatus <- 1

  names(pasientsvar_pre)[names(pasientsvar_pre)=='SkjemaGUID'] <- 'SkjemaGUID_pre'
  names(pasientsvar_post)[names(pasientsvar_post)=='SkjemaGUID'] <- 'SkjemaGUID_post'
  names(pasientsvar_post2)[names(pasientsvar_post2)=='SkjemaGUID'] <- 'SkjemaGUID_post2'

  RegData <- merge(legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'),
                   all.x = TRUE)
  RegData <- merge(RegData, pasientsvar_post, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_post'),
                   all.x = TRUE)
  RegData <- merge(RegData, pasientsvar_post2, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_post2'),
                   all.x = TRUE)

  RegData$DiagnosticNumber1 <- trimws(RegData$DiagnosticNumber1)
  RegData <- nnrr::nnrrPreprosess(RegData = RegData)
  rm(list = c('pasientsvar_pre', 'legeskjema', 'pasientsvar_post', 'pasientsvar_post2'))

  return(RegData)
}
