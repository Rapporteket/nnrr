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
  datasti <- if (rapbase::isRapContext()) {
    "~/mydata/nnrr/"
  } else {"~/softlinks/mydata/nnrr/"}

  legeskjema <-
    readr::read_csv2(
        paste0("~/mydata/nnrr/data_beh_2024-11-07_1551.csv"))
  pasientsvar_pre <-
    readr::read_csv2(
      paste0("~/mydata/nnrr/data_pre_2024-11-07_1554.csv"))
  pasientsvar_post <-
    readr::read_csv2(
      paste0("~/mydata/nnrr/data_6mnd_2024-11-07_1559.csv"))
  pasientsvar_post2 <-
    readr::read_csv2(
      paste0("~/mydata/nnrr/data_12mnd_2024-11-07_1602.csv"))

  flere_hovedskjemaGuid <- names(table(pasientsvar_pre$HovedskjemaGUID))[table(pasientsvar_pre$HovedskjemaGUID)>1]
  if (!is.null(flere_hovedskjemaGuid)){
    pasientsvar_pre <- pasientsvar_pre[!(pasientsvar_pre$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]
  }
  flere_hovedskjemaGuid <- names(table(pasientsvar_post$HovedskjemaGUID))[table(pasientsvar_post$HovedskjemaGUID)>1]
  if (!is.null(flere_hovedskjemaGuid)){
    pasientsvar_post <- pasientsvar_post[!(pasientsvar_post$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]
  }
  flere_hovedskjemaGuid <- names(table(pasientsvar_post2$HovedskjemaGUID))[table(pasientsvar_post2$HovedskjemaGUID)>1]
  if (!is.null(flere_hovedskjemaGuid)){
    pasientsvar_post2 <- pasientsvar_post2[!(pasientsvar_post2$HovedskjemaGUID %in% flere_hovedskjemaGuid), ]
  }

  # icd10 <- read.table('C:/GIT/data/nnrr/icd10.csv', sep=';',
  #                     header=T, stringsAsFactors = F, fileEncoding = 'UTF-8')
  legeskjema$regstatus <- 1
  pasientsvar_pre$regstatus <- 1
  pasientsvar_post$regstatus <- 1
  pasientsvar_post2$regstatus <- 1

  names(pasientsvar_pre)[names(pasientsvar_pre)=='SkjemaGUID'] <- 'SkjemaGUID_pre'
  names(pasientsvar_post)[names(pasientsvar_post)=='SkjemaGUID'] <- 'SkjemaGUID_post'
  names(pasientsvar_post2)[names(pasientsvar_post2)=='SkjemaGUID'] <- 'SkjemaGUID_post2'

  RegData <- merge(legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID',
                   by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'))
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
