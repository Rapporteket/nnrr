#' Provide global dataframe for NNRR
#'
#' Provides NNRR data
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return RegData data frame
#' @export

nnrrHentRegData <- function(datoFra = '2017-01-01',
                            datoTil = '2099-01-01') {

  registryName <- "data"
  dbType <- "mysql"

  legeskjema <- rapbase::loadRegData(
    registryName,
    paste0(
      "SELECT * FROM behandlerskjema_1 WHERE
      S1b_DateOfCompletion >= \'",
      datoFra, "\' AND S1b_DateOfCompletion <= \'",
      datoTil, "\' ")) |>
    dplyr::distinct(SkjemaGUID, .keep_all = TRUE)

  pasientsvar_pre <- rapbase::loadRegData(
    registryName,
    paste0(
      "SELECT * FROM pasientskjema_foer_behand_2 WHERE
      S1b_DateOfCompletion >= \'",
      datoFra, "\' AND S1b_DateOfCompletion <= \'",
      datoTil, "\' ")) |>
    dplyr::distinct(HovedskjemaGUID, .keep_all = TRUE)
  pasientsvar_post <- rapbase::loadRegData(
    registryName,
    paste0(
      "SELECT * FROM pasientskjema_6_maaneder__3 WHERE
      S1b_DateOfCompletion >= \'",
      datoFra, "\' AND S1b_DateOfCompletion <= \'",
      datoTil, "\' ")) |>
    dplyr::distinct(HovedskjemaGUID, .keep_all = TRUE)
  pasientsvar_post2 <- rapbase::loadRegData(
    registryName,
    paste0(
      "SELECT * FROM pasientskjema_12_maaneder_8 WHERE
      S1b_DateOfCompletion >= \'",
      datoFra, "\' AND S1b_DateOfCompletion <= \'",
      datoTil, "\' ")) |>
    dplyr::distinct(HovedskjemaGUID, .keep_all = TRUE)

  legeskjema$regstatus <- 1
  pasientsvar_pre$regstatus <- 1
  pasientsvar_post$regstatus <- 1
  pasientsvar_post2$regstatus <- 1

  names(pasientsvar_pre)[
    names(pasientsvar_pre)=='SkjemaGUID'] <- 'SkjemaGUID_pre'
  names(pasientsvar_post)[
    names(pasientsvar_post)=='SkjemaGUID'] <- 'SkjemaGUID_post'
  names(pasientsvar_post2)[
    names(pasientsvar_post2)=='SkjemaGUID'] <- 'SkjemaGUID_post2'

  RegData <- merge(
    legeskjema, pasientsvar_pre, by.x = 'SkjemaGUID',
    by.y = 'HovedskjemaGUID', suffixes = c('', '_pre'))
  RegData <- merge(
    RegData, pasientsvar_post, by.x = 'SkjemaGUID',
    by.y = 'HovedskjemaGUID', suffixes = c('', '_post'),
    all.x = TRUE)
  RegData <- merge(
    RegData, pasientsvar_post2, by.x = 'SkjemaGUID',
    by.y = 'HovedskjemaGUID', suffixes = c('', '_post2'),
    all.x = TRUE)

  RegData$DiagnosticNumber1 <- trimws(RegData$DiagnosticNumber1)
  RegData <- nnrr::nnrrPreprosess(RegData = RegData)
  rm(list = c('pasientsvar_pre', 'legeskjema',
              'pasientsvar_post', 'pasientsvar_post2'))

  return(RegData)
}
