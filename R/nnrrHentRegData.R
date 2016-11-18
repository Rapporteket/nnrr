#' Provide global dataframe for NNRR
#'
#' Provides NNRR data from staging
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return RegData data frame
#' @export

nnrrHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nnrr"
  dbType <- "mysql"

  query <- paste0("SELECT

                  FROM AlleVariablerNum INNER JOIN ForlopsOversikt
                  ON AlleVariablerNum.ForlopsID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
