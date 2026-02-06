#' Generer kvartalsrapport og returner filnavn og sti til fil.
#'
#' @export
#'
strikkRnwAbo <- function(baseName, reshID=0, datoTil = Sys.Date()) {

  src <- system.file(paste0(baseName, '.Rnw'), package="nnrr")
  tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.Rnw')

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  file.copy(src, tmpFile, overwrite = TRUE)

  pdfFile <- knitr::knit2pdf(tmpFile)
  utfil <- paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  file.copy(pdfFile, utfil)

  return(utfil)
}
