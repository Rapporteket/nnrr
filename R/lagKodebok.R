#' Denne funksjonen tilrettelegger en kodebok for bruk i rapportering
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return En formatert kodebok
#'
#' @export
#'

lagKodebok <- function()
{

  varnavn_1b <- xlsx::read.xlsx(system.file('extdata', 'kodebokNNRR25052018.xlsx', package = 'nnrr'), sheetIndex = 4,
                                encoding = 'UTF-8', stringsAsFactors = F)
  varnavn_1a <- xlsx::read.xlsx(system.file('extdata', 'kodebokNNRR25052018.xlsx', package = 'nnrr'), sheetIndex = 3,
                                encoding = 'UTF-8', stringsAsFactors = F)
  varnavn_2 <- xlsx::read.xlsx(system.file('extdata', 'kodebokNNRR25052018.xlsx', package = 'nnrr'), sheetIndex = 2,
                               encoding = 'UTF-8', stringsAsFactors = F)
  varnavn_1b <- tidyr::separate(varnavn_1b, Mulige.verdier, into = c('kode', 'label'), sep = ' = ')
  varnavn_1a <- tidyr::separate(varnavn_1a, Mulige.verdier, into = c('kode', 'label'), sep = ' = ')
  varnavn_2 <- tidyr::separate(varnavn_2, Mulige.verdier, into = c('kode', 'label'), sep = ' = ')
  varnavn_1b$Feltnavn <- gsub('<u>', '', varnavn_1b$Feltnavn)
  varnavn_1b$Feltnavn <- gsub('</u>', '', varnavn_1b$Feltnavn)

  save(varnavn_1a, varnavn_1b, varnavn_2, file = 'C:/GIT/nnrr/data/kodebok.RData')


  }
