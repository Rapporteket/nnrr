#' Beregn gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' Funksjon som beregner endring i en variabels gjennomsnitt før og etter intervensjonen
#'
#' @inheritParams nnrrFigAndeler
#' @param gr_var Grupperingsvariabel
#'
#' @return plotparams En liste med verdier til generering av plot
#'
#' @export
#'
nnrrBeregnGjsnPrePostGrVar <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                                     minald=0, maxald=120, erMann=99, sammenlign = 1,
                                     enhetsUtvalg=0, gr_var='SykehusNavn')

{

  RegData$Gr_var <- RegData[, gr_var]

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}

  # Definerer pre -og postvariabler, fjerner registreringer som mangler én eller begge
  PrePostVar <- switch(valgtVar,
                       ODI_PrePost = c('OdiScore', 'OdiScore_post', 'OdiScore_post2'),
                       NDI_PrePost = c('NdiScore', 'NdiScore_post', 'NdiScore_post2'),
                       EQ5D_PrePost = c('Eq5dScore', 'Eq5dScore_post', 'Eq5dScore_post'), # NB, finn ut av
                       PainExperiencesNoActivity = c('PainExperiencesNoActivity',
                                                     'PainExperiencesNoActivity_post',
                                                     'PainExperiencesNoActivity_post2'),
                       PainExperiencesActivity = c('PainExperiencesActivity',
                                                   'PainExperiencesActivity_post',
                                                   'PainExperiencesActivity_post2'))

  RegData$VarPre <- RegData[ ,PrePostVar[1]]
  RegData$VarPost <- RegData[ ,PrePostVar[2]]
  RegData$VarPost2 <- RegData[ ,PrePostVar[3]]
  if (sammenlign == 0) {RegData <- RegData[which(!is.na(RegData$VarPre)), ]}
  if (sammenlign == 1) {RegData <- RegData[which(!is.na(RegData$VarPre) & !is.na(RegData$VarPost)), ]}
  if (sammenlign == 2) {RegData <- RegData[which(!is.na(RegData$VarPre) &
                                                   !is.na(RegData$VarPost) &
                                                   !is.na(RegData$VarPost2)), ]}

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

  PrePost <- aggregate(RegData[, c('VarPre', "VarPost", "VarPost2")[1:(sammenlign+1)]],
                       by=list(RegData$Gr_var), mean, na.rm = TRUE)
  PrePostSD <- aggregate(RegData[, c('VarPre', "VarPost", "VarPost2")[1:(sammenlign+1)]],
                         by=list(RegData$Gr_var), sd, na.rm = TRUE)
  # Ngr <- tapply(RegData[, c('VarPre')], RegData$Gr_var, function(x){length(x[!is.na(x)])})
  Ngr <- aggregate(RegData[, c('VarPre')], by=list(RegData$Gr_var), length)
  kategorier <- as.character(Ngr$Group.1)
  Ngr <- as.matrix(t(Ngr[,-1]))
  PlotMatrise <- as.matrix(t(PrePost[,-1]))
  PrePostSD <- as.matrix(t(PrePostSD[,-1]))
  if (sammenlign == 0) {
    PlotMatrise <- cbind(PlotMatrise, mean(RegData[, c('VarPre')]))
    PrePostSD <- cbind(PrePostSD, sd(RegData[, c('VarPre')]))
  } else {
    PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('VarPre', "VarPost", "VarPost2")[1:(sammenlign+1)]]))
    PrePostSD <- cbind(PrePostSD, apply(RegData[, c('VarPre', "VarPost", "VarPost2")[1:(sammenlign+1)]], 2, sd, na.rm = TRUE))
  }

  Ngr <- c(Ngr, sum(Ngr, na.rm = TRUE))
  names(Ngr) <- c(kategorier, 'Totalt')

  KINed <- PlotMatrise - 1.96*PrePostSD/t(matrix(Ngr, ncol = sammenlign+1, nrow = length(Ngr)))
  KIOpp <- PlotMatrise + 1.96*PrePostSD/t(matrix(Ngr, ncol = sammenlign+1, nrow = length(Ngr)))

  grtxt <- c(names(Ngr)[1:(length(Ngr)-1)], 'Totalt')

  tittel <- switch(valgtVar,
                   ODI_PrePost = 'ODI-score',
                   NDI_PrePost = 'NDI-score',
                   EQ5D_PrePost = 'EQ5D-Score',
                   PainExperiencesNoActivity = 'Smerte i hvile',
                   PainExperiencesActivity = 'Smerte i aktivitet')

  plotparams <- list(PlotMatrise = PlotMatrise,
                     tittel = tittel,
                     KINed = KINed,
                     KIOpp = KIOpp,
                     Ngr = Ngr,
                     grtxt = grtxt,
                     sammenlign = sammenlign,
                     utvalgTxt = utvalgTxt
                     )

  return(invisible(plotparams))

}
