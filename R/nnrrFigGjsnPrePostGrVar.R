#' Søylediagram med gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' Funksjon som genererer en figur med som viser endring i en variabels gjennomsnitt før og etter intervensjonen
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams nnrrFigAndeler
#' @param gr_var Velg grupperingsvariabel
#'
#' @return Søylediagram med gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' @export
#'
nnrrFigGjsnPrePostGrVar <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                                      minald=0, maxald=120, erMann=99, outfile='',
                                      enhetsUtvalg=0, preprosess=F, hentData=F, gr_var='SykehusNavn')

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nnrrHentRegData()
  }

  if (preprosess){
    RegData <- nnrrPreprosess(RegData=RegData)
  }

  RegData$Gr_var <- RegData[, gr_var]

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)])
  }

  # Definerer pre -og postvariabler, fjerner registreringer som mangler én eller begge
  PrePostVar <- switch(valgtVar,
                       ODI_PrePost = c('OdiScore', 'OdiScore_post'),
                       NDI_PrePost = c('NdiScore', 'NdiScore_post'),
                       EQ5D_PrePost = c('Eq5dScore', 'Eq5dScore_post'),
                       PainExperiencesNoActivity = c('PainExperiencesNoActivity', 'PainExperiencesNoActivity_post'),
                       PainExperiencesActivity = c('PainExperiencesActivity', 'PainExperiencesActivity_post'))

  RegData$VarPre <- RegData[ ,PrePostVar[1]]
  RegData$VarPost <- RegData[ ,PrePostVar[2]]
  RegData <- RegData[which(!is.na(RegData$VarPre) & !is.na(RegData$VarPost)), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

  PrePost <- aggregate(RegData[, c('VarPre', "VarPost")],
                       by=list(RegData$Gr_var), mean, na.rm = TRUE)
  PrePostSD <- aggregate(RegData[, c('VarPre', "VarPost")],
                         by=list(RegData$Gr_var), sd, na.rm = TRUE)
  # Ngr <- tapply(RegData[, c('VarPre')], RegData$Gr_var, function(x){length(x[!is.na(x)])})
  Ngr <- aggregate(RegData[, c('VarPre')], by=list(RegData$Gr_var), length)
  kategorier <- as.character(Ngr$Group.1)
  Ngr <- as.matrix(t(Ngr[,-1]))
  PlotMatrise <- as.matrix(t(PrePost[,-1]))
  PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('VarPre', "VarPost")]))
  PrePostSD <- as.matrix(t(PrePostSD[,-1]))
  PrePostSD <- cbind(PrePostSD, apply(RegData[, c('VarPre', "VarPost")], 2, sd, na.rm = TRUE))
  #   Ngr <- table(as.character(RegData$Gr_var))  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
  Ngr <- c(Ngr, sum(Ngr, na.rm = TRUE))
  names(Ngr) <- c(kategorier, 'Totalt')
  sammenlign <- 1

  terskel <- 5
  #
  #   Hvis man vil utelate kategori fra figur pga. for få reg.:

  if (gr_var=='SykehusNavn') {
    utelat <- which(Ngr < terskel)
    if (length(utelat)>0){
      PlotMatrise <- PlotMatrise[,-utelat]
      PrePostSD <- PrePostSD[,-utelat]
      Ngr <- Ngr[-utelat]
    }
  }

  KINed <- PlotMatrise - 1.96*PrePostSD/t(matrix(c(Ngr, Ngr), ncol = 2, nrow = length(Ngr)))
  KIOpp <- PlotMatrise + 1.96*PrePostSD/t(matrix(c(Ngr, Ngr), ncol = 2, nrow = length(Ngr)))

  KINed[ , Ngr < terskel] <- 0
  KIOpp[ , Ngr < terskel] <- 0
  ############## Lag figur  ###############################

  grtxt <- c(names(Ngr)[1:(length(Ngr)-1)], 'Totalt')
  #   if (length(utelat>0)){
  #     grtxt <- c('Haukeland' ,'SUS', 'St. Olavs', 'UNN', 'Nasjonalt')[-utelat]
  #   }
  # grtxt <- c('Haukeland' ,'SUS', 'St. Olavs', 'UNN', 'Nasjonalt')[-utelat]

  tittel <- switch(valgtVar,
                   ODI_PrePost = 'ODI-score før og etter behandling',
                   NDI_PrePost = 'NDI-score før og etter behandling',
                   EQ5D_PrePost = 'EQ5D-Score før og etter behandling',
                   PainExperiencesNoActivity = 'Smerte i hvile',
                   PainExperiencesActivity = 'Smerte i aktivitet')

  tittel <- c(tittel, 'med 95% konfidensintervall')

  ytekst <- 'Gjennomsnittsscore'
  #   ytekst <- switch(valgtVar,
  #                    'DLQI_PrePost' = 'Gjennomsnittsscore',
  #                    'HS_PrePost' = 'Gjennomsnittsscore'
  #   )
  cexgr<-0.9
  cexleg <- 0.9	#Størrelse på legendtekst
  retn<-'V'
  txtretn<-1

  FigTypUt <- rapFigurer::figtype(outfile, fargepalett='BlaaOff')
  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  PlotMatrise[ , Ngr < terskel] <- 0
  grtxt2 <-  paste0('(N=', Ngr, ')')
  grtxt2[Ngr<terskel] <- paste0('(N<', terskel, ')')

  farger <- FigTypUt$farger
  ymax <- max(PlotMatrise, na.rm=T)*1.25

  pos <- barplot(PlotMatrise, beside=TRUE, las=txtretn, ylab=ytekst,
                 col=farger[1:(sammenlign+1)], border='white', ylim=c(0, ymax))
  mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
  mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)

  title(tittel, line=1, font.main=1)
  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  legend('top', c('Konsultasjon', 'Oppfølging')[1:(sammenlign+1)],
         border=c(fargeHoved,NA), col=farger[1:(sammenlign+1)], bty='n', pch=c(15,15), pt.cex=2,
         lwd=3,	lty=NA, ncol=2, cex=cexleg)

  inkl_konf <- 1
  if (inkl_konf == 1){
    arrows(x0=pos, y0=KINed, x1=pos, y1=KIOpp, code=3, angle=90, lwd=1, length=0.03, col=farger[3]) # konfidensintervall
  }


  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}


  return(invisible(PlotMatrise))





}
