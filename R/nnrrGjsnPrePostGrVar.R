#' Denne funksjonen generer figur på gjennomsnitt ved konsultasjon og 6mnd etter
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return En figur med gjennomsnitt ved konsultasjon og 6mnd etter
#' @export

nnrrGjsnPrePostGrVar <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='2050-12-31',
                           outfile = '', minald=0, maxald=130, gr_var='SykehusNavn', enhetsUtvalg=0,
                           erMann=99, reshID, inkl_konf=0, graa='')
{

  # RegData<-RegDataAll; valgtVar<-'PainExperiencesNoActivity' ; datoFra='2012-04-01'; datoTil='2050-12-31';
  # outfile = ''; minald=0; maxald=130; gr_var='SykehusNavn'; enhetsUtvalg=0;
  # erMann=99; reshID<-0; inkl_konf=0; graa=''

  ## Fjerner registreringer som mangler valgt variabel
  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]

  ## Setter grupperingsvariabel
  RegData$Gr_var <- RegData[, gr_var]

  ## Hvis kun egen avdeling
  if (enhetsUtvalg==2) {
    RegData <- RegData[which(RegData$AvdRESH == reshID), ]
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  # Definerer pre -og postvariabler, fjerner registreringer som mangler én eller begge
  PrePostVar <- switch(valgtVar,
                       PainExperiencesNoActivity = c('PainExperiencesNoActivity', 'PainExperiencesNoActivity_post'),
                       PainExperiencesActivity = c('PainExperiencesActivity', 'PainExperiencesActivity_post'))

  RegData$VarPre <- RegData[ ,PrePostVar[1]]
  RegData$VarPost <- RegData[ ,PrePostVar[2]]
  RegData <- RegData[!is.na(RegData$VarPre) & !is.na(RegData$VarPost), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  nnrrUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann)
  RegData <- nnrrUtvalg$RegData
  utvalgTxt <- nnrrUtvalg$utvalgTxt

  if (enhetsUtvalg==2) {
    utvalgTxt <- c(paste0('Avdeling: ', shtxt), utvalgTxt)
  }

  if (dim(RegData)[1] < 5) {
    ########## Plot feilmelding
    plot.new()
    legend('topleft',utvalgTxt, bty='n', cex=0.9)
    text(0.5, 0.6, 'Færre enn 5 registreringer', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

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

    tittel <-   PrePostVar <- switch(valgtVar,
                                     PainExperiencesNoActivity = 'Smerte i hvile',
                                     PainExperiencesActivity = 'Smerte i aktivitet')

    tittel <- c(tittel, 'med 95% konfidensintervall')

    ytekst <- 'Gjennomsnittsscore'

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

    legend('top', c('Konsultatsjon', 'Oppfølging')[1:(sammenlign+1)],
           border=c(fargeHoved,NA), col=farger[1:(sammenlign+1)], bty='n', pch=c(15,15), pt.cex=2,
           lwd=3,	lty=NA, ncol=2, cex=cexleg)

    inkl_konf <- 1
    if (inkl_konf == 1){
      arrows(x0=pos, y0=KINed, x1=pos, y1=KIOpp, code=3, angle=90, lwd=1, length=0.03, col=farger[3]) # konfidensintervall
    }


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
  }
}
