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
nnrrPlotGjsnPrePostGrVar <- function(plotparams, outfile='', inkl_konf = 1,
                                    fargepalett='BlaaOff', terskel = 5)

{
  tittel <- plotparams$tittel
  PlotMatrise <- plotparams$PlotMatrise
  KINed <- plotparams$KINed
  KIOpp <- plotparams$KIOpp
  Ngr <- plotparams$Ngr
  grtxt <- plotparams$grtxt
  sammenlign <- plotparams$sammenlign
  utvalgTxt <- plotparams$utvalgTxt

  if (inkl_konf) {
    tittel <- c(tittel, 'med 95% konfidensintervall')
  }

  ytekst <- 'Gjennomsnittsscore'

  cexgr<-0.9
  cexleg <- 0.9	#Størrelse på legendtekst

  PlotMatrise[ , Ngr < terskel] <- 0
  grtxt2 <-  paste0('(N=', Ngr, ')')
  grtxt2[Ngr<terskel] <- paste0('(N<', terskel, ')')
  grtxt <- paste0(grtxt, "\n ", grtxt2)
  KINed[ , Ngr < terskel] <- 0
  KIOpp[ , Ngr < terskel] <- 0

  rekkeflg <- order(PlotMatrise[1, ])
  PlotMatrise <- PlotMatrise[, rekkeflg]
  grtxt <- grtxt[rekkeflg]
  KINed <- KINed[dim(KINed)[1]:1, rekkeflg]
  KIOpp <- KIOpp[dim(KIOpp)[1]:1, rekkeflg]

  xmax <- max(PlotMatrise, na.rm=T)*1.25
  if (sammenlign == 0) {
    ymax <- length(PlotMatrise)*(sammenlign + 1)*1.3
  } else {
    ymax <- dim(PlotMatrise)[2]*(sammenlign + 2)*1.1
    }


  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=fargepalett)
  farger <- FigTypUt$farger
  NutvTxt <- length(utvalgTxt)
  vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7)
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med


  if (sammenlign == 0) {
    pos <- barplot(PlotMatrise, horiz=TRUE, main=tittel, las=1,
                   col=farger[1:(sammenlign+1)], border='white', font.main=1,  xlim=c(0,xmax), ylim =c(0,ymax),
                   names.arg=grtxt, cex.names=cexgr, xlab="Gjsn.score")
  } else {
    pos <- barplot(PlotMatrise[dim(PlotMatrise)[1]:1, ], beside=TRUE, horiz=TRUE, main=tittel, las=1,
                   col=farger[1:(sammenlign+1)], border='white', font.main=1,  xlim=c(0,xmax), ylim =c(0,ymax),
                   names.arg=grtxt, cex.names=cexgr, xlab="Gjsn.score")
  }


  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  legend('top', c('Konsultasjon', 'Oppfølging 6 mnd', 'Oppfølging 12 mnd')[1:(sammenlign+1)],
         border=c(fargeHoved,NA), col=rev(farger[1:(sammenlign+1)]), bty='n', pch=c(15,15), pt.cex=2,
         lwd=3,	lty=NA, ncol=2, cex=cexleg)

  if (inkl_konf == 1){
    arrows(y0=pos, x0=KINed, y1=pos, x1=KIOpp, code=3, angle=90, lwd=1, length=0.03, col=farger[4])
  }

  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

}
