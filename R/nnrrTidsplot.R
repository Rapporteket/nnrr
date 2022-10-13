#' Tidstrend av rate/andel for en gitt variabel
#'
#' Årlige, kvartalsmessige eller månedlige rater for valgt variabel.
#' Konfidensintervall kan inkluderes hvis ønskelig.
#'
#' Konfidensintervallet er basert på Clopper Pearsons "eksakte" metode for binominalfordelt data.
#'
#' @inheritParams nnrrFigAndeler
#' @param inkl_konf Inkluder konfidensintervall i figur
#'                  0: Nei
#'                  1: Ja
#'                  99: Bruk default som definert i nnrrPrepVar
#' @param tidsenhet Tidsenhet for figur
#'                  'Aar' (Default)
#'                  'Kvartal'
#'                  'Mnd'
#'
#' @return En figur med tidsutvikling av andel over tid
#'
#' @export
#'
nnrrTidsplot <- function(plotdata, outfile='', fargepalett="BlaaRapp") {


  tittel <- plotdata$PlotParams$tittel;
  grtxt <- plotdata$PlotParams$grtxt;
  subtxt <- plotdata$PlotParams$subtxt;
  retn <- plotdata$PlotParams$retn;
  cexgr <- plotdata$PlotParams$cexgr;
  NHoved <- plotdata$NHoved
  NRest <- plotdata$NRest
  utvalgTxt <- plotdata$utvalgTxt
  enhetsUtvalg <- plotdata$enhetsUtvalg
  shtxt <- plotdata$shtxt
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=fargepalett, pointsizePDF=12)


  if (length(indHoved) < 10 | (medSml ==1 & length(indRest)<10)) {
    #-----------Figur---------------------------------------
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

    if (inkl_konf==1) {
      Ant_tidpkt <- length(Tidtxt)
      xmin <- 0.9
      xmax <- Ant_tidpkt
      cexgr <- 0.9	#Kan endres for enkeltvariable
      ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
      ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)
      NutvTxt <- length(utvalgTxt)
      par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))

      xskala <- 1:Ant_tidpkt

      fargeHovedRes <- farger[1]
      fargeRestRes <- farger[4]

      plot(xskala, AndelHoved, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE,
           ylab=c(paste0('Andel ', VarTxt),'inkl. 95% konfidensintervall'),
           xlab=switch(tidsenhet, Aar='Intervensjonsår', Mnd='Intervensjonsår og -måned',
                       Kvartal='Intervensjonsår og -kvartal', Halvaar='Intervensjonsår og -halvår'),
           xaxt='n',
           sub='(Tall i boksene angir antall intervensjoner)', cex.sub=cexgr)	#, axes=F)
      axis(side=1, at = xskala, labels = Tidtxt)

      if (medSml==1) {

        polygon( c(xskala, xskala[Ant_tidpkt:1]), c(KonfRest[1,], KonfRest[2,Ant_tidpkt:1]),
                 col=fargeRestRes, border=NA)
        legend('top', cex=0.9, bty='o', bg='white', box.col='white', lty = NA,
               lwd=NA, pch=15, pt.cex=2, col=fargeRestRes,
               legend = paste0('95% konfidensintervall for ', smltxt, ', N=', sum(NTidRest, na.rm=T)) )
      } else {
        legend('top', cex=0.9, bty='o', bg='white', box.col='white', lty = 2,
               lwd=2, col=farger[1], legend = paste0('Gjennomsnitt hele perioden, ', shtxt, ' = ', round(AndelHovedGjsn, 1)))
      }
      h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
      b <- 1.1*strwidth(max(NTidHoved, na.rm=T), cex=cexgr)/2	#length(Aartxt)/30
      rect(xskala-b, AndelHoved-h, xskala+b, AndelHoved+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
      text(xskala, AndelHoved, NTidHoved, col=fargeHovedRes, cex=cexgr)

      #Konfidensintervall:
      ind <- which(Konf[1, ] > AndelHoved-h) #Nedre konfidensintervall som er mindre enn boksen
      options('warn'=-1)
      arrows(x0=xskala, y0=AndelHoved-h, x1=xskala, length=0.08, code=2, angle=90,
             y1=replace(Konf[1, ], ind, AndelHoved[ind]-h), col=fargeHovedRes, lwd=1.5)
      ind2 <- which(Konf[2, ] < AndelHoved+h) #Øvre konfidensintervall som er mindre enn boksen
      arrows(x0=xskala, y0=AndelHoved+h, x1=xskala, y1=replace(Konf[2, ], ind2, AndelHoved[ind2]+h),
             length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

      title(main=tittel, font.main=1, line=1)
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      # if (!is.na(maal)) {
      #   lines(range(xskala),rep(maal,2), col="green", lwd=2, lty=1)
      # }
      if (!is.na(maal)) {
        lines(range(xskala),rep(maal,2), col="green", lwd=2, lty=2)
        if (!is.na(maalnivaatxt)) {
          text(x = length(Tidtxt), y = maal, labels = maalnivaatxt, adj = c(1,1), xpd=T)
        }
      }

      # par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}

    } else {

      fargeHoved <- farger[3]
      fargeRest <- farger[1]
      NutvTxt <- length(utvalgTxt)
      hmarg <- 0.04+0.01*NutvTxt
      par('fig' = c(0,1,0,1-hmarg))
      cexleg <- 1	#Størrelse på legendtekst
      cexskala <- switch(tidsenhet, Aar=1, Mnd=0.9, Kvartal=0.9, Halvaar=0.9)
      xskala <- 1:length(Tidtxt)
      xaksetxt <- switch(tidsenhet, Aar='Intervensjonsår', Mnd='Intervensjonsår og -måned',
                         Kvartal='Intervensjonsår og -kvartal', Halvaar='Intervensjonsår og -halvår')
      ymax <- min(119, 1.25*max(Andeler,na.rm=T))

      plot(AndelHoved,  font.main=1,  type='o', pch="'", col=fargeHoved, xaxt='n',
           frame.plot = FALSE,  xaxp=c(1,length(Tidtxt),length(Tidtxt)-1),xlim = c(1,length(Tidtxt)),
           cex=2, lwd=3, xlab=xaksetxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i',
           sub='(Tall ved punktene angir antall intervensjoner)', cex.sub=cexgr)

      axis(side=1, at = xskala, labels = Tidtxt, cex.axis=0.9)
      title(tittel, line=1, font.main=1)
      text(xskala, AndelHoved, pos=3, NTidHoved, cex=0.9, col=fargeHoved)#pos=1,

      if (medSml == 1) {

        lines(xskala, AndelRest, col=fargeRest, lwd=3)
        points(xskala, AndelRest, pch="'", cex=2, col=fargeRest)	#}
        text(xskala, AndelRest, pos=3, NTidRest, cex=0.9, col=fargeRest)
        legend('top', border=NA, c(paste0(shtxt, ' (N=', NHovedRes, ')'),
                                   paste0(smltxt, ' (N=', NSmlRes, ')')),
               bty='n', lty=c(1,1), ncol=1, cex=cexleg,
               col=c(fargeHoved, fargeRest), lwd=c(3,3))
        # legend('topleft', border=NA, c(paste0(shtxt, ' (N=', NHovedRes, ')'),
        #                                paste0(smltxt, ' (N=', NSmlRes, ')'), paste0(shtxt, ' Gj.snitt'), paste0(smltxt, ' Gj.snitt')),
        #        bty='n', lty=c(1,1,2,2), ncol=2, cex=cexleg,
        #        col=c(fargeHoved, fargeRest, fargeHoved, fargeRest), lwd=c(3,3,2,2))

      } else {
        legend('top', paste0(shtxt, ' (N=', NHovedRes, ')'),
               col=c(fargeHoved, NA), lwd=3, bty='n')
      }

      #Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
      if ((ymax > 10) & (ymax < 40)) {lines(range(xskala),rep(10,2), col=farger[4])}
      if (ymax > 20) {lines(range(xskala),rep(20,2), col=farger[4])}
      if ((ymax > 30) & (ymax < 40)) {lines(range(xskala),rep(30,2), col=farger[4])}
      if (ymax > 40) {lines(range(xskala),rep(40,2), col=farger[4])}
      if (ymax > 60) {lines(range(xskala),rep(60,2), col=farger[4])}
      if (ymax > 80) {lines(range(xskala),rep(80,2), col=farger[4])}
      if (ymax > 100) {lines(range(xskala),rep(100,2), col=farger[4])}
      #		axis(2, at=c(0,20,40,60,80,100), pos=0),

      #Tekst som angir hvilket utvalg som er gjort
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

      if (!is.na(maal)) {
        lines(range(xskala),rep(maal,2), col="green", lwd=2, lty=2)
        if (!is.na(maalnivaatxt)) {
          text(x = length(Tidtxt), y = maal, labels = maalnivaatxt, adj = c(1,1), xpd=T)
        }
      }

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}
    }
  }
}
