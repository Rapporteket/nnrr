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
nnrrTidsplot_6og12mnd <- function(plotdata1,
                                  plotdata2,
                                  outfile='',
                                  fargepalett="BlaaRapp",
                                  inkl_konf = 1,
                                  maal=NA,
                                  maalnivaatxt="Høy måloppnåelse",
                                  moderat=NA,
                                  moderatTxt = "Moderat måloppnåelse") {

  plotdata <- plotdata1
  tittel <- plotdata$tittel2;
  Tidtxt <- plotdata$Tidtxt
  AndelHoved <- plotdata$Andeler$AndelHoved
  AndelRest <- plotdata$Andeler$AndelRest
  NTidHoved <- plotdata$NTid$NTidHoved
  NTidRest <- plotdata$NTid$NTidRest
  Konf <- plotdata$KonfInt$Konf
  KonfRest <- plotdata$KonfInt$KonfRest
  utvalgTxt <- plotdata$utvalgTxt
  utvalgTxt[1] <- substr(utvalgTxt[1], 6, nchar(utvalgTxt[1]))
  tidsenhet <- plotdata$tidsenhet
  VarTxt <- plotdata$VarTxt
  medSml <- plotdata$medSml
  shtxt <- plotdata$shtxt
  AndelRestGjsn <- plotdata$AndelRestGjsn
  AndelHovedGjsn <- plotdata$AndelHovedGjsn
  # maal <- plotdata$maal
  smltxt <- plotdata$smltxt
  xaksetxt <- plotdata$xaksetxt2

  plotdata <- plotdata2
  AndelHoved2 <- plotdata$Andeler$AndelHoved
  NTidHoved2 <- plotdata$NTid$NTidHoved
  AndelRest2 <- plotdata$Andeler$AndelRest
  NTidRest2 <- plotdata$NTid$NTidRest

  NHovedRes <- sum(NTidHoved, na.rm = T)
  NSmlRes <- sum(NTidRest, na.rm = T)
  NHovedRes2 <- sum(NTidHoved2, na.rm = T)
  NSmlRes2 <- sum(NTidRest2, na.rm = T)
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=fargepalett, pointsizePDF=12)
  farger <- FigTypUt$farger

  if (NHovedRes < 10 | (medSml ==1 & NSmlRes<10)) {
    #-----------Figur---------------------------------------

    plot.new()
    title(main=tittel, cex.main=2)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    cexgr <- 0.9
    {
      fargeHoved <- farger[3]
      fargeRest <- farger[1]
      NutvTxt <- length(utvalgTxt)
      hmarg <- 0.04+0.01*NutvTxt
      par('fig' = c(0,1,0,1-hmarg))
      cexleg <- 1	#Størrelse på legendtekst
      cexskala <- switch(tidsenhet, Aar=1, Mnd=0.9, Kvartal=0.9, Halvaar=0.9)
      xskala <- 1:length(Tidtxt)
      # xaksetxt <- switch(tidsenhet, Aar='Intervensjonsår', Mnd='Intervensjonsår og -måned',
      #                    Kvartal='Intervensjonsår og -kvartal', Halvaar='Intervensjonsår og -halvår')
      ymax <- min(119, 1.25*max(AndelHoved, AndelRest, AndelHoved2, AndelRest2, na.rm=T))

      plot(AndelHoved,  font.main=1,  type='o', pch="'", col=fargeHoved, xaxt='n',
           frame.plot = FALSE,  xaxp=c(1,length(Tidtxt),length(Tidtxt)-1),xlim = c(1,length(Tidtxt)),
           cex=2, lwd=3, xlab=xaksetxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i',
           sub='(Tall ved punktene angir antall intervensjoner)', cex.sub=cexgr)

      lines(xskala, AndelHoved2, col="red", lwd=3)
      points(xskala, AndelHoved2, pch="'", cex=2, col="red")
      text(xskala, AndelHoved2, pos=3, NTidHoved2, cex=0.9, col="red")
      axis(side=1, at = xskala, labels = Tidtxt, cex.axis=0.9)
      title(tittel, line=1, font.main=1.4, cex.main=1.5)
      text(xskala, AndelHoved, pos=3, NTidHoved, cex=0.9, col=fargeHoved)#pos=1,

      if (medSml == 1) {

        lines(xskala, AndelRest, col=fargeRest, lwd=3)
        points(xskala, AndelRest, pch="'", cex=2, col=fargeRest)	#}
        text(xskala, AndelRest, pos=3, NTidRest, cex=0.9, col=fargeRest)
        lines(xskala, AndelRest2, col="orange", lwd=3)
        points(xskala, AndelRest2, pch="'", cex=2, col="orange")	#}
        text(xskala, AndelRest2, pos=3, NTidRest2, cex=0.9, col="orange")

        legend('top', border=NA, c(paste0(shtxt, ' 6 mnd (N=', NHovedRes, ')'),
                                   paste0(smltxt, ' 6 mnd (N=', NSmlRes, ')'),
                                   paste0(shtxt, ' 12 mnd (N=', NHovedRes2, ')'),
                                   paste0(smltxt, ' 12 mnd (N=', NSmlRes2, ')')),
               bty='n', lty=c(1,1,1,1), ncol=2, cex=cexleg, xpd = T,
               col=c(fargeHoved, fargeRest, "red", "orange"), lwd=c(3,3,3,3))

      } else {
        legend('top', paste0(shtxt, ' (N=', sum(NTidHoved), ')'),
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

      if (!is.na(moderat)) {
        lines(range(xskala),rep(moderat,2), col="yellow", lwd=2, lty=2)
        if (!is.na(moderatTxt)) {
          text(x = length(Tidtxt), y = moderat, labels = moderatTxt, adj = c(1,1), xpd=T)
        }
      }
    }

    {


    }


    if ( outfile != '') {dev.off()}
  }
}
