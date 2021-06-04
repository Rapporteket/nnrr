#' Lag søylediagram som viser andeler av ulike variabler:
#'
#' Denne funksjonen lager søylediagram som viser fordelingen til valgt variabel med
#' separat søyle for menn og kvinner.
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return En figur med beskrevet innhold
#'
#' @export

nnrrFigAntallKjonnsdelt  <- function(RegData, valgtVar, datoFra='2014-01-01', datoTil='2050-12-31', enhetsUtvalg=1,
                                     minald=0, maxald=130, erMann=99, outfile='', reshID, preprosess=F, hentData=F)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nnrrHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nnrrPreprosess(RegData=RegData)
  }

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
  Nrest <- 0


  PlotParams <- nnrrPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA
  if (enhetsUtvalg==1) {
    AntHoved <- table(RegData[ind$Hoved, c("PatientGender", "VariabelGr")])
    NHoved <- rowSums(AntHoved)
    # Andeler$Hoved <- 100*AntHoved/NHoved
    AntRest <- table(RegData[ind$Rest, c("PatientGender", "VariabelGr")])
    Nrest <- rowSums(AntRest)	#length(indRest)- Kan inneholde NA
    # Andeler$Rest <- 100*AntRest/Nrest
  } else {
    AntHoved <- table(RegData[, c("PatientGender", "VariabelGr")])
    NHoved <- rowSums(AntHoved)
    # Andeler$Hoved <- 100*AntHoved/NHoved
    AntRest <- 0
  }


  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; #grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=NNRRUtvalg$fargepalett, pointsizePDF=12)

  if (min(NHoved) < 5 | (min(Nrest)<5 & enhetsUtvalg==1)) {
    farger <- FigTypUt$farger
    plot.new()
    # title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    ymax <- max(c(AntHoved, AntRest),na.rm=T)*1.25
    pos <- barplot(AntHoved, beside=TRUE, las=1, ylab="Antall pasienter",
                   sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, # ,	names.arg=grtxt, cex.names=cexgr,
                   col=farger[c(1,2)], border='white', ylim=c(0, ymax), xaxt='n')
    mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)

    legend('topright', c(paste0('Menn, N=', NHoved[1]), paste0('Kvinner, N=', NHoved[2])), bty='n',
           fill=farger[c(1,2)], border=NA, ncol=1, cex=1)
    # mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
  }


  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}


}
