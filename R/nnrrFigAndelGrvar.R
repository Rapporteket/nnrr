#' Andel for en gitt variabel gruppert etter grupperingsvariabel
#'
#' Første utkast, skal rafineres.
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return En figur med andel gruppert etter grupperingsvariabel
#'
#' @export
#'
nnrrFigAndelGrvar <- function(RegData, valgtVar="opplevd_nytte_beh", datoFra='2014-01-01', datoTil='2050-12-31', enhetsUtvalg=1, datovar="Besoksdato",
                            minald=0, maxald=130, erMann=99, outfile='', reshID, inkl_konf=0, grvar="beh_spes",
                            grtittel="Behandling i spesialisthelsetjenesten")
{

  # valgtVar="opplevd_nytte_beh"; datoFra='2014-01-01'; datoTil='2050-12-31'; enhetsUtvalg=1; datovar="dato_oppfolg";
  # minald=0; maxald=130; erMann=99; outfile=''; inkl_konf=0; grvar="beh_spes"

  RegData <- RegData[!is.na(RegData[, grvar]), ] # Forkast registreringer hvor grupperingsvariabel ikke finnes

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$UnitId == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$UnitId)])
  }

  ## Preparer variabler for fremstilling i figur
  PlotParams <- nnrrPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann, datovar=datovar)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt


  # Initialiserer nødvendige størrelser

  ind <- list(Hoved=which(RegData$UnitId == reshID), Rest=which(RegData$UnitId != reshID))

  if (enhetsUtvalg==1) {
    tabell_hoved <- RegData[ind$Hoved, ] %>% group_by(beh_spes, .drop = FALSE) %>% summarise(Antall = sum(Variabel),
                                                 N = n(),
                                                 Andel = Antall/N*100)
    tabell_rest <- RegData[ind$Rest, ] %>% group_by(beh_spes, .drop = FALSE) %>% summarise(Antall = sum(Variabel),
                                                              N = n(),
                                                              Andel = Antall/N*100)
    tabell_hoved$Andel[tabell_hoved$N == 0] <- 0
    tabell_rest$Andel[tabell_rest$N == 0] <- 0
    plottab <- tibble(hoved=tabell_hoved$Andel, rest=tabell_rest$Andel)
  } else {
    tabell_hoved <- RegData %>% group_by(beh_spes, .drop = FALSE) %>% summarise(Antall = sum(Variabel),
                                                              N = n(),
                                                              Andel = Antall/N*100)
  }

  #-----------Figur---------------------------------------

  tittel <- PlotParams$tittel; grtxt <- as.character(tabell_hoved[[1]]); grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; cexgr <- PlotParams$cexgr; retn <- "H" #retn <- PlotParams$retn;
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett='BlaaOff')

  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  farger <- FigTypUt$farger
  fargeHoved <- farger[1]
  fargeRest <- farger[3]

  ymax <- 3*length(grtxt)*1.2

  if (enhetsUtvalg==1) {
    pos <- barplot(t(as.matrix(plottab)), beside = T, horiz = T, col = farger[c(3,4)], border = NA, ylim = c(0, ymax),
                   xlab = "Andel (%)")

    legend('top', c(paste0(shtxt, ", N=", sum(tabell_hoved$N)), paste0("Landet for øvrig", ", N=", sum(tabell_rest$N))), bty='n',
           fill=farger[c(3,4)], border=NA, ncol=1, cex=1, xpd = T)
  }

  paste0(tabell_hoved$Antall, " av ", tabell_hoved$N)
  text(x=0, y=pos[1,], labels = paste0(tabell_hoved$Antall, " av ", tabell_hoved$N), cex = .8, adj=0)
  text(x=0, y=pos[2,], labels = paste0(tabell_rest$Antall, " av ", tabell_rest$N), cex = .8, adj=0)
  mtext(grtittel, WEST<-2, line=-1, cex=1, outer=TRUE)
  mtext(at=colMeans(pos)+0.05, text=grtxt, side=2, las=1, cex=1, adj=1, line=0.25)
  title(tittel, font.main=1.2)	#line=0.5,
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}





















}
