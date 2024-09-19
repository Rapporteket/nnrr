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
nnrrFigAndelGrvar_6og12mnd <- function(RegData, valgtVar1="opplevd_nytte_beh_6mnd",
                              valgtVar2="opplevd_nytte_beh_12mnd",
                              datoFra='2014-01-01', datoTil='2050-12-31',
                              enhetsUtvalg=1, datovar="Besoksdato",
                              minald=0, maxald=130, erMann=99, outfile='',
                              reshID, inkl_konf=0, grvar="beh_spes",
                              grtittel="Behandling i spesialisthelsetjenesten")
{

  # valgtVar1="opplevd_nytte_beh_6mnd"; valgtVar2="opplevd_nytte_beh_12mnd";
  # datoFra='2022-10-01'; datoTil='2023-09-30';
  # enhetsUtvalg=1; datovar="dato_oppfolg"; grtittel="Behandling i spesialisthelsetjenesten";
  # minald=0; maxald=130; erMann=99; outfile=''; inkl_konf=0; grvar="beh_spes_v3"

  RegData <- RegData[!is.na(RegData[, grvar]), ] # Forkast registreringer hvor grupperingsvariabel ikke finnes
  RegData$grvar <- RegData[, grvar]

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$UnitId == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$UnitId)])
  }

  ## Preparer variabler for fremstilling i figur
  PlotParams1 <- nnrrPrepVar(RegData=RegData, valgtVar=valgtVar1)
  PlotParams2 <- nnrrPrepVar(RegData=RegData, valgtVar=valgtVar2)

  RegData <- PlotParams1$RegData
  PlotParams1$RegData <- NA
  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann, datovar="dato_oppfolg")
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt
  utvalgTxt[1] <- substr(utvalgTxt[1], 6, nchar(utvalgTxt[1]))

  # Initialiserer nødvendige størrelser
  ind <- list(Hoved=which(RegData$UnitId == reshID), Rest=which(RegData$UnitId != reshID))

  if (enhetsUtvalg==1) {
    tabell_hoved <- RegData[ind$Hoved, ] %>%
      dplyr::group_by(grvar, .drop = FALSE) %>%
      dplyr::summarise(Antall = sum(Variabel),
                       N = dplyr::n(),
                       Andel = Antall/N*100)
    tabell_rest <- RegData[ind$Rest, ] %>%
      dplyr::group_by(grvar, .drop = FALSE) %>%
      dplyr::summarise(Antall = sum(Variabel),
                       N = dplyr::n(),
                       Andel = Antall/N*100)
    tabell_hoved$Andel[tabell_hoved$N == 0] <- 0
    tabell_rest$Andel[tabell_rest$N == 0] <- 0
    plottab <- dplyr::tibble(hoved=tabell_hoved$Andel, rest=tabell_rest$Andel)
  } else {
    tabell_hoved <- RegData %>%
      dplyr::group_by(grvar, .drop = FALSE) %>%
      dplyr::summarise(Antall = sum(Variabel),
                       N = dplyr::n(),
                       Andel = Antall/N*100)
  }

  RegData <- PlotParams2$RegData
  PlotParams2$RegData <- NA
  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann, datovar="dato_oppfolg2")
  RegData <- NNRRUtvalg$RegData

  # Initialiserer nødvendige størrelser
  ind <- list(Hoved=which(RegData$UnitId == reshID), Rest=which(RegData$UnitId != reshID))

  if (enhetsUtvalg==1) {
    tabell_hoved2 <- RegData[ind$Hoved, ] %>%
      dplyr::group_by(grvar, .drop = FALSE) %>%
      dplyr::summarise(Antall = sum(Variabel),
                       N = dplyr::n(),
                       Andel = Antall/N*100)
    tabell_rest2 <- RegData[ind$Rest, ] %>%
      dplyr::group_by(grvar, .drop = FALSE) %>%
      dplyr::summarise(Antall = sum(Variabel),
                       N = dplyr::n(),
                       Andel = Antall/N*100)
    tabell_hoved2$Andel[tabell_hoved2$N == 0] <- 0
    tabell_rest2$Andel[tabell_rest2$N == 0] <- 0
    plottab2 <- dplyr::tibble(hoved=tabell_hoved2$Andel, rest=tabell_rest2$Andel)
  } else {
    tabell_hoved2 <- RegData %>%
      dplyr::group_by(grvar, .drop = FALSE) %>%
      dplyr::summarise(Antall = sum(Variabel),
                       N = dplyr::n(),
                       Andel = Antall/N*100)
  }

  plottab <- dplyr::bind_cols(plottab2, plottab)

  #-----------Figur---------------------------------------

  tittel <- PlotParams1$tittel; grtxt <- as.character(tabell_hoved[[1]]); grtxt2 <- PlotParams1$grtxt2;
  subtxt <- PlotParams1$subtxt; cexgr <- PlotParams1$cexgr; retn <- "H" #retn <- PlotParams1$retn;
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett='BlaaOff')

  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  farger <- FigTypUt$farger
  fargeHoved <- farger[1]
  fargeRest <- farger[3]

  ymax <- 5*length(grtxt)*1.2

  if (enhetsUtvalg==1) {
    pos <- barplot(t(as.matrix(plottab)), beside = T, horiz = T,
                   col = c("red", "orange", farger[c(3,4)]), border = NA, ylim = c(0, ymax),
                   xlab = "Andel (%)")

    legend('top', rev(c(paste0(shtxt, " 12 mnd, N=", sum(tabell_hoved2$N)),
                        paste0("Landet for øvrig", " 12 mnd, N=", sum(tabell_rest2$N)),
                        paste0(shtxt, " 6 mnd, N=", sum(tabell_hoved$N)),
                        paste0("Landet for øvrig", " 6 mnd, N=", sum(tabell_rest$N)))),
           bty='n', fill=rev(c("red", "orange", farger[c(3,4)])), border=NA, ncol=2, cex=1, xpd = T)
  }

  # paste0(tabell_hoved$Antall, " av ", tabell_hoved$N)
  text(x=0, y=pos[1,], labels = paste0(tabell_hoved2$Antall, " av ", tabell_hoved2$N), cex = .6, adj=0)
  text(x=0, y=pos[2,], labels = paste0(tabell_rest2$Antall, " av ", tabell_rest2$N), cex = .6, adj=0)
  text(x=0, y=pos[3,], labels = paste0(tabell_hoved$Antall, " av ", tabell_hoved$N), cex = .6, adj=0)
  text(x=0, y=pos[4,], labels = paste0(tabell_rest$Antall, " av ", tabell_rest$N), cex = .6, adj=0)
  mtext(grtittel, WEST<-2, line=-1, cex=1, outer=TRUE)
  mtext(at=colMeans(pos)+0.05, text=grtxt, side=2, las=1, cex=1, adj=1, line=0.25)
  title(tittel, font.main=1.4)	#line=0.5,
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}





















}
