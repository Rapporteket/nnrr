#' Søylediagram som fordeling av valgt variabel, målt både før og etter intervensjon
#'
#' Funksjon som genererer en figur med som viser endring i en variabels fordeling fra før til etter intervensjonen
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return Søylediagram som fordeling av valgt variabel, målt både før og etter intervensjonen
#'
#' @export
#'
nnrrFigAndelerPrePost <- function(RegData=0, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                                    minald=0, maxald=120, erMann=99, outfile='',
                                    enhetsUtvalg=1, preprosess=F, hentData=F)

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nnrrHentRegData() ## Funksjonen må lages
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nnrrPreprosess(RegData=RegData)
  }

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  # Definerer pre -og postvariabler, fjerner registreringer som mangler én eller begge
  PrePostVar <- switch(valgtVar,
                       SmertestillendeResept = c('Smertestillende.paa.resept', 'Smertestillende.paa.resept_post'),
                       SmertestillendeUtenResept = c('Smertestillende.uten.resept', 'Smertestillende.uten.resept_post'))

  RegData$VarPre <- RegData[ ,PrePostVar[1]]
  RegData$VarPost <- RegData[ ,PrePostVar[2]]
  RegData <- RegData[which(!is.na(RegData$VarPre) & !is.na(RegData$VarPost)), ]

  ## Forbered variabler for fremstilling i figur
  PlotParams <- nnrrPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

  # Initialiserer nødvendige størrelser
  AndelerPP <- list(Hoved = 0, Rest =0)
  ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
  Nrest <- 0

  #Andeler$Hoved <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)

  if (enhetsUtvalg==1) {
    AntHovedPre <- table(RegData$VarPre[ind$Hoved]) #table(cut(RegData$VarPre, gr, right=F)) #cut sikrer at har med alle kategorier
    AntHovedPost <- table(RegData$VarPost[ind$Hoved])
    NHoved <- sum(AntHovedPre)	#length(indHoved)
    Nrest <- 0
    AndelerPP$Hoved <- cbind(AntHovedPre, AntHovedPost)/NHoved*100
    AntRestPre <- table(RegData$VarPre[ind$Rest]) #table(cut(RegData$VarPre, gr, right=F)) #cut sikrer at har med alle kategorier
    AntRestPost <- table(RegData$VarPost[ind$Rest])
    Nrest <- length(ind$Rest)
    AndelerPP$Rest <- cbind(AntRestPre, AntRestPost)/Nrest*100
    smltxt <- 'Landet forøvrig'
  } else {
    AntHovedPre <- table(RegData$VarPre) #table(cut(RegData$VarPre, gr, right=F)) #cut sikrer at har med alle kategorier
    AntHovedPost <- table(RegData$VarPost)
    NHoved <- sum(AntHovedPre)	#length(indHoved)
    Nrest <- 0
    AndelerPP$Hoved <- cbind(AntHovedPre, AntHovedPost)/NHoved*100
  }


  #-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=NNRRUtvalg$fargepalett)


  #Hvis for få observasjoner..
  if (NHoved < 1 | (enhetsUtvalg==1 & Nrest<1)) {
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste('variabel: ', valgtVar, sep=''))
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

    #Plottspesifikke parametre:
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    #Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 0.9	#Størrelse på legendtekst
    cexpt <- 2	#Størrelse på punkter (resten av landet)

    if (retn == 'V' ) {
      #Vertikale søyler eller linje
      ymax <- min(max(c(AndelerPP$Hoved, AndelerPP$Rest),na.rm=T)*1.25, 110)
      pos <- barplot(t(AndelerPP$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                     sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, # ,	names.arg=grtxt, cex.names=cexgr,
                     col=farger[c(2,1)], border='white', ylim=c(0, ymax), xaxt='n')
      mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (enhetsUtvalg == 1) {
        points(pos, as.numeric(t(AndelerPP$Rest)), col=fargeRest,  cex=cexpt, pch=18)
        legend('top', c(paste(c('Før, N=', 'Etter, N='), NHoved , sep=''),
                        paste(smltxt, ' N=', Nrest, sep='')), text.width = c(2,2,2),
               bty='n', pch=c(15,15,18), pt.cex=cexpt, #lty=c(NA,NA,NA),
               col=farger[c(2,1,3)], border=farger[c(2,1,3)], ncol=3, cex=cexleg)
      } else {
        legend('top', c('Før', 'Etter', paste('N=', NHoved , sep='')), bty='n',
               fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
      }
    }

    if (retn == 'H') {
      #Horisontale søyler
      ymax <- 2*antGr*1.6
      xmax <- min(max(c(AndelerPP$Hoved, AndelerPP$Rest),na.rm=T)*1.25, 100)
      pos <- barplot(t(AndelerPP$Hoved)[2:1,antGr:1], beside=TRUE, horiz=TRUE, main='', las=1,
                     col=farger[c(1,2)], border='white', font.main=1,  xlim=c(0,xmax), ylim=c(0.25, 3.3)*antGr,
                     names.arg=rev(grtxt), cex.names=cexgr, xlab="Andel pasienter (%)")
      #text(1,pos, paste('N=', t(Ngr), sep=''), adj=0, cex=0.8, col=farger[3])	#Antall obs i hver søyle
      if (enhetsUtvalg == 1) {
        points(as.numeric(t(AndelerPP$Rest))[2:1,antGr:1], y=pos+0.1,  col=fargeRest,  cex=cexpt, pch=18) #c("p","b","o"),
        legend('topleft', c(paste(c('Før, N=', 'Etter, N='), NHoved , sep=''),
                            paste(smltxt, ' N=', Nrest, sep='')), text.width = c(0.2,0.2,0.21)*xmax,
               bty='n', pch=c(15,15,18), pt.cex=cexpt, #lty=c(NA,NA,NA),
               col=farger[c(2,1,3)], border=farger[c(2,1,3)], ncol=3, cex=cexleg)
      } else {
        legend('top', c('Før', 'Etter',paste('N=',NHoved,sep='')), bty='n',
               fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
      }
    }

    title(tittel, font.main=1)	#line=0.5,
    title(shtxt, font.main=1, line=0.5, cex=0.7)
    #Tekst som angir hvilket utvalg som er gjort
    avst <- 0.8
    utvpos <- 3+length(tittel)-1	#Startlinje for teksten
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  }

}
