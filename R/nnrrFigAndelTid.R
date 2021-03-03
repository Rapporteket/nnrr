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
nnrrFigAndelTid <- function(RegData, valgtVar="tverrfaglig_behandlet", datoFra='2014-01-01', datoTil='2050-12-31', enhetsUtvalg=1, datovar="Besoksdato",
                               minald=0, maxald=130, erMann=99, outfile='', reshID, tidsenhet="Kvartal", inkl_konf=0, maal = NA, maalnivaatxt=NA)
{

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

  RegData$Dato <- RegData[, datovar]
  RegData$Aar <- as.numeric(format(RegData$Dato, '%Y'))
  RegData$Mnd <- as.numeric(format(RegData$Dato, '%m'))
  RegData$Kvartal <- floor((RegData$Mnd - 1)/3)+1
  RegData$Halvaar <- floor((RegData$Mnd - 1)/6)+1

  RegData$TidsEnhet <- switch(tidsenhet,
                              Aar = RegData$Aar-min(RegData$Aar)+1,
                              Mnd = RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1+(RegData$Aar-min(RegData$Aar))*12,
                              Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                (RegData$Aar-min(RegData$Aar))*4,
                              Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                (RegData$Aar-min(RegData$Aar))*2
  )

  Tidtxt <- switch(tidsenhet,
                   Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                               sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='.'),
                   Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                   sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                   Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                   sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                   Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]))

  RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=1:max(RegData$TidsEnhet))




  # if (valgtShus[1]!='') {
  #   valgtShus <- as.numeric(valgtShus)
  #   if (length(valgtShus)==1) {reshID<-valgtShus[1]}
  # }

  # if (enhetsUtvalg!=0 & length(valgtShus)>1) {
  #   RegData$UnitId[RegData$UnitId %in% valgtShus] <- 99
  #   shtxt <- 'Ditt utvalg'
  #   reshID <- 99
  # }

  #Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$UnitId == reshID), ]}	#{indHovedUt <- which(RegData$UnitId != reshID)}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$UnitId)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$UnitId) != reshID)
    }
  }


  NHovedRes <- length(indHoved)
  NSmlRes <- length(indRest)

  #-------------------------Beregning av andel-----------------------------------------

  NTidRest <- tapply(RegData$Variabel[indRest], RegData$TidsEnhet[indRest], length)
  NTidHendRest <- tapply(RegData$Variabel[indRest], RegData$TidsEnhet[indRest],sum, na.rm=T)
  AndelRest <- NTidHendRest/NTidRest*100
  NTidHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'TidsEnhet'], length)
  NTidHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'TidsEnhet'],sum, na.rm=T)
  AndelHoved <- NTidHendHoved/NTidHoved*100
  Andeler <- rbind(AndelRest, AndelHoved)
  AndelHovedGjsn <- sum(RegData[indHoved, 'Variabel'])/length(RegData[indHoved, 'Variabel'])*100
  AndelRestGjsn <- sum(RegData[indRest, 'Variabel'])/length(RegData[indRest, 'Variabel'])*100

  NTidHendHoved[is.na(NTidHendHoved)] <- 0
  NTidHoved[is.na(NTidHoved)] <- 0
  NTidHendRest[is.na(NTidHendRest)] <- 0
  NTidRest[is.na(NTidRest)] <- 0
  Konf <- binomkonf(NTidHendHoved, NTidHoved)*100
  KonfRest <- NULL
  if (medSml==1) {
    KonfRest <- binomkonf(NTidHendRest, NTidRest)*100}

  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
  incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  VarTxt <- PlotParams$VarTxt; ##
  # if (!(inkl_konf %in% c(0,1))) {inkl_konf=PlotParams$inkl_konf}

  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=NNRRUtvalg$fargepalett)
  farger <- FigTypUt$farger
  tittel <-  c(tittel, shtxt)

  #----------FIGUR------------------------------
  #Hvis for få observasjoner..
  #if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & medSml == 1)) {
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

    utData <- list(tittel = tittel, utvalgTxt = utvalgTxt, Andeler = list(AndelHoved=AndelHoved, AndelRest=AndelRest), Tidtxt = Tidtxt,
                   NTid=list(NTidHoved=NTidHoved, NTidRest=NTidRest), KonfInt=list(Konf=Konf, KonfRest=KonfRest))
    return(invisible(utData))
  }














}
