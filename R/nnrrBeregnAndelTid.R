#' Tidstrend av rate/andel for en gitt variabel
#'
#' Årlige, kvartalsmessige eller månedlige rater for valgt variabel.
#' Konfidensintervall kan inkluderes hvis ønskelig.
#'
#' Konfidensintervallet er basert på Clopper Pearsons "eksakte" metode for binominalfordelt data.
#'
#' @inheritParams nnrrFigAndeler
#' @param tidsenhet Tidsenhet for figur
#'                  'Aar' (Default)
#'                  'Kvartal'
#'                  'Mnd'
#'
#' @return En figur med tidsutvikling av andel over tid
#'
#' @export
#'
nnrrBeregnAndelTid <- function(RegData,
                               valgtVar="tverrfaglig_behandlet",
                               datoFra='2014-01-01',
                               datoTil='2050-12-31',
                               enhetsUtvalg=1,
                               datovar="Besoksdato",
                               minald=0,
                               maxald=130,
                               erMann=99,
                               outfile='',
                               reshID,
                               tidsenhet="Kvartal",
                               maal = NA,
                               maalnivaatxt=NA,
                               tverrfaglig = 99,
                               minHSCL = 1,
                               maxHSCL = 4,
                               medikamenter = NULL,
                               smerte = NULL,
                               tolk = 99,
                               iArbeid = 99)
{
  datotxt <- switch(datovar,
                    Besoksdato = "intervensjon",
                    dato_oppfolg = "6-mnd oppfølging",
                    dato_oppfolg2 = "12-mnd oppfølging"
  )
  xaksetxt <- switch(tidsenhet,
                     Aar=paste0("År for ", datotxt),
                     Mnd=paste0("År og måned for ", datotxt),
                     Kvartal=paste0("År og kvartal for ", datotxt),
                     Halvaar=paste0("År og halvår for ", datotxt))
  xaksetxt2 <- switch(tidsenhet,
                      Aar="År for oppfølging",
                      Mnd="År og måned for oppfølging",
                      Kvartal="År og kvartal for oppfølging",
                      Halvaar="År og halvår for oppfølging")

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
                           maxald=maxald, erMann=erMann, datovar=datovar,
                           tverrfaglig=tverrfaglig, minHSCL = minHSCL,
                           maxHSCL = maxHSCL, medikamenter = medikamenter,
                           smerte = smerte, tolk=tolk, iArbeid = iArbeid)
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

  #Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$UnitId == reshID), ]}	#{indHovedUt <- which(RegData$UnitId != reshID)}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
    smltxt <- NULL
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

  # FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=NNRRUtvalg$fargepalett)
  # farger <- FigTypUt$farger
  tittel2 <- tittel
  tittel <-  c(tittel, shtxt)

  utData <- list(tittel = tittel,
                 tittel2 = tittel2,
                 utvalgTxt = utvalgTxt,
                 Andeler = list(AndelHoved=AndelHoved, AndelRest=AndelRest),
                 Tidtxt = Tidtxt,
                 NTid=list(NTidHoved=NTidHoved, NTidRest=NTidRest),
                 KonfInt=list(Konf=Konf, KonfRest=KonfRest),
                 tidsenhet=tidsenhet,
                 VarTxt=VarTxt,
                 medSml=medSml,
                 smltxt=smltxt,
                 shtxt=shtxt,
                 AndelHovedGjsn=AndelHovedGjsn,
                 AndelRestGjsn=AndelRestGjsn,
                 maal=maal,
                 xaksetxt=xaksetxt,
                 xaksetxt2=xaksetxt2)
  return(invisible(utData))
}














