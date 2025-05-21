#' Lag søylediagram som viser andeler av ulike variabler:
#'
#' Denne funksjonen lager søylediagram som viser fordelingen til valgt variabel
#'
#' @param RegData - ei dataramme med alle nødvendige variable fra registeret
#' @param reshID - avdelingsid for egen avdeling, standard: 0-hele landet
#' @param valgtVar - Variabel som skal vises
#' @param erMann - kjønn, 1-menn, 0-kvinner, default 99, dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param datoFra <- '2014-01-01'    # min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '2050-12-31'
#' @param enhetsUtvalg: 0: Hele landet
#'                      1: Egen enhet mot resten av landet
#'                      2: Egen enhet
#'
#' @return En figur med beskrevet innhold
#'
#' @export

nnrrBeregnAndeler  <- function(RegData,
                               valgtVar,
                               datoFra='2014-01-01',
                               datoTil='2050-12-31',
                               enhetsUtvalg=1,
                               minald=0,
                               maxald=120,
                               erMann=99,
                               tverrfaglig = 99,
                               minHSCL = 1,
                               maxHSCL = 4,
                               medikamenter = NULL,
                               smerte = NULL,
                               tolk = 99,
                               reshID)
{

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$UnitId == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$UnitId)])
  }

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra,
                           datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann,
                           tverrfaglig=tverrfaglig, minHSCL = minHSCL,
                           maxHSCL = maxHSCL, medikamenter = medikamenter,
                           smerte = smerte, tolk=tolk)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  # ind <- list(Hoved=which(RegData$UnitId == reshID), Rest=which(RegData$UnitId != reshID))
  NRest <- 0
  NvarRest <- 0

  if (valgtVar %in% c('AarsakSmerte_PasRap', 'beh_kommunalt', 'beh_kommunalt_v2',
                      'beh_spesialist', 'beh_spesialist_v2',
                      'pasrapp_beh_klinikk', 'pasrapp_beh_klinikk_v2')) {
    flerevar <- 1
  } else {
    flerevar <- 0
  }

  if (flerevar == 0 ) {
    ## Forbered variabler for fremstilling i figur
    PlotParams <- nnrrPrepVar(RegData=RegData, valgtVar=valgtVar)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA
    if (enhetsUtvalg==1) {
      ind <- list(Hoved=which(RegData$UnitId == reshID),
                  Rest=which(RegData$UnitId != reshID))
      AntHoved <- table(RegData$VariabelGr[ind$Hoved])
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      AntRest <- table(RegData$VariabelGr[ind$Rest])
      NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*AntRest/NRest
      NvarRest <- rep(NRest, length(Andeler$Rest))
      NvarHoved <- rep(NHoved, length(Andeler$Hoved))
    } else {
      AntHoved <- table(RegData$VariabelGr)
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      AntRest <- 0
      NvarHoved <- rep(NHoved, length(Andeler$Hoved))
    }
  }

  if (flerevar == 1){

    if (enhetsUtvalg==1) {
      ind <- list(Hoved=which(RegData$UnitId == reshID),
                  Rest=which(RegData$UnitId != reshID))
      PlotParams <- nnrrPrepVar(RegData[ind$Hoved, ], valgtVar) # Hovegruppe
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      NvarHoved <- PlotParams$NVar
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      PlotParams2 <- nnrrPrepVar(RegData[ind$Rest, ], valgtVar) # Sammenligningsgruppe
      AntRest <- PlotParams2$AntVar
      NRest <- max(PlotParams2$NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
      NvarRest <- PlotParams2$NVar
      Andeler$Rest <- 100*PlotParams2$AntVar/PlotParams2$NVar
      rm(PlotParams2)
    } else {
      PlotParams <- nnrrPrepVar(RegData, valgtVar)
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      NvarHoved <- PlotParams$NVar
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      AntRest <- 0
    }
  }   #end sjekk om figuren inneholder flere variable

  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; #grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;

  #Beregninger som returneres fra funksjonen.
  AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
  rownames(AndelerUt) <- c('Hoved', 'Rest')
  AntallUt <- rbind(AntHoved, AntRest)
  rownames(AntallUt) <- c('Hoved', 'Rest')

  UtData <- list(paste(toString(tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
  names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
  UtData$utvalgTxt <- utvalgTxt
  UtData$N <- data.frame(NHoved = NvarHoved, NRest = NvarRest)
  UtData$NHoved <- NHoved
  UtData$NRest <- NRest
  UtData$PlotParams <- PlotParams
  UtData$enhetsUtvalg <- enhetsUtvalg
  UtData$shtxt <- shtxt

  return(invisible(UtData))
}
