#' Preprosesser data for bruk i NNRR sine rapporter
#'
#' Denne funksjonen gjor nodvendig preprosessering av NNRR sin data for bruk i rapporter
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return Et preprosessert datasett
#'
#' @export
#'

nnrrPreprosess <- function(RegData)
  {

  RegData$Besoksdato <- as.POSIXlt(RegData$Besoksdato, format="%d.%m.%Y")
  RegData$Besoksdato_pre <- as.POSIXlt(RegData$Besoksdato_pre, format="%d.%m.%Y")
  RegData$Besoksdato_post <- as.POSIXlt(RegData$Besoksdato_post, format="%d.%m.%Y")
  RegData$FormDate <- as.POSIXlt(RegData$FormDate, format="%d.%m.%Y")
  RegData$FormDate_pre <- as.POSIXlt(RegData$FormDate_pre, format="%d.%m.%Y")
  RegData$FormDate_post <- as.POSIXlt(RegData$FormDate_post, format="%d.%m.%Y")

  RegData$regstatus_pre[which(is.na(RegData$regstatus_pre))] <- 0
  RegData$regstatus_post[which(is.na(RegData$regstatus_post))] <- 0
  RegData$regstatus[which(is.na(RegData$regstatus))] <- 0

  # Ordne dato for pre og post

  #### Testing ###########
  # RegData$Besoksdato[RegData$regstatus_pre == 1] <- RegData$Besoksdato_pre[RegData$regstatus_pre == 1]
  #########################

  RegData$Besoksdato[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$Besoksdato_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$Besoksdato[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$Besoksdato_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData$PatientAge[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$PatientAge_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$PatientAge[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$PatientAge_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]
  RegData$ReshId[RegData$regstatus == 0 & RegData$regstatus_pre == 1] <-
    RegData$ReshId_pre[RegData$regstatus == 0 & RegData$regstatus_pre == 1]
  RegData$ReshId[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1] <-
    RegData$ReshId_post[RegData$regstatus == 0 & RegData$regstatus_pre == 0 & RegData$regstatus_post == 1]

  RegData$Aar <- RegData$Besoksdato$year+1900

  RegData[, c('Arbeidsbelastning', 'Hjemmebelastning', 'Folelsesmessig.belastning',
              'Fritidsaktivitet', 'Skade.i.skjelett', 'Skade.i.muskulatur',
              'Skade.i.nerve', 'Vet.ikke', 'Hode', 'Nakke', 'Ovre.del.mage', "Venstre.skulder", "Hoyre.albue", "Venstre.albue", "Mage",
              "Hoyre.haandledd", "Venstre.haandledd", "Hoyre.hofte.laar", "Venstre.hofte.laar", "Hoyre.kne", "Venstre.kne",
              "Hoyre.ankel.fot", "Venstre.ankel.fot")] <-
    apply(RegData[, c('Arbeidsbelastning', 'Hjemmebelastning', 'Folelsesmessig.belastning',
                      'Fritidsaktivitet', 'Skade.i.skjelett', 'Skade.i.muskulatur',
                      'Skade.i.nerve', 'Vet.ikke', 'Hode', 'Nakke', 'Ovre.del.mage', "Venstre.skulder", "Hoyre.albue", "Venstre.albue", "Mage",
                      "Hoyre.haandledd", "Venstre.haandledd", "Hoyre.hofte.laar", "Venstre.hofte.laar", "Hoyre.kne", "Venstre.kne",
                      "Hoyre.ankel.fot", "Venstre.ankel.fot")],
          2, function(x){as.logical(x)})

  RegData$ErMann <- NA
  RegData$ErMann[which(RegData$PatientGender == 'Female')] <- 0
  RegData$ErMann[which(RegData$PatientGender == 'Male')] <- 1

  RegData$SoktUforetrygd <- factor(RegData$Har.du.sokt.uforetrygd.,
                                   levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))
  RegData$SoktErstatning <- factor(RegData$Har.du.sokt.erstatning.,
                                   levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))
  RegData$OnsketTilbake <- factor(RegData$Foler.du.at.din.arbeidsgiver.onsker.deg.tilbake.i.jobb.,
                                  levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))
  RegData$DagligRoyk <- factor(RegData$Royker.du.daglig.,
                                  levels = c(1,2,0), labels = c('Ja', 'Nei', 'Ikke Reg.'))

  RegData$HSCL10.Score <- as.numeric(sapply(as.character(RegData$HSCL10.Score), gsub, pattern = ",", replacement= "."))
  RegData$ODI.Score <- as.numeric(sapply(as.character(RegData$ODI.Score), gsub, pattern = ",", replacement= "."))
  RegData$ODI.Score_post <- as.numeric(sapply(as.character(RegData$ODI.Score_post), gsub, pattern = ",", replacement= "."))
  RegData$NDI.Score <- as.numeric(sapply(as.character(RegData$NDI.Score), gsub, pattern = ",", replacement= "."))
  RegData$NDI.Score_post <- as.numeric(sapply(as.character(RegData$NDI.Score_post), gsub, pattern = ",", replacement= "."))

  RegData$SykehusNavn <- NA
  RegData$SykehusNavn[RegData$ReshId == 102959] <- 'Haukeland'
  RegData$SykehusNavn[RegData$ReshId == 104293] <- 'St. Olavs'
  RegData$SykehusNavn[RegData$ReshId == 109834] <- 'OUS'
  RegData$SykehusNavn[RegData$ReshId == 601032] <- 'UNN'


  names(RegData)[which(names(RegData) == 'Paa.en.skala.fra.0..verst.tenkelig.tilstand..til.100..best.tenkelige.tilstand...angi.din.naavaerende.helsetilstand..')] <- 'EQ5D.VAS'

  return(RegData)
}



