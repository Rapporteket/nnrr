
rm(list=ls())
library(nnrr)
# library(dplyr)
# library(tidyr)

rap_aar <- 2023

RegData <- nnrr::nnrrHentRegData()
Oppf_data <- RegData
Oppf_data$Aar <- Oppf_data$aar_oppfolg

aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]
aux <- aux[!is.na(aux$OdiScore) & !is.na(aux$OdiScore_post), ]
aux$odidiff_klin_viktig <- 0
aux$odidiff_klin_viktig[(aux$OdiScore - aux$OdiScore_post)/aux$OdiScore >= .3] <- 1

aux <- aux %>% mutate(Gr = ifelse(HSCL10.Score >= 1.85, "StrLik", "Mindre")) %>%
  dplyr::filter(!is.na(Gr))

Tabell1 <- aux %>% filter(Aar <= rap_aar & Aar > rap_aar-3) %>%
  summarise(ant_klin_vikt = sum(odidiff_klin_viktig),
            N=n(),
            .by = c(Aar, Gr)) %>%
  mutate(andel_klin_vikt = ant_klin_vikt/N*100)

Tabell <- Tabell1 %>%
  tidyr::pivot_wider(id_cols = Gr, names_from = Aar, values_from = andel_klin_vikt) %>%
  select(1,4,3,2)

outfile<-"testfil.svg"
FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                pointsizePDF=11, fargepalett='BlaaOff')
farger <- FigTypUt$farger

xpos <- barplot(as.vector(unlist(Tabell[,4])), col = farger[3], border = F,
        ylim = c(0,1.25*max(Tabell[,-1])), main = "Klinisk viktig endring i ODI",
        ylab = "Andel %")
pktStr <- 1.5
points(y=as.vector(unlist(Tabell[,2])), x=xpos,cex=pktStr) #'#4D4D4D'
points(y=as.vector(unlist(Tabell[,3])), x=xpos,cex=pktStr,pch= 19)
legend('top', cex=0.9*pktStr, bty='n', #bg='white', box.col='white',y=max(ypos),
       lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
       legend=names(Tabell[-1]), ncol = 3)

mtext(c(paste0("N=", Tabell1$N[1]), paste0("N=", Tabell1$N[2])), side = 1, line = -1, at = xpos)
mtext(c("HSCL-score \u2265 1.85", "HSCL-score < 1.85"), side = 1, line = 1, at = xpos)

if (outfile != "") {dev.off()}

# prop.test(x = c(457, 435), n = c(1389, 1662),
#           alternative = "greater")


###### Bestilling 02.11.2023 #####################################################
aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]
aux <- aux[!is.na(aux$OdiScore) & !is.na(aux$OdiScore_post), ]
aux$odidiff_klin_viktig <- 0
aux$odidiff_klin_viktig[(aux$OdiScore - aux$OdiScore_post)/aux$OdiScore >= .3] <- 1

aux <- aux %>% mutate(Gr = ifelse(PainDurationNow != "Mer enn 2", "mindreellerlik2", "str2")) %>%
  dplyr::filter(PainDurationNow != "Ikke svart")

Tabell1 <- aux %>% filter(Aar <= rap_aar & Aar > rap_aar-3) %>%
  summarise(ant_klin_vikt = sum(odidiff_klin_viktig),
            N=n(),
            .by = c(Aar, Gr)) %>%
  mutate(andel_klin_vikt = ant_klin_vikt/N*100)

Tabell <- Tabell1 %>%
  tidyr::pivot_wider(id_cols = Gr, names_from = Aar, values_from = andel_klin_vikt) %>%
  select(1,4,3,2)

outfile<-"odi_smertevarighet.svg"
FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                pointsizePDF=11, fargepalett='BlaaOff')
farger <- FigTypUt$farger

xpos <- barplot(as.vector(unlist(Tabell[,4])), col = farger[3], border = F,
                ylim = c(0,1.25*max(Tabell[,-1])), main = "Klinisk viktig endring i ODI",
                ylab = "Andel %")
pktStr <- 1.5
points(y=as.vector(unlist(Tabell[,2])), x=xpos,cex=pktStr) #'#4D4D4D'
points(y=as.vector(unlist(Tabell[,3])), x=xpos,cex=pktStr,pch= 19)
legend('top', cex=0.9*pktStr, bty='n', #bg='white', box.col='white',y=max(ypos),
       lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
       legend=names(Tabell[-1]), ncol = 3)

mtext(c(paste0("N=", Tabell1$N[1]), paste0("N=", Tabell1$N[2])), side = 1, line = -1, at = xpos)
mtext(c("Smertevarighet \u2264 2 år", "Smertevarighet > 2 år"), side = 1, line = 1, at = xpos)

if (outfile != "") {dev.off()}
prop.test(x = Tabell1[1:2, 3], n = Tabell1[1:2, 4],
          alternative = "greater")
##################### Smerte i aktivitet ############################################333

aux <- Oppf_data %>% filter(regstatus_pre == 1 & regstatus_post == 1 & !is.na(PainExperiencesActivity) &
                              !is.na(PainExperiencesActivity_post) & PainExperiencesActivity != 0)

aux$pstEndringSmerteAktiv <- (aux$PainExperiencesActivity - aux$PainExperiencesActivity_post)/aux$PainExperiencesActivity*100

aux$Indikator <- 0
aux$Indikator[aux$pstEndringSmerteAktiv >= 30 ] <- 1

aux <- aux %>% mutate(Gr = ifelse(PainDurationNow != "Mer enn 2", "mindreellerlik2", "str2")) %>%
  dplyr::filter(PainDurationNow != "Ikke svart")

Tabell1 <- aux %>% filter(Aar <= rap_aar & Aar > rap_aar-3) %>%
  summarise(ant_klin_vikt = sum(Indikator),
            N=n(),
            .by = c(Aar, Gr)) %>%
  mutate(andel_klin_vikt = ant_klin_vikt/N*100)

Tabell <- Tabell1 %>%
  tidyr::pivot_wider(id_cols = Gr, names_from = Aar, values_from = andel_klin_vikt) %>%
  select(1,4,3,2)

outfile<-"smerteaktiv_smertevarighet.svg"
FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                pointsizePDF=11, fargepalett='BlaaOff')
farger <- FigTypUt$farger

xpos <- barplot(as.vector(unlist(Tabell[,4])), col = farger[3], border = F,
                ylim = c(0,1.25*max(Tabell[,-1])), main = 'Klinisk viktig bedring av smerte i aktivitet',
                ylab = "Andel %")
pktStr <- 1.5
points(y=as.vector(unlist(Tabell[,2])), x=xpos,cex=pktStr) #'#4D4D4D'
points(y=as.vector(unlist(Tabell[,3])), x=xpos,cex=pktStr,pch= 19)
legend('top', cex=0.9*pktStr, bty='n', #bg='white', box.col='white',y=max(ypos),
       lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
       legend=names(Tabell[-1]), ncol = 3)

mtext(c(paste0("N=", Tabell1$N[1]), paste0("N=", Tabell1$N[2])), side = 1, line = -1, at = xpos)
mtext(c("Smertevarighet \u2264 2 år", "Smertevarighet > 2 år"), side = 1, line = 1, at = xpos)

if (outfile != "") {dev.off()}
prop.test(x = Tabell1[1:2, 3], n = Tabell1[1:2, 4],
          alternative = "greater")


