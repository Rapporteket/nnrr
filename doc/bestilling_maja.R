
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


######## Tall til artikkel #####################################################

rm(list=ls())
library(nnrr)

RegData <- nnrr::nnrrHentRegData()

baseline <- RegData %>%
  dplyr::filter(Aar == 2021,
                PainDurationNow != "Ikke svart") %>%
  mutate(Single = ifelse(FamilyStatus == 'Enslig', 1, 0),
         Single = ifelse(FamilyStatus == 'Ikke svart', NA, Single),
         Gr = ifelse(PainDurationNow != "Mer enn 2", "mindreellerlik2", "str2"))


tabell <- baseline %>%
  summarise(
    antall = dplyr::n(),
    gj.sn.alder = mean(PatientAge, na.rm = TRUE),
    antall.kvinner = sum(ErMann == 0),
    antall.menn = sum(ErMann == 1),
    antall.hoyereutdannet = sum(UtdanningHoy == 1, na.rm = T),
    antall.ikkehoyereutdannet = sum(UtdanningHoy == 0, na.rm = T),
    antall.single = sum(FamilyStatus == 'Enslig'),
    antall.ikkesingle = sum(!(FamilyStatus %in% c('Enslig', 'Ikke svart'))),
    gj.sn.hscl10 = mean(HSCL10.Score, na.rm = T),
    sd.hscl10 = sd(HSCL10.Score, na.rm = T),
    n.hscl10 = sum(!is.na(HSCL10.Score)),
    gj.sn.fabq = mean(FABQScore1, na.rm = T),
    sd.fabq = sd(FABQScore1, na.rm = T),
    gj.sn.odi = mean(OdiScore, na.rm = T),
    sd.odi = sd(OdiScore, na.rm = T),
    gj.sn.eq5d = mean(Score_EQ5DL, na.rm = T),
    sd.eq5d = sd(Score_EQ5DL, na.rm = T),
    gj.sn.nrs.aktiv = mean(PainExperiencesActivity, na.rm = T),
    sd.nrs.aktiv = sd(PainExperiencesActivity, na.rm = T),
    gj.sn.nrs.hvile = mean(PainExperiencesNoActivity, na.rm = T),
    sd.nrs.hvile = sd(PainExperiencesNoActivity, na.rm = T),
    .by = Gr
  ) %>% tr_summarize_output()

tabell <- baseline %>%
  summarise(
    antall = dplyr::n(),
    gj.sn.alder = mean(PatientAge, na.rm = TRUE),
    andel.kvinner = sum(ErMann == 0)/antall*100,
    andel.hoyereutdannet = sum(UtdanningHoy == 1, na.rm = T)/sum(!is.na(UtdanningHoy))*100,
    andel.single = sum(Single == 1, na.rm = T)/sum(!is.na(Single))*100,
    gj.sn.hscl10 = mean(HSCL10.Score, na.rm = T),
    gj.sn.fabq = mean(FABQScore1, na.rm = T),
    gj.sn.odi = mean(OdiScore, na.rm = T),
    gj.sn.eq5d = mean(Score_EQ5DL, na.rm = T),
    gj.sn.nrs.hvile = mean(PainExperiencesNoActivity, na.rm = T),
    gj.sn.nrs.aktiv = mean(PainExperiencesActivity, na.rm = T),
    .by = Gr
  ) %>% tr_summarize_output(kolnavn1 = "variabel")


studydata_num <- baseline %>%
  select(Gr, PatientAge, HSCL10.Score, FABQScore1, OdiScore, Score_EQ5DL,
         PainExperiencesNoActivity, PainExperiencesActivity)

studydata_kat <- baseline %>%
  select(Gr, ErMann, UtdanningHoy, Single) %>%
  mutate(UtdanningHoy = if_else(UtdanningHoy == 1, "Ja", "Nei"),
         Single = if_else(Single == 1, "Ja", "Nei"))

resultat_num <- purrr::map(
  select(studydata_num, -Gr),
  ~t.test(.x ~ Gr , data = studydata_num)
)

resultat_kat <- purrr::map_dfr(
  studydata_kat[,-1],
  ~rstatix::fisher_test(table(.x, studydata_kat$Gr), detailed = TRUE), .id = 'colmn'
)



tmp <- purrr::map(resultat_num, "conf.int") %>% unlist()
konf <- matrix(tmp, nrow = 2, ncol = length(tmp)/2) %>% t()
colnames(konf) <- c("conf.low", "conf.high")
konf <- as.data.frame(konf)
konf$colmn <- names(tmp)[seq(1,length(tmp), 2)] %>%
  substr(1, nchar(.)-1)



tabell_odds <- tabell %>%
  filter(variabel %in% c("andel.kvinner", "andel.hoyereutdannet", "andel.single")) %>%
  bind_cols(resultat_kat %>% select(estimate, conf.low, conf.high, )) %>%
  mutate(oddsratio_95konf = paste0(sprintf("%.2f", estimate), " (",
                                  sprintf("%.2f", conf.low),
                                  "-", sprintf("%.2f", conf.high),
                                  ")"),
         mindreellerlik2 = paste0(sprintf("%.1f", mindreellerlik2), "%"),
         str2 = paste0(sprintf("%.1f", str2), "%")) %>%
  select(variabel, mindreellerlik2, str2, oddsratio_95konf)


tabell_num <- tabell %>%
  filter(!(variabel %in% c("antall", "andel.kvinner",
                           "andel.hoyereutdannet", "andel.single"))) %>%
  bind_cols(konf %>% select(colmn, conf.low, conf.high)) %>%
  rename(conf.low = conf.high,
         conf.high = conf.low) %>%
  mutate(conf.low = -conf.low,
         conf.high = -conf.high) %>%
  mutate(meandiff_95konf = paste0(sprintf("%.2f", str2-mindreellerlik2), " (",
                           sprintf("%.2f", conf.low),
                           "-", sprintf("%.2f", conf.high),
                           ")"),
         mindreellerlik2 = sprintf("%.1f", mindreellerlik2),
         str2 = sprintf("%.1f", str2)) %>%
  select(variabel, mindreellerlik2, str2, meandiff_95konf)

samlet <- bind_rows(tabell_odds, tabell_num)
samlet <- bind_rows(purrr::map_df(tabell[1,], as.character), samlet)

write.csv2(samlet, "~/mydata/nnrr/tabell_maja_mars2024.csv", row.names = F,
           na = "")

xlsx::write.xlsx(samlet, "~/mydata/nnrr/tabell_maja_mars2024.xlsx", showNA = F)




varnavn <- names(tmp)[seq(1,length(tmp), 2)] %>%
  substr(1, nchar(.)-1)

substr(varnavn, 1, nchar(varnavn)-1)

baseline %>%
  mutate(Smrt_2plus = ifelse(PainDurationNow != "Mer enn 2", 0, 1)) %>%
  dplyr::select(ErMann, Smrt_2plus) %>%
  table() %>% fisher.test()



# count(ErMann, Smrt_2plus)






# Oppf_data <- RegData
# Oppf_data$Aar <- Oppf_data$aar_oppfolg
#
# aux <- Oppf_data[which(Oppf_data$regstatus==1 & Oppf_data$regstatus_post==1), ]
# aux <- aux[!is.na(aux$OdiScore) & !is.na(aux$OdiScore_post), ]
# aux$odidiff_klin_viktig <- 0
# aux$odidiff_klin_viktig[(aux$OdiScore - aux$OdiScore_post)/aux$OdiScore >= .3] <- 1
#
# aux <- aux %>% mutate(Gr = ifelse(HSCL10.Score >= 1.85, "StrLik", "Mindre")) %>%
#   dplyr::filter(!is.na(Gr))
#
# Tabell1 <- aux %>% filter(Aar <= rap_aar & Aar > rap_aar-3) %>%
#   summarise(ant_klin_vikt = sum(odidiff_klin_viktig),
#             N=n(),
#             .by = c(Aar, Gr)) %>%
#   mutate(andel_klin_vikt = ant_klin_vikt/N*100)
#
# Tabell <- Tabell1 %>%
#   tidyr::pivot_wider(id_cols = Gr, names_from = Aar, values_from = andel_klin_vikt) %>%
#   select(1,4,3,2)
#
#
# tabell <- baseline %>%
#   summarise(
#     antall = dplyr::n(),
#     gj.sn.alder = mean(PatientAge, na.rm = TRUE),
#     andel.kvinner = sum(ErMann == 0)/antall*100,
#     .by = Gr
#   )
#
#
#
#






