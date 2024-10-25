rm(list=ls())
library(nnrr)
library(dplyr)
library(tidyr)

figfolder <- "~/mydata/nnrr/"

RegData <- nnrr::nnrrHentRegData()

RegData <- RegData %>%
  dplyr::filter(Aar == 2023) %>%
  dplyr::mutate(
    Kjønn = factor(ErMann, levels = 0:1, labels = c("Kvinne", "Mann")),
    pstEndringSmerteHvile = (PainExperiencesNoActivity -
                               PainExperiencesNoActivity_post)/
      PainExperiencesNoActivity*100,
    pstEndringSmerteAktiv = (PainExperiencesActivity -
                               PainExperiencesActivity_post)/
      PainExperiencesActivity*100)
oppsum <- RegData %>%
  dplyr::summarise(
    Antall = n(),
    gjsn_PainExperiencesActivity = mean(PainExperiencesActivity, na.rm = T),
    gjsn_PainExperiencesNoActivity = mean(PainExperiencesNoActivity, na.rm = T),
    gjsn_OdiScore = mean(OdiScore, na.rm = T),
    andel_hcsl10_str1.85 = sum(HSCL10Score > 1.85, na.rm = T)/sum(!is.na(HSCL10Score))*100,
    klin_bedring_funksjon_6mnd =
      sum(regstatus==1 & regstatus_post==1 &
            !is.na(OdiScore) & !is.na(OdiScore_post) & OdiScore != 0 &
            ((OdiScore - OdiScore_post)/OdiScore >= .3))/
      sum(regstatus==1 & regstatus_post==1 & OdiScore != 0 &
            !is.na(OdiScore) & !is.na(OdiScore_post))*100,
    minimal_funksjonsnedsettelse_6mnd = sum(
      regstatus==1 &
        regstatus_post==1 &
        !is.na(OdiScore) &
        !is.na(OdiScore_post) &
        (OdiScore_post <= 23))/
      sum(regstatus==1 &
            regstatus_post==1 &
            !is.na(OdiScore) &
            !is.na(OdiScore_post))*100,
    klin_viktig_EndringSmerteHvile =
      sum((regstatus_pre == 1 & regstatus_post == 1 &
             !is.na(PainExperiencesNoActivity) &
             !is.na(PainExperiencesNoActivity_post) &
             (PainExperiencesNoActivity != 0)) & (pstEndringSmerteHvile >= 30))/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            !is.na(PainExperiencesNoActivity) &
            !is.na(PainExperiencesNoActivity_post) &
            (PainExperiencesNoActivity != 0))*100,
    klin_viktig_EndringSmerteAktiv =
      sum((regstatus_pre == 1 & regstatus_post == 1 &
             !is.na(PainExperiencesActivity) &
             !is.na(PainExperiencesActivity_post) &
             (PainExperiencesActivity != 0)) & (pstEndringSmerteAktiv >= 30))/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            !is.na(PainExperiencesActivity) &
            !is.na(PainExperiencesActivity_post) &
            (PainExperiencesActivity != 0))*100,
    andel_tilbake_jobb =
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            IsEmployed == 1 & S2_SickLeave & S2_Working_post &
            (!S2_SickLeave_post & !S2_NAV_post))/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            IsEmployed == 1 & S2_SickLeave)*100,
    andel_pasrapportert_bedring =
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            UseOfTreatment %in% 1:3)/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            UseOfTreatment %in% 1:7)*100,
    andel_fornoyd =
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction) &
            TreatmentSatisfaction %in% 1:3)/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction))*100,
    andel_tverrfaglig =
      sum(regstatus==1 & (Treatment_GroupInterdisciplinary2018 != 0 |
                            Treatment_GroupInterdisciplinary != 0 |
                            Treatment_InvidualInterdisciplinary != 0))/
      sum(regstatus==1)*100,
    .by = Kjønn
  ) %>% tr_summarize_output(kolnavn1 = "Variabel")

oppsum$pverdi <- NA
oppsum$pverdi[oppsum$Variabel == "gjsn_PainExperiencesActivity"] <-
  t.test(RegData %>% filter(ErMann==0) %>% select(PainExperiencesActivity),
         RegData %>% filter(ErMann==1) %>% select(PainExperiencesActivity))$p.value
oppsum$pverdi[oppsum$Variabel == "gjsn_PainExperiencesNoActivity"] <-
  t.test(RegData %>% filter(ErMann==0) %>% select(PainExperiencesNoActivity),
         RegData %>% filter(ErMann==1) %>% select(PainExperiencesNoActivity))$p.value
oppsum$pverdi[oppsum$Variabel == "gjsn_OdiScore"] <-
  t.test(RegData %>% filter(ErMann==0) %>% select(OdiScore),
         RegData %>% filter(ErMann==1) %>% select(OdiScore))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(HSCL10Score > 1.85 ~ 1,
                            HSCL10Score <= 1.85 ~ 0,
                            .default = NA))
oppsum$pverdi[oppsum$Variabel == "andel_hcsl10_str1.85"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(
    regstatus==1 & regstatus_post==1 &
      !is.na(OdiScore) & !is.na(OdiScore_post) & OdiScore != 0 &
      ((OdiScore - OdiScore_post)/OdiScore >= .3) ~ 1,
    regstatus==1 & regstatus_post==1 &
      !is.na(OdiScore) & !is.na(OdiScore_post) & OdiScore != 0 &
      ((OdiScore - OdiScore_post)/OdiScore < .3) ~ 0,
    .default = NA
  ))
oppsum$pverdi[oppsum$Variabel == "klin_bedring_funksjon_6mnd"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(regstatus==1 &
                              regstatus_post==1 &
                              !is.na(OdiScore) &
                              !is.na(OdiScore_post) &
                              (OdiScore_post <= 23) ~ 1,
                            regstatus==1 &
                              regstatus_post==1 &
                              !is.na(OdiScore) &
                              !is.na(OdiScore_post) &
                              (OdiScore_post > 23) ~ 0,
                            .default = NA))
oppsum$pverdi[oppsum$Variabel == "minimal_funksjonsnedsettelse_6mnd"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value


RegData <- RegData %>%
  mutate(tmpvar = case_when(
    (regstatus_pre == 1 & regstatus_post == 1 &
       !is.na(PainExperiencesNoActivity) &
       !is.na(PainExperiencesNoActivity_post) &
       (PainExperiencesNoActivity != 0)) & (pstEndringSmerteHvile >= 30) ~ 1,
    (regstatus_pre == 1 & regstatus_post == 1 &
       !is.na(PainExperiencesNoActivity) &
       !is.na(PainExperiencesNoActivity_post) &
       (PainExperiencesNoActivity != 0)) & (pstEndringSmerteHvile < 30) ~ 0,
    .default = NA))
oppsum$pverdi[oppsum$Variabel == "klin_viktig_EndringSmerteHvile"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(
    (regstatus_pre == 1 & regstatus_post == 1 &
       !is.na(PainExperiencesActivity) &
       !is.na(PainExperiencesActivity_post) &
       (PainExperiencesActivity != 0)) & (pstEndringSmerteAktiv >= 30) ~ 1,
    (regstatus_pre == 1 & regstatus_post == 1 &
       !is.na(PainExperiencesActivity) &
       !is.na(PainExperiencesActivity_post) &
       (PainExperiencesActivity != 0)) & (pstEndringSmerteAktiv < 30) ~ 0,
    .default = NA))
oppsum$pverdi[oppsum$Variabel == "klin_viktig_EndringSmerteAktiv"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(
    regstatus_pre == 1 & regstatus_post == 1 &
      IsEmployed == 1 & S2_SickLeave & S2_Working_post &
      (!S2_SickLeave_post & !S2_NAV_post) ~ 1,
    regstatus_pre == 1 & regstatus_post == 1 &
      IsEmployed == 1 & S2_SickLeave & (!S2_Working_post |
                                          S2_SickLeave_post | S2_NAV_post) ~ 0,
    .default = NA))
oppsum$pverdi[oppsum$Variabel == "andel_tilbake_jobb"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(
    regstatus_pre == 1 & regstatus_post == 1 &
      UseOfTreatment %in% 1:3 ~ 1,
    regstatus_pre == 1 & regstatus_post == 1 &
      UseOfTreatment %in% 4:7 ~ 0,
    .default = NA))
oppsum$pverdi[oppsum$Variabel == "andel_pasrapportert_bedring"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = case_when(
    regstatus_pre == 1 & regstatus_post == 1 &
      TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction) &
      TreatmentSatisfaction %in% 1:3 ~ 1,
    regstatus_pre == 1 & regstatus_post == 1 &
      TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction) &
      TreatmentSatisfaction %in% 4:5 ~ 0,
    .default = NA))
oppsum$pverdi[oppsum$Variabel == "andel_fornoyd"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value

RegData <- RegData %>%
  mutate(tmpvar = if_else(
    regstatus==1 &
      (Treatment_GroupInterdisciplinary2018 != 0 |
         Treatment_GroupInterdisciplinary != 0 |
         Treatment_InvidualInterdisciplinary != 0), 1, 0))
oppsum$pverdi[oppsum$Variabel == "andel_tverrfaglig"] <-
  fisher.test(table(RegData$tmpvar, RegData$ErMann))$p.value


write.csv2(oppsum, "~/mydata/nnrr/majatall_okt2024.csv", row.names = F)

plotdata <- RegData %>%
  dplyr::filter(Aar == 2023) %>%
  dplyr::mutate(
    Kjønn = factor(ErMann, levels = 0:1, labels = c("Kvinne", "Mann")),
    pstEndringSmerteHvile = (PainExperiencesNoActivity -
                               PainExperiencesNoActivity_post)/
      PainExperiencesNoActivity*100,
    pstEndringSmerteAktiv = (PainExperiencesActivity -
                               PainExperiencesActivity_post)/
      PainExperiencesActivity*100) %>%
  dplyr::summarise(
    Antall = n(),
    gjsn_PainExperiencesActivity = mean(PainExperiencesActivity, na.rm = T),
    N_PainExperiencesActivity = sum(!is.na(PainExperiencesActivity)),
    gjsn_PainExperiencesNoActivity = mean(PainExperiencesNoActivity, na.rm = T),
    N_PainExperiencesNoActivity = sum(!is.na(PainExperiencesNoActivity)),
    gjsn_OdiScore = mean(OdiScore, na.rm = T),
    N_OdiScore = sum(!is.na(OdiScore)),
    andel_hcsl10_str1.85 = sum(HSCL10Score > 1.85, na.rm = T)/sum(!is.na(HSCL10Score))*100,
    N_hcsl10_str1.85 = sum(!is.na(HSCL10Score)),
    klin_bedring_funksjon_6mnd =
      sum(regstatus==1 & regstatus_post==1 &
            !is.na(OdiScore) & !is.na(OdiScore_post) & OdiScore != 0 &
            ((OdiScore - OdiScore_post)/OdiScore >= .3))/
      sum(regstatus==1 & regstatus_post==1 & OdiScore != 0 &
            !is.na(OdiScore) & !is.na(OdiScore_post))*100,
    N_klin_bedring_funksjon_6mnd = sum(regstatus==1 & regstatus_post==1 & OdiScore != 0 &
                                         !is.na(OdiScore) & !is.na(OdiScore_post)),
    minimal_funksjonsnedsettelse_6mnd = sum(
      regstatus==1 &
        regstatus_post==1 &
        !is.na(OdiScore) &
        !is.na(OdiScore_post) &
        (OdiScore_post <= 23))/
      sum(regstatus==1 &
            regstatus_post==1 &
            !is.na(OdiScore) &
            !is.na(OdiScore_post))*100,
    N_minimal_funksjonsnedsettelse_6mnd = sum(regstatus==1 &
                                                regstatus_post==1 &
                                                !is.na(OdiScore) &
                                                !is.na(OdiScore_post)),
    klin_viktig_EndringSmerteHvile =
      sum((regstatus_pre == 1 & regstatus_post == 1 &
             !is.na(PainExperiencesNoActivity) &
             !is.na(PainExperiencesNoActivity_post) &
             (PainExperiencesNoActivity != 0)) & (pstEndringSmerteHvile >= 30))/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            !is.na(PainExperiencesNoActivity) &
            !is.na(PainExperiencesNoActivity_post) &
            (PainExperiencesNoActivity != 0))*100,
    N_klin_viktig_EndringSmerteHvile = sum(regstatus_pre == 1 & regstatus_post == 1 &
                                             !is.na(PainExperiencesNoActivity) &
                                             !is.na(PainExperiencesNoActivity_post) &
                                             (PainExperiencesNoActivity != 0)),
    klin_viktig_EndringSmerteAktiv =
      sum((regstatus_pre == 1 & regstatus_post == 1 &
             !is.na(PainExperiencesActivity) &
             !is.na(PainExperiencesActivity_post) &
             (PainExperiencesActivity != 0)) & (pstEndringSmerteAktiv >= 30))/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            !is.na(PainExperiencesActivity) &
            !is.na(PainExperiencesActivity_post) &
            (PainExperiencesActivity != 0))*100,
    N_klin_viktig_EndringSmerteAktiv = sum(regstatus_pre == 1 & regstatus_post == 1 &
                                             !is.na(PainExperiencesActivity) &
                                             !is.na(PainExperiencesActivity_post) &
                                             (PainExperiencesActivity != 0)),
    andel_tilbake_jobb =
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            IsEmployed == 1 & S2_SickLeave & S2_Working_post &
            (!S2_SickLeave_post & !S2_NAV_post))/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            IsEmployed == 1 & S2_SickLeave)*100,
    N_andel_tilbake_jobb = sum(regstatus_pre == 1 & regstatus_post == 1 &
                                 IsEmployed == 1 & S2_SickLeave),
    andel_pasrapportert_bedring =
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            UseOfTreatment %in% 1:3)/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            UseOfTreatment %in% 1:7)*100,
    N_pasrapportert_bedring = sum(regstatus_pre == 1 & regstatus_post == 1 &
                                    UseOfTreatment %in% 1:7),
    andel_fornoyd =
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction) &
            TreatmentSatisfaction %in% 1:3)/
      sum(regstatus_pre == 1 & regstatus_post == 1 &
            TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction))*100,
    N_fornoyd =sum(regstatus_pre == 1 & regstatus_post == 1 &
                     TreatmentSatisfaction != 0 & !is.na(TreatmentSatisfaction)),
    andel_tverrfaglig =
      sum(regstatus==1 & (Treatment_GroupInterdisciplinary2018 != 0 |
                            Treatment_GroupInterdisciplinary != 0 |
                            Treatment_InvidualInterdisciplinary != 0))/
      sum(regstatus==1)*100,
    N_tverrfagling = sum(regstatus==1),
    .by = Kjønn
  )



plotkjonnsgreier <- function(plotvektor, tittel, outfile="",
                             maal = NA, minstekrav = NA, maalretn="hoy",
                             xlab = 'Andel (%)', vidde=1000, hoyde=700,
                             inkl_pst = TRUE) {
  figinfo <- rapFigurer::figtype(outfile = outfile, fargepalett = "BlaaRapp",
                                 width = vidde, height = hoyde)
  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')
  xmax <- max(plotvektor)*1.2
  ypos <- barplot( plotvektor, beside=T, las=1, xlim = c(0,xmax),
                   horiz=T, main = tittel,
                   col=figinfo$farger[3], border=NA,
                   xlab = xlab)
  if (inkl_pst) {solyetext <- paste0(round(plotvektor, 1), "%")
  } else {
    solyetext <- paste0(round(plotvektor, 1))
  }
  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=0, xright=maal, ytop=max(ypos)+1,
         col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=0, xright=min(xmax, 100), ytop=max(ypos)+1,
         col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+1,
         col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=0, xright=maal, ytop=max(ypos)+1,
         col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    rect(xleft=0, ybottom=0, xright=maal, ytop=max(ypos)+1,
         col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    rect(xleft=maal, ybottom=0, xright=min(xmax, 100), ytop=max(ypos)+1,
         col = fargerMaalNiva[1], border = NA)}
  barplot( plotvektor, beside=T, las=1, xlim = c(0,xmax),
           horiz=T, main = tittel,
           col=figinfo$farger[3], border=NA,
           xlab = xlab, add = TRUE)
  text(x = plotvektor, y = ypos,
       labels = solyetext, adj = 0)
  if (outfile!="") {dev.off()}
}

outfile = paste0(figfolder, "andel_tverrfaglig.pdf")
plotvektor <- plotdata$andel_tverrfaglig
maal = 30
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_tverrfagling)
tittel <- "Andel tverrfaglig behandlet"
plotkjonnsgreier(plotvektor, tittel, maal = maal, outfile = outfile)

outfile = paste0(figfolder, "smerte_aktiv.pdf")
plotvektor <- plotdata$gjsn_PainExperiencesActivity
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_PainExperiencesActivity)
tittel <- "Gj.sn. smerte i aktivitet"
plotkjonnsgreier(plotvektor, tittel, outfile = outfile,
                 xlab = "Gj.sn. score", inkl_pst = FALSE)

outfile = paste0(figfolder, "smerte_hvile.pdf")
plotvektor <- plotdata$gjsn_PainExperiencesNoActivity
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_PainExperiencesNoActivity)
tittel <- "Gj.sn. smerte i hvile"
plotkjonnsgreier(plotvektor, tittel, outfile = outfile,
                 xlab = "Gj.sn. score", inkl_pst = FALSE)

outfile = paste0(figfolder, "odi.pdf")
plotvektor <- plotdata$gjsn_OdiScore
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_OdiScore)
tittel <- "Gj.sn. ODI"
plotkjonnsgreier(plotvektor, tittel, outfile = outfile,
                 xlab = "Gj.sn. score", inkl_pst = FALSE)

outfile = paste0(figfolder, "hscl185.pdf")
plotvektor <- plotdata$andel_hcsl10_str1.85
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_hcsl10_str1.85)
tittel <- "Andel med HSCL >= 1.85"
plotkjonnsgreier(plotvektor, tittel, outfile = outfile)

outfile = paste0(figfolder, "bedring_funksjon.pdf")
plotvektor <- plotdata$klin_bedring_funksjon_6mnd
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_klin_bedring_funksjon_6mnd)
tittel <- "Andel med klinisk bedring i funksjon"
plotkjonnsgreier(plotvektor, tittel, outfile = outfile)

outfile = paste0(figfolder, "min_funk_nedsettelse.pdf")
plotvektor <- plotdata$minimal_funksjonsnedsettelse_6mnd
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_minimal_funksjonsnedsettelse_6mnd)
tittel="Andel med minimal funksjonsnedsettelse"
maal = 30; minstekrav = 25
plotkjonnsgreier(plotvektor, tittel, maal=maal,
                 minstekrav=minstekrav, outfile = outfile)

outfile = paste0(figfolder, "bedring_smerte_hvile.pdf")
plotvektor <- plotdata$klin_viktig_EndringSmerteHvile
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_klin_viktig_EndringSmerteHvile)
tittel="Klinisk viktig bedring av smerte i hvile"
maal = 30; minstekrav = 25
plotkjonnsgreier(plotvektor, tittel, maal=maal,
                 minstekrav=minstekrav, outfile = outfile)

outfile = paste0(figfolder, "bedring_smerte_aktiv.pdf")
plotvektor <- plotdata$klin_viktig_EndringSmerteAktiv
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_klin_viktig_EndringSmerteAktiv)
tittel="Klinisk viktig bedring av smerte i aktivitet"
maal = 30; minstekrav = 25
plotkjonnsgreier(plotvektor, tittel, maal=maal,
                 minstekrav=minstekrav, outfile = outfile)

outfile = paste0(figfolder, "jobb.pdf")
plotvektor <- plotdata$andel_tilbake_jobb
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_andel_tilbake_jobb)
tittel="Andel tilbake i arbeid"
maal = 30; minstekrav = 25
plotkjonnsgreier(plotvektor, tittel, maal=maal,
                 minstekrav=minstekrav, outfile = outfile)

outfile = paste0(figfolder, "pasrapp_bedring.pdf")
plotvektor <- plotdata$andel_pasrapportert_bedring
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_pasrapportert_bedring)
tittel="Pasientopplevd bedring av behandling"
maal = 30; minstekrav = 25
plotkjonnsgreier(plotvektor, tittel, maal=maal,
                 minstekrav=minstekrav, outfile = outfile)

outfile = paste0(figfolder, "fornoyd.pdf")
plotvektor <- plotdata$andel_fornoyd
names(plotvektor) <- paste0(c("Mann", "Kvinne"), "\n N=",
                            plotdata$N_fornoyd)
tittel="Andel fornøyd med eller nøytral til behandling"
maal = 90; minstekrav = 80
plotkjonnsgreier(plotvektor, tittel, maal=maal,
                 minstekrav=minstekrav, outfile = outfile)

kjonn <- plotdata$Antall
names(kjonn) <- paste0(c("Menn", "Kvinner"), "\n", plotdata$Antall)

rapFigurer::figtype(outfile = paste0(figfolder, "kake.pdf"), fargepalett = "BlaaRapp")
pie(kjonn, col = c("blue", "red"), clockwise = T, main = "Kjønnsfordeling")
dev.off()

outfile = paste0(figfolder, "kjonn.pdf")
plotvektor <- kjonn/sum(kjonn)*100
tittel <- "Kjønnsfordeling"
plotkjonnsgreier(plotvektor, tittel, outfile = outfile)


