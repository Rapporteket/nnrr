rm(list=ls())
library(nnrr)
library(dplyr)
library(tidyr)

RegData <- nnrr::nnrrHentRegData()


oppsum <- RegData %>%
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
    gjsn_PainExperiencesNoActivity = mean(PainExperiencesNoActivity, na.rm = T),
    gjsn_OdiScore = mean(OdiScore, na.rm = T),
    andel_hcsl10_str1.85 = sum(HSCL10Score > 1.85, na.rm = T)/sum(!is.na(HSCL10Score))*100,
    klin_bedring_funksjon_6mnd = sum(
      regstatus==1 &
        regstatus_post==1 &
        !is.na(OdiScore) &
        !is.na(OdiScore_post) &
        ((OdiScore - OdiScore_post)/OdiScore >= .3))/
      sum(regstatus==1 &
            regstatus_post==1 &
            !is.na(OdiScore) &
            !is.na(OdiScore_post))*100,
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
    .by = Kjønn
  )




indikator <- RegData[which(RegData$regstatus==1 & RegData$regstatus_post==1), ]

indikator <- indikator[indikator$UseOfTreatment %in% 1:7, ]
indikator$NytteAvBehandling <- 0
indikator$NytteAvBehandling[indikator$UseOfTreatment %in% 1:3] <- 1









