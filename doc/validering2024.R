rm(list=ls())
library(nnrr)
library(dplyr)
library(tidyr)

# RegData <- nnrr::nnrrHentRegData()
pasientsvar_pre <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Pasientskjema+før+behandling_2024-06-04_0904.csv')
legeskjema <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Behandlerskjema_2024-06-04_0904.csv')
fnrfil <- readr::read_csv2(
  '~/mydata/nnrr/validering2024/DataDump_MRS-PROD_Behandlerskjema_2024-04-12_1532.csv') %>%
  dplyr::select(Fødselsnummer, PasientGUID) %>%
  summarise(FNR = first(Fødselsnummer), .by = PasientGUID)
pasientsvar_post <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Pasientskjema+6+måneder+etter+behandling_2024-06-04_0904.csv')
pasientsvar_post2 <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Pasientskjema+12+måneder+etter+behandling_2024-06-04_0905.csv')
filfolder <- "~/mydata/nnrr/validering2024/"
fil1 <- paste0(filfolder, "Variabelliste.xlsx")

### Trekk ut liste over pasienter:

pasientsvar_post <- pasientsvar_post %>%
  mutate(LastUpdate = as.Date(LastUpdate, format="%d.%m.%Y")) %>%
  filter(LastUpdate == last(LastUpdate),
         SkjemaGUID == first(SkjemaGUID),
         .by = HovedskjemaGUID)
pasientsvar_post2 <- pasientsvar_post2 %>%
  mutate(LastUpdate = as.Date(LastUpdate, format="%d.%m.%Y")) %>%
  filter(LastUpdate == last(LastUpdate),
         SkjemaGUID == first(SkjemaGUID),
         .by = HovedskjemaGUID)

kobletdata <- merge(legeskjema[, c("PasientGUID", "SkjemaGUID", "FormStatus",
                                   "FormDate", "S1b_DateOfCompletion")],
                    pasientsvar_pre[, c("HovedskjemaGUID", "FormStatus")],
                    by.x = "SkjemaGUID", by.y = "HovedskjemaGUID",
                    suffixes = c('', '_pre')) %>%
  dplyr::mutate(S1b_DateOfCompletion = as.Date(S1b_DateOfCompletion, format="%d.%m.%Y")) %>%
  dplyr::filter(S1b_DateOfCompletion <= "2023-06-30",
                S1b_DateOfCompletion >= "2022-07-01") %>%
  merge(pasientsvar_post[, c("HovedskjemaGUID", "FormStatus")],
        by.x = "SkjemaGUID", by.y = "HovedskjemaGUID",
        suffixes = c('', '_post1'), all.x = TRUE) %>%
  merge(pasientsvar_post2[, c("HovedskjemaGUID", "FormStatus")],
        by.x = "SkjemaGUID", by.y = "HovedskjemaGUID",
        suffixes = c('', '_post2'), all.x = TRUE)


mangleroppf <- kobletdata %>%
  mutate(FormStatus_post1 = as.numeric(!is.na(FormStatus_post1)),
         FormStatus_post2 = as.numeric(!is.na(FormStatus_post2))) %>%
  select(SkjemaGUID, PasientGUID, S1b_DateOfCompletion, FormStatus_post1, FormStatus_post2) %>%
  dplyr::filter((FormStatus_post1 == 0) | (FormStatus_post2 == 0)) %>%
  merge(fnrfil, by = "PasientGUID") %>%
  select(FNR, S1b_DateOfCompletion, FormStatus_post1, FormStatus_post2) %>%
  rename(FormStatus_6mnd =FormStatus_post1,
         FormStatus_12mnd =FormStatus_post2)

write.csv2(mangleroppf, "~/mydata/nnrr/validering2024/nnrr_mangleroppf_validering2024.csv",
           row.names = F, fileEncoding = "Latin1")




################################################################################


varliste_forbehandl <- readxl::read_xlsx(fil1, sheet = 1)
varliste_behandler <- readxl::read_xlsx(fil1, sheet = 2)
kodebok_pas <- readxl::read_xlsx("~/mydata/nnrr/kodebok_nnrr_feb2024.xlsx", sheet = 6)
kodebok_beh <- readxl::read_xlsx("~/mydata/nnrr/kodebok_nnrr_feb2024.xlsx", sheet = 7)
kodebok_verdier <- dplyr::bind_rows(kodebok_pas, kodebok_beh) %>%
  tidyr::separate(col = `Mulige verdier`, into = c("value", "label"), sep = " = ") %>%
  dplyr::select(Variabelnavn, Felttype, value, label)

# which(!is.na(kodebok_verdier$Variabelnavn))
# which(diff(which(!is.na(kodebok_verdier$Variabelnavn))) > 1)


# 1. index

index1 <- which(!is.na(kodebok_verdier$Variabelnavn))[which(diff(which(!is.na(kodebok_verdier$Variabelnavn))) > 1)]
intervall <- diff(which(!is.na(kodebok_verdier$Variabelnavn)))[diff(which(!is.na(kodebok_verdier$Variabelnavn)))>1]

indeksering <- data.frame(indeks1 = (index1 + 1), indeks2 = (index1 + intervall - 1))

for (i in 1:dim(indeksering)[1]) {
  kodebok_verdier$Variabelnavn[indeksering$indeks1[i]:indeksering$indeks2[i]] <-
    kodebok_verdier$Variabelnavn[index1[i]]
  kodebok_verdier$Felttype[indeksering$indeks1[i]:indeksering$indeks2[i]] <-
    kodebok_verdier$Felttype[index1[i]]
}

kodebok <- dplyr::bind_rows(kodebok_pas, kodebok_beh) %>%
  dplyr::filter(!is.na(Felttype)) %>% dplyr::select(Variabelnavn, Felttype)

forbehandl <- c("PasientGUID", "HovedskjemaGUID", "S1b_DateOfCompletion",
                unique(varliste_forbehandl$Variabelnavn))
forbehandl <- forbehandl[!is.na(forbehandl)]

behandl <- c("SkjemaGUID", "S1b_DateOfCompletion", "PatientAge",
             unique(varliste_behandler$Variabelnavn))
behandl <- behandl[!is.na(behandl)]

data_forbehandl <- pasientsvar_pre[, forbehandl] %>%
  mutate(Dato = as.Date(S1b_DateOfCompletion, format="%d.%m.%Y")) %>%
  dplyr::filter(Dato >= "2021-01-01",
                Dato <= "2023-06-30") %>%
  dplyr::select(-c(S1b_DateOfCompletion))
data_behandl <- legeskjema[, behandl] %>%
  mutate(Dato = as.Date(S1b_DateOfCompletion, format="%d.%m.%Y")) %>%
  dplyr::filter(Dato >= "2021-01-01",
                Dato <= "2023-06-30") %>%
  dplyr::select(-c(Dato, S1b_DateOfCompletion))

data_pre <- merge(data_forbehandl, data_behandl, by.x = "HovedskjemaGUID",
                  by.y = "SkjemaGUID", suffixes = c("_pas", "_beh"))

pasientsvar_post6mnd <- pasientsvar_post[, c("HovedskjemaGUID", "FormDate", "DateOfCompletion")]

data_pre <- merge(data_pre, pasientsvar_post6mnd,
                  by = "HovedskjemaGUID",
                  all.x = TRUE) %>%
  dplyr::mutate(Oppfolging = ifelse(is.na(DateOfCompletion), "Mangler_oppf", "Har_oppf"))

katvar <- names(data_pre)[names(data_pre) %in%
                            kodebok$Variabelnavn[kodebok$Felttype %in% c("Avkrysning", "Enkeltvalg")]]
ikkekat <- names(data_pre)[!(names(data_pre) %in%
                               kodebok$Variabelnavn[kodebok$Felttype %in% c("Avkrysning", "Enkeltvalg")])]

oppsummert <- data_pre %>%
  pivot_longer(all_of(katvar),
               names_to = "variable", values_to = "value") %>%
  count(Oppfolging, variable, value) %>%
  pivot_wider(names_from = Oppfolging, values_from = n) %>%
  mutate(har_oppf_prosent = Har_oppf/sum(data_pre$Oppfolging=="Har_oppf")*100,
         mangler_oppf_prosent = Mangler_oppf/sum(data_pre$Oppfolging=="Mangler_oppf")*100)



oppsummert <- merge(oppsummert, kodebok_verdier[, c("Variabelnavn", "value", "label", "Felttype")],
                    by.x = c("variable", "value"),
                    by.y = c("Variabelnavn", "value"),
                    all.x = TRUE) %>%
  dplyr::select(variable, value, label, Har_oppf, Mangler_oppf, har_oppf_prosent,
                mangler_oppf_prosent, Felttype) %>%
  dplyr::mutate(Felttype = ifelse(is.na(Felttype), "Avkrysning", Felttype))


numvar <- names(data_pre)[(names(data_pre) %in%
                             kodebok$Variabelnavn[kodebok$Felttype %in% c("Tall")])]

res <- t.test(Eq5d_L_spm6 ~ Oppfolging, data = data_pre)
# oppsum_stat <- list(
#   gj.sn = ~mean(.x, na.rm = TRUE),
#   std.avvik = ~sd(.x, na.rm = TRUE),
#   n = ~sum(!is.na(.x))
# )


oppsummert_num1 <- data_pre %>%
  summarise(across(all_of(numvar), \(x) mean(x, na.rm = TRUE)), .by = Oppfolging) %>%
  pivot_longer(cols = numvar) %>%
  pivot_wider(names_from = Oppfolging, values_from = value)

oppsummert_num2 <- data_pre %>%
  summarise(across(all_of(numvar), \(x) sd(x, na.rm = TRUE)), .by = Oppfolging) %>%
  pivot_longer(cols = numvar) %>%
  pivot_wider(names_from = Oppfolging, values_from = value)

oppsummert_num3 <- data_pre %>%
  summarise(across(all_of(numvar), \(x) sum(!is.na(x))), .by = Oppfolging) %>%
  pivot_longer(cols = numvar) %>%
  pivot_wider(names_from = Oppfolging, values_from = value) %>%
  rename(Har_oppf_n = Har_oppf,
         Mangler_oppf_n = Mangler_oppf)


oppsummert_num <- merge(oppsummert_num1, oppsummert_num2, by = "name",
                        suffixes = c("_gjsn", "_stdav")) %>%
  merge(oppsummert_num3, by = "name") #%>%
  # mutate(std_pooled = sqrt(((Har_oppf_n-1)*Har_oppf_stdav^2 +
  #          (Mangler_oppf_n-1)*Mangler_oppf_stdav^2)/(Har_oppf_n+Mangler_oppf_n-2)),
  #        t1 = (Har_oppf_gjsn-Mangler_oppf_gjsn)/
  #          (std_pooled*sqrt(1/Har_oppf_n + 1/Mangler_oppf_n)),
  #        t2 = (Har_oppf_gjsn-Mangler_oppf_gjsn)/
  #          sqrt(Har_oppf_stdav^2/Har_oppf_n + Mangler_oppf_stdav^2/Mangler_oppf_n),
  #        df1 = Har_oppf_n + Mangler_oppf_n -2,
  #        df2 = (Har_oppf_stdav^2/Har_oppf_n + Mangler_oppf_stdav^2/Mangler_oppf_n)^2/
  #          ((Har_oppf_stdav^2/Har_oppf_n)^2/(Har_oppf_n-1) +
  #             (Mangler_oppf_stdav^2/Mangler_oppf_n)^2/(Mangler_oppf_n-1)),
  #        p1 = 2*pt(q = abs(t1), df = df1, lower.tail = FALSE),
  #        p2 = 2*pt(q = abs(t2), df = df2, lower.tail = FALSE)
  #        )

for (m in 1:length(oppsummert_num$name)) {
  res <- t.test( data_pre %>% filter(Oppfolging == "Har_oppf") %>%
                   select(oppsummert_num$name[m]),
                 data_pre %>% filter(Oppfolging == "Mangler_oppf") %>%
                   select(oppsummert_num$name[m]))
  # oppsummert_num$meandiff[m] <- res$statistic
  oppsummert_num$p.verdi[m] <- res$p.value
  oppsummert_num$konfint_lav[m] <- res$conf.int[1]
  oppsummert_num$konfint_hoy[m] <- res$conf.int[2]
}

###### Slå sammen kategorier ###################################################



slaasammen <- read.csv2("~/mydata/nnrr/validering2024/oppsummering_kategoriske_redigert_paindurationnow.csv",
                        fileEncoding = "Latin1") %>%
  filter(variable != "") %>%
  mutate(label = case_when(
    Felttype == "Avkrysning" & value == 0 ~ "Nei",
    Felttype == "Avkrysning" & value == 1 ~ "Ja",
    !is.na(label) ~ label
  ))

tmp <- slaasammen %>% filter(label != "Ikke utfylt" | is.na(label),
                             label != "Ukjent" | is.na(label))
dikotome <- tmp %>% filter(Slaa_sammen == "")



slaattsammen <- slaasammen %>% filter(Slaa_sammen != "") %>%
  summarise(label = paste0(label, collapse = ", "),
            Har_oppf = sum(Har_oppf),
            Mangler_oppf = sum(Mangler_oppf),
            Felttype = Felttype[1],
            .by = c(variable, Slaa_sammen))

samlet <- dplyr::bind_rows(dikotome, slaattsammen)

Utflatet <- merge(samlet[seq(1, nrow(samlet), 2), ], samlet[seq(2, nrow(samlet), 2), ],
                  by = "variable", suffixes = c("_gr1", "_gr2"))


for (m in 1:dim(Utflatet)[1]){
  aux <- matrix(c(Utflatet$Har_oppf_gr1[m], Utflatet$Mangler_oppf_gr1[m],
                  Utflatet$Har_oppf_gr2[m], Utflatet$Mangler_oppf_gr2[m]),
                ncol = 2)
  testres <- fisher.test(aux)
  Utflatet$odds.ratio[m] <- testres$estimate
  Utflatet$p.verdi[m] <- testres$p.value
  Utflatet$konfint_lav[m] <- testres$conf.int[1]
  Utflatet$konfint_hoy[m] <- testres$conf.int[2]
}

oppsummert_kat <- Utflatet %>%
  select(variable, label_gr1, label_gr2, Har_oppf_gr1, Mangler_oppf_gr1,
         Har_oppf_gr2, Mangler_oppf_gr2, odds.ratio,
         p.verdi, konfint_lav, konfint_hoy)

write.csv2(oppsummert_kat,
           "~/mydata/nnrr/validering2024/oppsummering_kategoriske_var.csv",
           row.names = F,
           fileEncoding = "Latin1")

write.csv2(oppsummert_num,
           "~/mydata/nnrr/validering2024/oppsummering_numeriske_var.csv",
           row.names = F,
           fileEncoding = "Latin1")


########### 12 mnd #############################################################

data_forbehandl <- pasientsvar_pre[, forbehandl] %>%
  mutate(Dato = as.Date(S1b_DateOfCompletion, format="%d.%m.%Y")) %>%
  dplyr::filter(Dato >= "2021-01-01",
                Dato <= "2022-12-31") %>%
  dplyr::select(-c(S1b_DateOfCompletion))
data_behandl <- legeskjema[, behandl] %>%
  mutate(Dato = as.Date(S1b_DateOfCompletion, format="%d.%m.%Y")) %>%
  dplyr::filter(Dato >= "2021-01-01",
                Dato <= "2022-12-31") %>%
  dplyr::select(-c(Dato, S1b_DateOfCompletion))

data_pre <- merge(data_forbehandl, data_behandl, by.x = "HovedskjemaGUID",
                  by.y = "SkjemaGUID", suffixes = c("_pas", "_beh"))

pasientsvar_post12mnd <- pasientsvar_post2[, c("HovedskjemaGUID", "FormDate", "DateOfCompletion")]

data_pre <- merge(data_pre, pasientsvar_post12mnd,
                  by = "HovedskjemaGUID",
                  all.x = TRUE) %>%
  dplyr::mutate(Oppfolging = ifelse(is.na(DateOfCompletion), "Mangler_oppf", "Har_oppf"))

oppsummert <- data_pre %>%
  pivot_longer(all_of(katvar),
               names_to = "variable", values_to = "value") %>%
  count(Oppfolging, variable, value) %>%
  pivot_wider(names_from = Oppfolging, values_from = n) %>%
  mutate(har_oppf_prosent = Har_oppf/sum(data_pre$Oppfolging=="Har_oppf")*100,
         mangler_oppf_prosent = Mangler_oppf/sum(data_pre$Oppfolging=="Mangler_oppf")*100)



oppsummert <- merge(oppsummert, kodebok_verdier[, c("Variabelnavn", "value",
                                                    "label", "Felttype")],
                    by.x = c("variable", "value"),
                    by.y = c("Variabelnavn", "value"),
                    all.x = TRUE) %>%
  dplyr::select(variable, value, label, Har_oppf, Mangler_oppf, har_oppf_prosent,
                mangler_oppf_prosent, Felttype) %>%
  dplyr::mutate(Felttype = ifelse(is.na(Felttype), "Avkrysning", Felttype))

