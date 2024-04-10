rm(list=ls())
library(nnrr)
library(dplyr)
library(tidyr)

# RegData <- nnrr::nnrrHentRegData()
pasientsvar_pre <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Pasientskjema+før+behandling_2024-03-14_1622.csv')
legeskjema <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Behandlerskjema_2024-03-14_1622.csv')
pasientsvar_post <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Pasientskjema+6+måneder+etter+behandling_2024-03-14_1622.csv')
pasientsvar_post2 <-
  readr::read_csv2(
    '~/mydata/nnrr/DataDump_MRS-PROD_Pasientskjema+12+måneder+etter+behandling_2024-03-14_1622.csv')
filfolder <- "~/mydata/nnrr/validering2024/"
fil1 <- paste0(filfolder, "Variabelliste.xlsx")

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
  merge(oppsummert_num3, by = "name")

write.csv2(oppsummert,
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

