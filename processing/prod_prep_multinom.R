# 0. Identification ---------------------------------------------------

# Title: Data preparation for thesis project Social Mobilty and Market Justice Preferencees
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code to data preparation for thesis project
# Date: May 1, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               here,
               car,
               sjmisc,
               sjlabelled,
               summarytools,
               nnet,
               occupar,
               cobalt,
               WeightIt)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))
load(here("input/data/elsoc_ciuo08.RData"))

glimpse(elsoc_long_2016_2023)

# 3. Processing -----------------------------------------------------------

elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(just_educ = d02_02, 
         just_pension = d02_01, 
         just_health = d02_03, 
         merit_effort = c18_09,
         merit_talent = c18_10) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# Market Justice Preferences

frq(elsoc_long_2016_2023$just_health)
frq(elsoc_long_2016_2023$just_pension)
frq(elsoc_long_2016_2023$just_educ)

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(
    across(
      .cols = c(just_health, just_pension, just_educ),
      .fns = ~ car::recode(., recodes = c("1='Strongly disagree'; 2='Disagree';
                                          3='Neither agree nor disagree'; 4='Agree';
                                          5='Strongly agree'"), 
                           levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),
                           as.factor = T)
    )
  )

elsoc_long_2016_2023$just_health <- sjlabelled::set_label(elsoc_long_2016_2023$just_health,
                                                          label = "Health distributive justice")

elsoc_long_2016_2023$just_pension <- sjlabelled::set_label(elsoc_long_2016_2023$just_pension,
                                                           label = "Pension distributive justice")

elsoc_long_2016_2023$just_educ <- sjlabelled::set_label(elsoc_long_2016_2023$just_educ,
                                                        label = "Education distributive justice")

# Meritocracy

frq(elsoc_long_2016_2023$merit_effort)
frq(elsoc_long_2016_2023$merit_talent)

elsoc_long_2016_2023 <- cbind(elsoc_long_2016_2023, 
                              "merit_i" = rowMeans(
                                elsoc_long_2016_2023 %>% 
                                  select(merit_effort, merit_talent), na.rm=TRUE))

frq(elsoc_long_2016_2023$merit_i)

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(
    across(
      .cols = c(merit_effort, merit_talent),
      .fns = ~ car::recode(., recodes = c("1='Strongly disagree'; 2='Disagree';
                                          3='Neither agree nor disagree'; 4='Agree';
                                          5='Strongly agree'"), 
                           levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),
                           as.factor = T)
    )
  )


elsoc_long_2016_2023$merit_effort <- sjlabelled::set_label(elsoc_long_2016_2023$merit_effort, 
                                                           label = "People are rewarded for their efforts")

elsoc_long_2016_2023$merit_talent <- sjlabelled::set_label(elsoc_long_2016_2023$merit_talent, 
                                                           label = "People are rewarded for their intelligence")


# IPW variables

## nacionalidad (m45)

frq(elsoc_long_2016_2023$m45)

elsoc_long_2016_2023$nacionalidad <- if_else(elsoc_long_2016_2023$m45 == 1, 0, 1)

## genero (m0_sexo)

frq(elsoc_long_2016_2023$m0_sexo)

elsoc_long_2016_2023$sexo <- if_else(elsoc_long_2016_2023$m0_sexo == 1, 1, 0)

## edad (m0_edad)

frq(elsoc_long_2016_2023$m0_edad)

elsoc_long_2016_2023$edad2 <- (elsoc_long_2016_2023$m0_edad)^2

## etnia/pueblos indigenas (m53)

frq(elsoc_long_2016_2023$m53)

elsoc_long_2016_2023$etnia <- if_else(elsoc_long_2016_2023$m53 == 10, 0, 1)

## presencia ambos padres 15 años (m49)

frq(elsoc_long_2016_2023$m49)

elsoc_long_2016_2023$hogar_bip <- if_else(elsoc_long_2016_2023$m49 == 3, 1, 0)

## madre trabajaba cuando era pequeño (m52)

frq(elsoc_long_2016_2023$m52) 

elsoc_long_2016_2023$madre_trab <- if_else(elsoc_long_2016_2023$m52 == 1, 1, 0)

## nivel educacional madre/padre (m27 y m28)

frq(elsoc_long_2016_2023$m27) # no presente 2022, imputar si es el mismo id?
frq(elsoc_long_2016_2023$m28) # no presente 2022, imputar si es el mismo id?

elsoc_long_2016_2023$educ_orig <- pmax(elsoc_long_2016_2023$m27, elsoc_long_2016_2023$m28, na.rm = T)

## Merge

# 2018 y 2023

ola6 <- elsoc_long_2016_2023 %>% 
  filter(ola == 6)

ola6 <- left_join(
  ola6, 
  df %>% 
    select(idencuesta, cod_final, prob, isco08_orig,
           isei08_orig, isei92_orig),
  by = "idencuesta")

vars_invariantes <- elsoc_long_2016_2023 %>%
  arrange(idencuesta, ola) %>%  # ordena temporalmente
  group_by(idencuesta) %>%
  reframe(
    hogar_bip_f = first(na.omit(hogar_bip)),
    educ_orig_f = first(na.omit(educ_orig)),
    educ_madre_f = first(na.omit(m27)),
    educ_padre_f = first(na.omit(m28)),
    nacionalidad_f = first(na.omit(nacionalidad)),
    sexo_f = first(na.omit(sexo)),
    m0_edad_f = first(na.omit(m0_edad)),
    etnia_f = first(na.omit(etnia)),
    .groups = "drop"
  )

ola6 <- left_join(ola6, vars_invariantes, by = "idencuesta")

ola6 <- ola6 %>% 
  select(idencuesta, isco08_orig, isei92_orig, isei08_orig,
         hogar_bip_f, educ_orig_f, educ_madre_f, educ_padre_f,
         nacionalidad_f, sexo_f, m0_edad_f, etnia_f)


# intentar con 2016, 2018 y 2023

frq(elsoc_long_2016_2023$ola)

db <- elsoc_long_2016_2023 %>% 
  filter(ola %in% c(1, 3, 7))

db <- left_join(db, ola6, by = "idencuesta")

library(DIGCLASS)


db$isco88to08 <- occupar::isco88to08(db$ciuo88_m03)
db$isco88to08_dig <- DIGCLASS::isco88_to_isco08(as.character(db$ciuo08_m03))

frq(db$isco88to08)
frq(db$isco88to08_dig)

db$isco08 <- NA
db$isco08[db$ola %in% c(3,7)] <- db$ciuo08_m03[db$ola %in% c(3,7)]
db$isco08[db$ola %in% c(1)] <- db$isco88to08[db$ola %in% c(1)]

db <- db %>% 
  mutate(isei08_ocupa = occupar::isco08toISEI08(isco08),
         isei08_ocupa_dig = DIGCLASS::isco08_to_isei(as.character(isco08)))

frq(db$isei08_ocupa)
frq(db$isei08_ocupa_dig)

## Estimar IPW

db <- db %>% 
  select(idencuesta, ola,
         just_pension, merit_i,
         nacionalidad_f, sexo_f, m0_edad_f, etnia_f, 
         hogar_bip_f, educ_orig_f, 
         isei08_orig, isei08_ocupa) %>% 
  mutate(just_pension = as.integer(just_pension)) %>% 
  as_tibble()

# Crear estrato_ocupa basado en rangos de isei_ocupa

#db <- db %>% 
#  mutate(
#    estrato_ocupa = case_when(isei08_ocupa <= 39 ~ 1,
#                              isei08_ocupa >= 40 & isei08_ocupa <= 59 ~ 2,
#                              isei08_ocupa >= 60  ~ 3,
#                              NA ~ NA_real_))
#
#db <- db %>% 
#  mutate(
#    estrato_orig = case_when(isei08_orig <= 39 ~ 1,
#                             isei08_orig >= 40 & isei08_orig <= 59 ~ 2,
#                             isei08_orig >= 60  ~ 3,
#                             NA ~ NA_real_))
#
#db$terc_ocupa <- ntile(db$isei08_ocupa, n = 3)

#db <- db %>% 
#  mutate(
#    terc_orig = case_when(isei92_orig <= 31 ~ 1,
#                          isei92_orig > 31 & isei92_orig <= 42 ~ 2,
#                          isei92_orig > 42 & isei92_orig <= 89 ~ 3,
#                          NA ~ NA_real_)
#  )

#db$terc_orig <- ntile(db$isei08_orig, n = 3)

# Reemplazar estratos con los terciles
#db$estrato_orig <- db$terc_orig
#db$estrato_ocupa <- db$terc_ocupa

# Crear estrato_ocupa basado en rangos de isei_ocupa

#db <- db %>% 
#  mutate(
#    estrato_ocupa = case_when(isei08_ocupa >= 10 & isei08_ocupa < 20 ~ 1,
#                              isei08_ocupa >= 20 & isei08_ocupa < 30 ~ 2,
#                              isei08_ocupa >= 30 & isei08_ocupa < 40 ~ 3,
#                              isei08_ocupa >= 40 & isei08_ocupa < 50 ~ 4,
#                              isei08_ocupa >= 50 & isei08_ocupa < 60 ~ 5,
#                              isei08_ocupa >= 60 & isei08_ocupa < 70 ~ 6,
#                              isei08_ocupa >= 70 & isei08_ocupa < 90 ~ 7,
#                              NA ~ NA_real_))
#
#db <- db %>% 
#  mutate(
#    estrato_orig = case_when(isei08_orig >= 10 & isei08_orig < 20 ~ 1,
#                             isei08_orig >= 20 & isei08_orig < 30 ~ 2,
#                             isei08_orig >= 30 & isei08_orig < 40 ~ 3,
#                             isei08_orig >= 40 & isei08_orig < 50 ~ 4,
#                             isei08_orig >= 50 & isei08_orig < 60 ~ 5,
#                             isei08_orig >= 60 & isei08_orig < 70 ~ 6,
#                             isei08_orig >= 70 & isei08_orig < 90 ~ 7,
#                             NA ~ NA_real_))

db$terc_ocupa <- ntile(db$isei08_ocupa, n = 3)

db$terc_orig <- ntile(db$isei08_orig, n = 3)

# Reemplazar estratos con los terciles
db$estrato_orig <- db$terc_orig
db$estrato_ocupa <- db$terc_ocupa

table(db$estrato_orig, db$estrato_ocupa)

# Combinations

db <- db %>%
  mutate(
    ll = if_else(estrato_orig == 1 & estrato_ocupa == 1, 1, 0),
    lm = if_else(estrato_orig == 1 & estrato_ocupa == 2, 1, 0),
    lh = if_else(estrato_orig == 1 & estrato_ocupa == 3, 1, 0),
    
    ml = if_else(estrato_orig == 2 & estrato_ocupa == 1, 1, 0),
    mm = if_else(estrato_orig == 2 & estrato_ocupa == 2, 1, 0),
    mh = if_else(estrato_orig == 2 & estrato_ocupa == 3, 1, 0),
    
    hl = if_else(estrato_orig == 3 & estrato_ocupa == 1, 1, 0),
    hm = if_else(estrato_orig == 3 & estrato_ocupa == 2, 1, 0),
    hh = if_else(estrato_orig == 3 & estrato_ocupa == 3, 1, 0)
  )

db <- na.omit(db)

# =====
# Low
# =====
db <- db %>%
  mutate(
    low_orig = case_when(
      ll == 1 ~ "ll",
      lm == 1 ~ "lm",
      lh == 1 ~ "lh",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(low_orig = factor(low_orig, levels = c("ll","lm","lh")))

model_data1 <- db %>%
  select(idencuesta, ola, low_orig, m0_edad_f, sexo_f, nacionalidad_f, etnia_f, hogar_bip_f, educ_orig_f) %>%
  tidyr::drop_na()

# --- lm vs ll (ATT de lm)
pair_lm_ll <- model_data1 %>%
  filter(low_orig %in% c("lm","ll")) %>%
  mutate(treat_lm = as.integer(low_orig == "lm"))

W_lm_ll <- weightit(
  treat_lm ~ m0_edad_f + factor(sexo_f) + factor(nacionalidad_f) +
    factor(etnia_f) + factor(hogar_bip_f) + educ_orig_f,
  data     = pair_lm_ll,
  method   = "ebal",
  estimand = "ATT"
)

bal.tab(W_lm_ll, un = TRUE, m.threshold = .05, v.threshold = 2, pairwise = TRUE)

pair_lm_ll <- pair_lm_ll %>%
  mutate(
    w_low_lm   = as.numeric(W_lm_ll$weights),
    w_low_lm   = w_low_lm / mean(w_low_lm, na.rm = TRUE),
    w_low_lm_t = pmin(w_low_lm, quantile(w_low_lm, 0.99, na.rm = TRUE))
  )

# --- lh vs ll (ATT de lh)
pair_lh_ll <- model_data1 %>%
  filter(low_orig %in% c("lh","ll")) %>%
  mutate(treat_lh = as.integer(low_orig == "lh"))

W_lh_ll <- weightit(
  treat_lh ~ m0_edad_f + factor(sexo_f) + factor(nacionalidad_f) +
    factor(etnia_f) + factor(hogar_bip_f) + educ_orig_f,
  data     = pair_lh_ll,
  method   = "ebal",
  estimand = "ATT"
)

bal.tab(W_lh_ll, un = TRUE, m.threshold = .05, v.threshold = 2, pairwise = TRUE)

pair_lh_ll <- pair_lh_ll %>%
  mutate(
    w_low_lh   = as.numeric(W_lh_ll$weights),
    w_low_lh   = w_low_lh / mean(w_low_lh, na.rm = TRUE),
    w_low_lh_t = pmin(w_low_lh, quantile(w_low_lh, 0.99, na.rm = TRUE))
  )

# Pega pesos de ambos pares en model_data1 (quedan NA donde no aplica)
model_data1 <- model_data1 %>%
  left_join(pair_lm_ll %>% select(idencuesta, ola, w_low_lm, w_low_lm_t), by = c("idencuesta","ola")) %>%
  left_join(pair_lh_ll %>% select(idencuesta, ola, w_low_lh, w_low_lh_t), by = c("idencuesta","ola"))

# Construye datos1 como en tu pipeline
datos1 <- left_join(
  db %>% select(-low_orig),
  model_data1 %>% select(idencuesta, ola, low_orig, starts_with("w_low_")),
  by = c("idencuesta", "ola")
) %>%
  mutate(
    moblm = case_when(lm == 1 ~ 1, ll == 1 ~ 0, TRUE ~ NA_real_),
    moblh = case_when(lh == 1 ~ 1, ll == 1 ~ 0, TRUE ~ NA_real_)
  )

# =========
# Middle
# =========
db <- db %>%
  mutate(
    m_orig = case_when(
      mm == 1 ~ "mm",
      ml == 1 ~ "ml",
      mh == 1 ~ "mh",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(m_orig = factor(m_orig, levels = c("mm","ml","mh")))

model_data2 <- db %>%
  select(idencuesta, ola, m_orig, m0_edad_f, sexo_f, nacionalidad_f, etnia_f, hogar_bip_f, educ_orig_f) %>%
  tidyr::drop_na()

# --- ml vs mm (ATT de ml)
pair_ml_mm <- model_data2 %>%
  filter(m_orig %in% c("ml","mm")) %>%
  mutate(treat_ml = as.integer(m_orig == "ml"))

W_ml_mm <- weightit(
  treat_ml ~ m0_edad_f + factor(sexo_f) + factor(nacionalidad_f) +
    factor(etnia_f) + factor(hogar_bip_f) + educ_orig_f,
  data     = pair_ml_mm,
  method   = "ebal",
  estimand = "ATT"
)

bal.tab(W_ml_mm, un = TRUE, m.threshold = .05, v.threshold = 2, pairwise = TRUE)

pair_ml_mm <- pair_ml_mm %>%
  mutate(
    w_mid_ml   = as.numeric(W_ml_mm$weights),
    w_mid_ml   = w_mid_ml / mean(w_mid_ml, na.rm = TRUE),
    w_mid_ml_t = pmin(w_mid_ml, quantile(w_mid_ml, 0.99, na.rm = TRUE))
  )

# --- mh vs mm (ATT de mh)
pair_mh_mm <- model_data2 %>%
  filter(m_orig %in% c("mh","mm")) %>%
  mutate(treat_mh = as.integer(m_orig == "mh"))

W_mh_mm <- weightit(
  treat_mh ~ m0_edad_f + factor(sexo_f) + factor(nacionalidad_f) +
    factor(etnia_f) + factor(hogar_bip_f) + educ_orig_f,
  data     = pair_mh_mm,
  method   = "ebal",
  estimand = "ATT"
)

bal.tab(W_mh_mm, un = TRUE, m.threshold = .05, v.threshold = 2, pairwise = TRUE)

pair_mh_mm <- pair_mh_mm %>%
  mutate(
    w_mid_mh   = as.numeric(W_mh_mm$weights),
    w_mid_mh   = w_mid_mh / mean(w_mid_mh, na.rm = TRUE),
    w_mid_mh_t = pmin(w_mid_mh, quantile(w_mid_mh, 0.99, na.rm = TRUE))
  )

# Pega pesos en model_data2
model_data2 <- model_data2 %>%
  left_join(pair_ml_mm %>% select(idencuesta, ola, w_mid_ml, w_mid_ml_t), by = c("idencuesta","ola")) %>%
  left_join(pair_mh_mm %>% select(idencuesta, ola, w_mid_mh, w_mid_mh_t), by = c("idencuesta","ola"))

# Construye datos2
datos2 <- left_join(
  db %>% select(-low_orig, -m_orig),
  model_data2 %>% select(idencuesta, ola, m_orig, starts_with("w_mid_")),
  by = c("idencuesta", "ola")
) %>%
  mutate(
    mobml = case_when(ml == 1 ~ 1, mm == 1 ~ 0, TRUE ~ NA_real_),
    mobmh = case_when(mh == 1 ~ 1, mm == 1 ~ 0, TRUE ~ NA_real_)
  )

# ======
# High
# ======
db <- db %>%
  mutate(
    h_orig = case_when(
      hh == 1 ~ "hh",
      hm == 1 ~ "hm",
      hl == 1 ~ "hl",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(h_orig = factor(h_orig, levels = c("hh","hm","hl")))

model_data3 <- db %>%
  select(idencuesta, ola, h_orig, m0_edad_f, sexo_f, nacionalidad_f, etnia_f, hogar_bip_f, educ_orig_f) %>%
  tidyr::drop_na()

# --- hm vs hh (ATT de hm)
pair_hm_hh <- model_data3 %>%
  filter(h_orig %in% c("hm","hh")) %>%
  mutate(treat_hm = as.integer(h_orig == "hm"))

W_hm_hh <- weightit(
  treat_hm ~ m0_edad_f + factor(sexo_f) + factor(nacionalidad_f) +
    factor(etnia_f) + factor(hogar_bip_f) + educ_orig_f,
  data     = pair_hm_hh,
  method   = "ebal",
  estimand = "ATT"
)

bal.tab(W_hm_hh, un = TRUE, m.threshold = .05, v.threshold = 2, pairwise = TRUE)

pair_hm_hh <- pair_hm_hh %>%
  mutate(
    w_hig_hm   = as.numeric(W_hm_hh$weights),
    w_hig_hm   = w_hig_hm / mean(w_hig_hm, na.rm = TRUE),
    w_hig_hm_t = pmin(w_hig_hm, quantile(w_hig_hm, 0.99, na.rm = TRUE))
  )

# --- hl vs hh (ATT de hl)
pair_hl_hh <- model_data3 %>%
  filter(h_orig %in% c("hl","hh")) %>%
  mutate(treat_hl = as.integer(h_orig == "hl"))

W_hl_hh <- weightit(
  treat_hl ~ m0_edad_f + factor(sexo_f) + factor(nacionalidad_f) +
    factor(etnia_f) + factor(hogar_bip_f) + educ_orig_f,
  data     = pair_hl_hh,
  method   = "ebal",
  estimand = "ATT"
)

bal.tab(W_hl_hh, un = TRUE, m.threshold = .05, v.threshold = 2, pairwise = TRUE)

pair_hl_hh <- pair_hl_hh %>%
  mutate(
    w_hig_hl   = as.numeric(W_hl_hh$weights),
    w_hig_hl   = w_hig_hl / mean(w_hig_hl, na.rm = TRUE),
    w_hig_hl_t = pmin(w_hig_hl, quantile(w_hig_hl, 0.99, na.rm = TRUE))
  )

# Pega pesos en model_data3
model_data3 <- model_data3 %>%
  left_join(pair_hm_hh %>% select(idencuesta, ola, w_hig_hm, w_hig_hm_t), by = c("idencuesta","ola")) %>%
  left_join(pair_hl_hh %>% select(idencuesta, ola, w_hig_hl, w_hig_hl_t), by = c("idencuesta","ola"))

# Construye datos3 (tu forma)
datos3 <- left_join(
  db %>% select(-low_orig, -m_orig, -h_orig),
  model_data3 %>% select(idencuesta, ola, h_orig, starts_with("w_hig_")),
  by = c("idencuesta", "ola")
) %>%
  mutate(
    mobhm = case_when(hm == 1 ~ 1, hh == 1 ~ 0, TRUE ~ NA_real_),
    mobhl = case_when(hl == 1 ~ 1, hh == 1 ~ 0, TRUE ~ NA_real_)
  )

# 4. Save and export ------------------------------------------------------

db <- db %>%
  select(-c(isei08_orig, isei08_ocupa, terc_orig, terc_ocupa)) %>% 
  mutate(ola = case_when(ola == 1 ~ 1,
                         ola == 3 ~ 2,
                         ola == 7 ~ 3,
                         TRUE ~ NA_real_)) %>% 
  mutate(
    across(
      .cols = c(estrato_orig, estrato_ocupa),
      .fns = ~ car::recode(.,
                           recodes = c("1 = 'Low';
                                       2 = 'Middle';
                                       3 = 'High'"),
                           as.factor = T)
    )
  ) %>% 
  mutate(
    across(
      .cols = c(estrato_orig, estrato_ocupa),
      .fns = ~ factor(., levels = c("Low", "Middle", "High"))
    )
  ) %>% 
  mutate(wave = case_when(ola == 1 ~ "2016",
                          ola == 2 ~ "2018",
                          ola == 3 ~ "2023"),
         wave = factor(wave, levels = c("2016",
                                        "2018",
                                        "2023"))) 

db$estrato_orig <- sjlabelled::set_label(db$estrato_orig, "Father stratum")

db$estrato_ocupa <- sjlabelled::set_label(db$estrato_ocupa, "Offspring stratum")


db <- db %>% 
  mutate(
    mobility = paste(estrato_orig, estrato_ocupa, sep = "-"),
    mobility = case_when(mobility %in% c("High-Middle", 
                                         "High-Low",
                                         "Middle-Low") ~ "Downward",
                         estrato_orig == estrato_ocupa ~ "Inmobile",
                         mobility %in% c("Middle-High", 
                                         "Low-Middle",
                                         "Low-High") ~ "Upward"))


db <- db %>% filter(!is.na(mobility)) %>% select(-c(low_orig, m_orig, h_orig))

bases <- c("datos1", "datos2", "datos3", "db")

for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    mutate(
      just_pension_f = if_else(just_pension >= 4, 1, 0),
      nacionalidad_f = if_else(nacionalidad_f == 0, 1, 0),
      sexo_f = if_else(sexo_f == 1, 0, 1),
      ola = case_when(ola == 1 ~ 1,
                      ola == 3 ~ 2,
                      ola == 7 ~ 3,
                      TRUE ~ NA_real_)
    )
  
  assign(nm, df, inherits = TRUE)
}


bases <- c("datos1", "datos2", "datos3")

for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    group_by(idencuesta) %>% 
    mutate(merit_m = mean(merit_i)
    ) %>% 
    ungroup()
  
  assign(nm, df, inherits = TRUE)
}


for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    mutate(merit_i = if_else(merit_i >= 4, 1, 0),
           merit_i = factor(merit_i, levels = c(0,1), labels = c("Low", "High")),
           merit_m = if_else(merit_m >= 4, 1, 0),
           merit_m = factor(merit_m, levels = c(0,1), labels = c("Low", "High"))
    )
  
  assign(nm, df, inherits = TRUE)
}


save(db,
     datos1,
     datos2,
     datos3, 
     W_lm_ll, W_lh_ll,
     W_ml_mm, W_mh_mm,
     W_hm_hh, W_hl_hh,
     file = here("input/data/proc/data_v2.3.RData"))
