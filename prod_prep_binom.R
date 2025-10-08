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
               data.table,
               summarytools,
               nnet,
               occupar,
               PSweight)

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
    .groups = "drop"
  )

ola6 <- left_join(ola6, vars_invariantes, by = "idencuesta")

ola6 <- ola6 %>% 
  select(idencuesta, isco08_orig, isei92_orig, isei08_orig,
         hogar_bip_f, educ_orig_f, educ_madre_f, educ_padre_f)


# intentar con 2016, 2018 y 2023

frq(elsoc_long_2016_2023$ola)

db <- elsoc_long_2016_2023 %>% 
  filter(ola %in% c(1, 3, 7))

db <- left_join(db, ola6, by = "idencuesta")

db$isco88to08 <- occupar::isco88to08(db$ciuo88_m03)
db$isco08 <- NA
db$isco08[db$ola %in% c(3,7)] <- db$ciuo08_m03[db$ola %in% c(3,7)]
db$isco08[db$ola %in% c(1)] <- db$isco88to08[db$ola %in% c(1)]


db <- db %>% 
  mutate(isei08_ocupa = occupar::isco08toISEI08(isco08))


## Estimar IPW

db <- db %>% 
  select(idencuesta, ola, ponderador_long_total, segmento, estrato,
         just_pension, merit_effort,
         nacionalidad, sexo, m0_edad, etnia, hogar_bip_f,
         educ_orig_f, isei08_orig, 
         isei08_ocupa) %>% 
  mutate(just_pension = as.integer(just_pension),
         merit_effort = as.integer(merit_effort),
         merit_effort = if_else(merit_effort <= 3, 0, 1))

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

db <- db %>%
  mutate(
    inmobile = if_else(estrato_ocupa == estrato_orig, 1, 0),
    upward = if_else(estrato_ocupa > estrato_orig, 1, 0),
    downward = if_else(estrato_ocupa < estrato_orig, 1, 0))

# Downward

db <- db %>%
  mutate(
    downward_mob = case_when(
      inmobile == 1 ~ 0,
      downward == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

model_data1 <- db %>%
  select(idencuesta, ola, downward_mob,
         m0_edad, sexo, nacionalidad, etnia, hogar_bip_f, educ_orig_f) %>%
  tidyr::drop_na()

W.dw <- weightit(
  downward_mob ~ m0_edad + factor(sexo) + factor(nacionalidad) +
    factor(etnia) + factor(hogar_bip_f) + educ_orig_f,
  data     = model_data1,
  method   = "glm",
  estimand = "ATT")

summary(W.dw)
bal.tab(W.dw, stats = c("m", "v"),
        thresholds = c(m = .05),
        estimand = "ATT")
cobalt::love.plot(W.dw, stats = "m", abs = TRUE, threshold = .10,
                  var.order = "unadjusted", line = TRUE,
                  colors = c("grey70","black")) +
  ggtitle("Balance SMD — downward (ATT)")

model_data1 <- model_data1 %>%
  mutate(
    w_att = as.numeric(W.dw$weights),
    w_att = w_att / mean(w_att)      # reescalado opcional
  )

cap <- quantile(model_data1$w_att, 0.99, na.rm = TRUE)
model_data1 <- model_data1 %>% mutate(w_att_trim = pmin(w_att, cap))

datos1 <- db %>%
  select(-downward_mob) %>%
  left_join(
    model_data1 %>% 
      select(idencuesta, ola, downward_mob, w_att, w_att_trim),
    by = c("idencuesta","ola")
  )

# inmobile

db <- db %>%
  mutate(
    inmobile_mob = case_when(
      downward == 1 ~ 0,
      upward == 1 ~ 0,
      inmobile == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

model_data2 <- db %>%
  select(idencuesta, ola, inmobile_mob,
         m0_edad, sexo, nacionalidad, etnia, hogar_bip_f, educ_orig_f) %>%
  tidyr::drop_na()

W.inm <- weightit(
  inmobile_mob ~ m0_edad + factor(sexo) + factor(nacionalidad) +
    factor(etnia) + factor(hogar_bip_f) + educ_orig_f,
  data     = model_data2,
  method   = "glm",
  estimand = "ATT")

summary(W.inm)
bal.tab(W.inm, stats = c("m", "v"),
        thresholds = c(m = .05),
        estimand = "ATT")
cobalt::love.plot(W.inm, stats = "m", abs = TRUE, threshold = .10,
                  var.order = "unadjusted", line = TRUE,
                  colors = c("grey70","black")) +
  ggtitle("Balance SMD — innmobile (ATT)")

model_data2 <- model_data2 %>%
  mutate(
    w_att = as.numeric(W.inm$weights),
    w_att = w_att / mean(w_att)      # reescalado opcional
  )

cap <- quantile(model_data2$w_att, 0.99, na.rm = TRUE)
model_data2 <- model_data2 %>% mutate(w_att_trim = pmin(w_att, cap))

datos2 <- db %>%
  select(-downward_mob, -inmobile_mob) %>%
  left_join(
    model_data2 %>% 
      select(idencuesta, ola, inmobile_mob, w_att, w_att_trim),
    by = c("idencuesta","ola")
  )


# Upward

db <- db %>%
  mutate(
    upward_mob = case_when(
      inmobile == 1 ~ 0,
      upward == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

model_data3 <- db %>%
  select(idencuesta, ola, upward_mob,
         m0_edad, sexo, nacionalidad, etnia, hogar_bip_f, educ_orig_f) %>%
  tidyr::drop_na()

W.uw <- weightit(
  upward_mob ~ m0_edad + factor(sexo) + factor(nacionalidad) +
    factor(etnia) + factor(hogar_bip_f) + educ_orig_f,
  data     = model_data3,
  method   = "glm",
  estimand = "ATT")

summary(W.uw)
bal.tab(W.uw, stats = c("m", "v"),
        thresholds = c(m = .05),
        estimand = "ATT")
cobalt::love.plot(W.uw, stats = "m", abs = TRUE, threshold = .10,
                  var.order = "unadjusted", line = TRUE,
                  colors = c("grey70","black")) +
  ggtitle("Balance SMD — upward (ATT)")

model_data3 <- model_data3 %>%
  mutate(
    w_att = as.numeric(W.uw$weights),
    w_att = w_att / mean(w_att)      # reescalado opcional
  )

cap <- quantile(model_data3$w_att, 0.99, na.rm = TRUE)
model_data3 <- model_data3 %>% mutate(w_att_trim = pmin(w_att, cap))

datos3 <- db %>%
  select(-downward_mob, -inmobile_mob, -upward_mob) %>%
  left_join(
    model_data3 %>% 
      select(idencuesta, ola, upward_mob, w_att, w_att_trim),
    by = c("idencuesta","ola")
  )

# 4. Save and export ------------------------------------------------------

save(db,
     datos1,
     datos2,
     datos3, file = here("input/data/proc/data_v3.RData"))
