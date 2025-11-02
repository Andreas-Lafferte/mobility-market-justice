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
               naniar,
               data.table,
               summarytools,
               nnet,
               occupar)

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
      .fns = ~ car::recode(., recodes = c("1='Strongly desagree'; 2='Desagree';
                                          3='Neither agree nor desagree'; 4='Agree';
                                          5='Strongly agree'"), 
                           levels = c("Strongly desagree", "Desagree", "Neither agree nor desagree", "Agree", "Strongly agree"),
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
      .fns = ~ car::recode(., recodes = c("1='Strongly desagree'; 2='Desagree';
                                          3='Neither agree nor desagree'; 4='Agree';
                                          5='Strongly agree'"), 
                           levels = c("Strongly desagree", "Desagree", "Neither agree nor desagree", "Agree", "Strongly agree"),
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

frq(elsoc_long_2016_2023$m49) # no presente 2022, imputar si es el mismo id?

elsoc_long_2016_2023$hogar_bip <- if_else(elsoc_long_2016_2023$m49 == 3, 1, 0)

## madre trabajaba cuando era pequeño (m52)

frq(elsoc_long_2016_2023$m52) # no presente 2022, imputar si es el mismo id?

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
  summarise(
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


# intentar con 2018 y 2023

frq(elsoc_long_2016_2023$ola)

db <- elsoc_long_2016_2023 %>% 
  filter(ola %in% c(3, 7))

db <- left_join(db, ola6, by = "idencuesta")

db <- db %>% 
  mutate(isei08_ocupa = occupar::isco08toISEI08(ciuo08_m03))


## Estimar IPW

db <- db %>% 
  select(idencuesta, ola, ponderador_long_total, segmento, estrato,
         just_educ, just_pension, just_health, merit_effort, merit_talent,
         nacionalidad, sexo, m0_edad, etnia, hogar_bip_f,
         educ_madre_f, educ_padre_f, educ_orig_f, isei08_orig, isei92_orig,
         isei08_ocupa)

# Crear estrato_ocupa basado en rangos de isei_ocupa

db <- db %>% 
  mutate(
    estrato_ocupa = case_when(isei08_ocupa >= 10 & isei08_ocupa < 20 ~ 1,
                              isei08_ocupa >= 20 & isei08_ocupa < 30 ~ 2,
                              isei08_ocupa >= 30 & isei08_ocupa < 40 ~ 3,
                              isei08_ocupa >= 40 & isei08_ocupa < 50 ~ 4,
                              isei08_ocupa >= 50 & isei08_ocupa < 60 ~ 5,
                              isei08_ocupa >= 60 & isei08_ocupa < 70 ~ 6,
                              isei08_ocupa >= 70 & isei08_ocupa < 90 ~ 7,
                              NA ~ NA_real_))

db <- db %>% 
  mutate(
    estrato_orig = case_when(isei08_orig >= 10 & isei08_orig < 20 ~ 1,
                             isei08_orig >= 20 & isei08_orig < 30 ~ 2,
                             isei08_orig >= 30 & isei08_orig < 40 ~ 3,
                             isei08_orig >= 40 & isei08_orig < 50 ~ 4,
                             isei08_orig >= 50 & isei08_orig < 60 ~ 5,
                             isei08_orig >= 60 & isei08_orig < 70 ~ 6,
                             isei08_orig >= 70 & isei08_orig < 90 ~ 7,
                             NA ~ NA_real_))

db$terc_ocupa <- ntile(db$isei08_ocupa, n = 3)

#db <- db %>% 
#  mutate(
#    terc_orig = case_when(isei92_orig <= 31 ~ 1,
#                          isei92_orig > 31 & isei92_orig <= 42 ~ 2,
#                          isei92_orig > 42 & isei92_orig <= 89 ~ 3,
#                          NA ~ NA_real_)
#  )

db$terc_orig <- ntile(db$isei08_orig, n = 3)

# Reemplazar estratos con los terciles
db$estrato_orig <- db$terc_orig
db$estrato_ocupa <- db$terc_ocupa

db <- db %>%
  mutate(
    inmobile = case_when(estrato_orig == 3 & estrato_ocupa == 3 ~ 1,
                         estrato_orig == 2 & estrato_ocupa == 2 ~ 1,
                         estrato_orig == 1 & estrato_ocupa == 1 ~ 1),
    
    upward = case_when(estrato_orig == 3 & estrato_ocupa %in% c(1,2) ~ 1,
                       estrato_orig == 2 & estrato_ocupa == 1 ~ 1),
    
    downward = case_when(estrato_orig == 2 & estrato_ocupa == 3 ~ 1,
                         estrato_orig == 1 & estrato_ocupa %in% c(2,3) ~ 1))

table(db$estrato_orig, db$estrato_ocupa)

db %>% 
  group_by(estrato_orig, estrato_ocupa) %>% 
  tally()

# downward_mob

db <- db %>%
  mutate(
    downward_mob = case_when(
      inmobile == 1 ~ 0,
      downward == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )


model_data1 <- db %>% 
  select(idencuesta, ola, downward_mob, m0_edad, sexo, nacionalidad, etnia, hogar_bip_f,
         educ_orig_f) %>% na.omit()

m1 <- glm(
  downward_mob ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip_f) + educ_orig_f,
  data = model_data1, family = "binomial"
)

# Probabilidad de downward_mob == 1 (descenso)
model_data1$pdownward_mob1 <- predict(m1, type = "response")

# Probabilidad de downward_mob == 0 (inmovilidad)
model_data1$pdownward_mob0 <- 1 - model_data1$pdownward_mob1


model_data1 <- model_data1 %>%
  mutate(
    wdownward_mob1 = case_when(
      downward_mob == 1 ~ (pdownward_mob0 + pdownward_mob1) / pdownward_mob1,
      downward_mob == 0 ~ (pdownward_mob0 + pdownward_mob1) / pdownward_mob0,
      TRUE ~ NA_real_))


datos1 <- left_join(
  db %>% 
    select(-downward_mob),
  model_data1 %>% 
    select(idencuesta, ola, downward_mob, pdownward_mob0, pdownward_mob1, 
           wdownward_mob1),
  by = c("idencuesta", "ola"))


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
  select(idencuesta, ola, inmobile_mob, m0_edad, sexo, nacionalidad, etnia, hogar_bip_f,
         educ_orig_f) %>% na.omit()

m2 <- glm(
  inmobile_mob ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip_f) + educ_orig_f,
  data = model_data2, family = "binomial"
)

# Probabilidad de inmobile_mob == 1 (inmobile)
model_data2$pinmobile_mob1 <- predict(m2, type = "response")

# Probabilidad de inmobile_mob == 0 (mobile)
model_data2$pinmobile_mob0 <- 1 - model_data2$pinmobile_mob1


model_data2 <- model_data2 %>%
  mutate(
    winmobile1 = case_when(
      inmobile_mob == 1 ~ (pinmobile_mob0 + pinmobile_mob1) / pinmobile_mob1,
      inmobile_mob == 0 ~ (pinmobile_mob0 + pinmobile_mob1) / pinmobile_mob0,
      TRUE ~ NA_real_))

datos2 <- left_join(
  db %>% 
    select(-downward_mob, -inmobile_mob),
  model_data2 %>% 
    select(idencuesta, ola, inmobile_mob, pinmobile_mob0, pinmobile_mob1, winmobile1),
  by = c("idencuesta", "ola"))

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
  select(idencuesta, ola, upward_mob, m0_edad, sexo, nacionalidad, etnia, hogar_bip_f,
         educ_orig_f) %>% na.omit()

m3 <- glm(
  upward_mob ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip_f) + educ_orig_f,
  data = model_data3, family = "binomial"
)

# Probabilidad de inmobile_mob == 1 (inmobile)
model_data3$pupward1 <- predict(m3, type = "response")

# Probabilidad de inmobile_mob == 0 (mobile)
model_data3$pupward0 <- 1 - model_data3$pupward1

model_data3 <- model_data3 %>%
  mutate(
    wpupward1 = case_when(
      upward_mob == 1 ~ (pupward0 + pupward1) / pupward1,
      upward_mob == 0 ~ (pupward0 + pupward1) / pupward0,
      TRUE ~ NA_real_))


datos3 <- left_join(
  db %>% 
    select(-downward_mob, -inmobile_mob, -upward_mob),
  model_data3 %>% 
    select(idencuesta, ola, upward_mob, pupward0, pupward1, wpupward1),
  by = c("idencuesta", "ola"))


## Regresiones con IPW

library(texreg)
library(estimatr)

### Pensiones

### downward

datos1 %>% 
  select(idencuesta, ola, just_pension, downward_mob, wdownward_mob1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_pension ~ downward_mob*merit_effort,
            weights = wdownward_mob1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### inmobile

datos2 %>% 
  select(idencuesta, ola, just_pension, inmobile_mob, winmobile1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_pension ~ inmobile_mob*merit_effort,
            weights = winmobile1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### upward

datos3 %>% 
  select(idencuesta, ola, just_pension, upward_mob, wpupward1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_pension ~ upward_mob*merit_effort,
            weights = wpupward1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()


## Salud

### downward

datos1 %>% 
  select(idencuesta, ola, just_health, downward_mob, wdownward_mob1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_health ~ downward_mob*merit_effort,
            weights = wdownward_mob1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### inmobile

datos2 %>% 
  select(idencuesta, ola, just_health, inmobile_mob, winmobile1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health),
         merit_effort = as.numeric(merit_effort))  %>% 
  lm_robust(just_health ~ inmobile_mob*merit_effort,
            weights = winmobile1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### upward

datos3 %>% 
  select(idencuesta, ola, just_health, upward_mob, wpupward1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health),
         merit_effort = as.numeric(merit_effort))  %>% 
  lm_robust(just_health ~ upward_mob*merit_effort,
            weights = wpupward1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()


## Educ

### downward

datos1 %>% 
  select(idencuesta, ola, just_educ, downward_mob, wdownward_mob1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_educ ~ downward_mob*merit_effort,
            weights = wdownward_mob1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### inmobile

datos2 %>% 
  select(idencuesta, ola, just_educ, inmobile_mob, winmobile1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_educ ~ inmobile_mob*merit_effort,
            weights = winmobile1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### upward

datos3 %>% 
  select(idencuesta, ola, just_educ, upward_mob, wpupward1, merit_effort) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ),
         merit_effort = as.numeric(merit_effort)) %>% 
  lm_robust(just_educ ~ upward_mob*merit_effort,
            weights = wpupward1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()



