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
    hogar_bip = first(na.omit(hogar_bip)),
    madre_trab = first(na.omit(madre_trab)),
    educ_orig = first(na.omit(educ_orig)),
    educ_madre = first(na.omit(m27)),
    educ_padre = first(na.omit(m28)),
    .groups = "drop"
  )

ola6 <- ola6 %>%
  left_join(vars_invariantes, by = "idencuesta")

ola6 <- ola6 %>% 
  select(idencuesta, isei92_orig,
         hogar_bip.y, madre_trab.y, educ_orig.y, educ_madre, educ_padre)


# intentar con 2018

frq(elsoc_long_2016_2023$ola)

db <- left_join(
elsoc_long_2016_2023 %>% 
  filter(ola == 3),
ola6,
by = "idencuesta")

db <- db %>% 
  mutate(isei08_ocupa = occupar::isco08toISEI08(ciuo08_m03))

## Estimar IPW

db <- db %>% 
  select(idencuesta, ola, ponderador_long_total, segmento, estrato,
         just_educ, just_pension, just_health, merit_effort, merit_talent,
         nacionalidad, sexo, m0_edad, etnia, hogar_bip.y, madre_trab.y,
         educ_madre, educ_padre, educ_orig.y, isei92_orig, isei08_ocupa)


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
    estrato_orig = case_when(isei92_orig >= 10 & isei92_orig < 20 ~ 1,
                             isei92_orig >= 20 & isei92_orig < 30 ~ 2,
                             isei92_orig >= 30 & isei92_orig < 40 ~ 3,
                             isei92_orig >= 40 & isei92_orig < 50 ~ 4,
                             isei92_orig >= 50 & isei92_orig < 60 ~ 5,
                             isei92_orig >= 60 & isei92_orig < 70 ~ 6,
                             isei92_orig >= 70 & isei92_orig < 81 ~ 7,
                              NA ~ NA_real_))

db$terc_ocupa <- ntile(db$isei08_ocupa, n = 3)

db <- db %>% 
  mutate(
    terc_orig = case_when(isei92_orig <= 31 ~ 3,
                          isei92_orig > 31 & isei92_orig <= 42 ~ 2,
                          isei92_orig > 42 & isei92_orig <= 89 ~ 1,
                          NA ~ NA_real_)
  )


# Reemplazar estratos con los terciles
db$estrato_orig <- db$terc_orig
db$estrato_ocupa <- db$terc_ocupa

db <- db %>%
  mutate(
    ll = if_else(estrato_orig == 3 & estrato_ocupa == 3, 1, 0),
    lm = if_else(estrato_orig == 3 & estrato_ocupa == 2, 1, 0),
    lh = if_else(estrato_orig == 3 & estrato_ocupa == 1, 1, 0),
    
    ml = if_else(estrato_orig == 2 & estrato_ocupa == 3, 1, 0),
    mm = if_else(estrato_orig == 2 & estrato_ocupa == 2, 1, 0),
    mh = if_else(estrato_orig == 2 & estrato_ocupa == 1, 1, 0),
    
    hl = if_else(estrato_orig == 1 & estrato_ocupa == 3, 1, 0),
    hm = if_else(estrato_orig == 1 & estrato_ocupa == 2, 1, 0),
    hh = if_else(estrato_orig == 1 & estrato_ocupa == 1, 1, 0)
  )

table(db$estrato_orig, db$estrato_ocupa)

# Low

db <- db %>%
  mutate(
    low_orig = case_when(
      ll == 1 ~ 0,
      lm == 1 ~ 1,
      lh == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )


model_data1 <- db %>% 
  select(idencuesta, low_orig, m0_edad, sexo, nacionalidad, etnia, hogar_bip.y,
         madre_trab.y, educ_orig.y) %>% na.omit()

m1 <- multinom(
  low_orig ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
  factor(hogar_bip.y) + factor(madre_trab.y) + educ_orig.y,
  data = model_data1
)

# Obtener probabilidades predichas (solo en filas sin NA en las variables del modelo)
probs <- predict(m1, type = "probs")

# Asignar a nuevas columnas
model_data1$plow_orig0 <- probs[, "0"]
model_data1$plow_orig1 <- probs[, "1"]
model_data1$plow_orig2 <- probs[, "2"]

model_data1 <- model_data1 %>%
  mutate(
    wlow_orig1 = case_when(
      low_orig == 1 ~ (plow_orig0 + plow_orig1) / plow_orig1,
      low_orig == 0 ~ (plow_orig0 + plow_orig1) / plow_orig0,
      TRUE ~ NA_real_
    ),
    wlow_orig2 = case_when(
      low_orig == 2 ~ (plow_orig0 + plow_orig2) / plow_orig2,
      low_orig == 0 ~ (plow_orig0 + plow_orig2) / plow_orig0,
      TRUE ~ NA_real_
    )
  )


datos1 <- left_join(
db %>% 
  select(-low_orig),
model_data1 %>% 
  select(idencuesta, low_orig, plow_orig0, plow_orig1, plow_orig2,
         wlow_orig1, wlow_orig2),
by = "idencuesta")

datos1 <- datos1 %>% 
  mutate(
    moblm = case_when(
      lm == 1 ~ 1,
      ll == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    moblh = case_when(
      lh == 1 ~ 1,
      ll == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )


# Middle

db <- db %>%
  mutate(
    m_orig = case_when(
      mm == 1 ~ 0,
      ml == 1 ~ 1,
      mh == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )


model_data2 <- db %>% 
  select(idencuesta, m_orig, m0_edad, sexo, nacionalidad, etnia, hogar_bip.y,
         madre_trab.y, educ_orig.y) %>% na.omit()

m2 <- multinom(
  m_orig ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip.y) + factor(madre_trab.y) + educ_orig.y,
  data = model_data2
)

# Obtener probabilidades predichas (solo en filas sin NA en las variables del modelo)
probs <- predict(m2, type = "probs")

# Asignar a nuevas columnas
model_data2$pm_mm <- probs[, "0"]
model_data2$pm_ml <- probs[, "1"]
model_data2$pm_mh <- probs[, "2"]

model_data2 <- model_data2 %>%
    mutate(
      wm_ml = case_when(
        m_orig == 1 ~ (pm_mm + pm_ml) / pm_ml,
        m_orig == 0 ~ (pm_mm + pm_ml) / pm_mm,
        TRUE ~ NA_real_
      ),
      wm_mh = case_when(
        m_orig == 2 ~ (pm_mm + pm_mh) / pm_mh,
        m_orig == 0 ~ (pm_mm + pm_mh) / pm_mm,
        TRUE ~ NA_real_
      )
    )
    
datos2 <- left_join(
  db %>% 
    select(-low_orig, -m_orig),
  model_data2 %>% 
    select(idencuesta, m_orig, pm_mm, pm_ml, pm_mh, wm_ml, wm_mh),
  by = "idencuesta")

datos2 <- datos2 %>% 
  mutate(
    mobml = case_when(
      ml == 1 ~ 1,
      mm == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    mobmh = case_when(
      mh == 1 ~ 1,
      mm == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# High

db <- db %>%
  mutate(
    h_orig = case_when(
      hh == 1 ~ 0,
      hl == 1 ~ 1,
      hm == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )


model_data3 <- db %>% 
  select(idencuesta, h_orig, m0_edad, sexo, nacionalidad, etnia, hogar_bip.y,
         madre_trab.y, educ_orig.y) %>% na.omit()

m3 <- multinom(
  h_orig ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip.y) + factor(madre_trab.y) + educ_orig.y,
  data = model_data3
)


# Obtener probabilidades predichas (solo en filas sin NA en las variables del modelo)
probs <- predict(m3, type = "probs")

# Asignar a nuevas columnas
model_data3$ph_hh <- probs[, "0"]
model_data3$ph_hl <- probs[, "1"]
model_data3$ph_hm <- probs[, "2"]

model_data3 <- model_data3 %>%
  mutate(
    wh_hl = case_when(
      h_orig == 1 ~ (ph_hh + ph_hl) / ph_hl,
      h_orig == 0 ~ (ph_hh + ph_hl) / ph_hh,
      TRUE ~ NA_real_
    ),
    wh_hm = case_when(
      h_orig == 2 ~ (ph_hh + ph_hm) / ph_hm,
      h_orig == 0 ~ (ph_hh + ph_hm) / ph_hh,
      TRUE ~ NA_real_
    )
  )


datos3 <- left_join(
  db %>% 
    select(-low_orig, -m_orig, -h_orig),
  model_data3 %>% 
    select(idencuesta, h_orig, ph_hh, ph_hl, ph_hm, wh_hl, wh_hm),
  by = "idencuesta")

datos3 <- datos3 %>% 
  mutate(
    mobhl = case_when(
      hl == 1 ~ 1,
      hh == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    mobhm = case_when(
      hm == 1 ~ 1,
      hh == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

## Regresiones con IPW

### Pensiones

### Low to middle

datos1 %>% 
  select(just_pension, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension)) %>% 
  lm(just_pension ~ moblm, 
     data = ., 
     weights = wlow_orig1) %>% summary()

### Low to high

datos1 %>% 
  select(just_pension, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension)) %>% 
  lm(just_pension ~ moblh, 
     data = ., 
     weights = wlow_orig2) %>% summary()

### middle to high

datos2 %>% 
  select(just_pension, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension)) %>% 
  lm(just_pension ~ mobmh, 
     data = ., 
     weights = wm_mh) %>% summary()


### middle to low

datos2 %>% 
  select(just_pension, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension)) %>% 
  lm(just_pension ~ mobml, 
     data = ., 
     weights = wm_ml) %>% summary()

### High to middle

datos3 %>% 
  select(just_pension, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension)) %>% 
  lm(just_pension ~ mobhm, 
     data = ., 
     weights = wh_hm) %>% summary()


### High to low

datos3 %>% 
  select(just_pension, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(just_pension = as.numeric(just_pension)) %>% 
  lm(just_pension ~ mobhl, 
     data = ., 
     weights = wh_hl) %>% summary()

### Salud

### Low to middle

datos1 %>% 
  select(just_health, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health)) %>% 
  lm(just_health ~ moblm, 
     data = ., 
     weights = wlow_orig1) %>% summary()

### Low to high

datos1 %>% 
  select(just_health, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health)) %>% 
  lm(just_health ~ moblh, 
     data = ., 
     weights = wlow_orig2) %>% summary() # aqui * 

### middle to high

datos2 %>% 
  select(just_health, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health)) %>% 
  lm(just_health ~ mobmh, 
     data = ., 
     weights = wm_mh) %>% summary()


### middle to low

datos2 %>% 
  select(just_health, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health)) %>% 
  lm(just_health ~ mobml, 
     data = ., 
     weights = wm_ml) %>% summary()

### High to middle

datos3 %>% 
  select(just_health, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health)) %>% 
  lm(just_health ~ mobhm, 
     data = ., 
     weights = wh_hm) %>% summary()


### High to low

datos3 %>% 
  select(just_health, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(just_health = as.numeric(just_health)) %>% 
  lm(just_health ~ mobhl, 
     data = ., 
     weights = wh_hl) %>% summary()


### Educ

### Low to middle

datos1 %>% 
  select(just_educ, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ)) %>% 
  lm(just_educ ~ moblm, 
     data = ., 
     weights = wlow_orig1) %>% summary()

### Low to high

datos1 %>% 
  select(just_educ, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ)) %>% 
  lm(just_educ ~ moblh, 
     data = ., 
     weights = wlow_orig2) %>% summary() 

### middle to high

datos2 %>% 
  select(just_educ, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ)) %>% 
  lm(just_educ ~ mobmh, 
     data = ., 
     weights = wm_mh) %>% summary()


### middle to low

datos2 %>% 
  select(just_educ, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ)) %>% 
  lm(just_educ ~ mobml, 
     data = ., 
     weights = wm_ml) %>% summary()

### High to middle

datos3 %>% 
  select(just_educ, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ)) %>% 
  lm(just_educ ~ mobhm, 
     data = ., 
     weights = wh_hm) %>% summary()


### High to low

datos3 %>% 
  select(just_educ, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(just_educ = as.numeric(just_educ)) %>% 
  lm(just_educ ~ mobhl, 
     data = ., 
     weights = wh_hl) %>% summary()

# SOLO UN RESULTADO SIGNIFICATIVO

# intentar con issp 2019


