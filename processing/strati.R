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
  mutate(merit_effort = c18_09,
         merit_talent = c18_10,
         perc_inequality2 = c18_11,
         perc_sal_gerente = d03_01,
         perc_sal_obrero = d03_02, 
         just_sal_gerente = d04_01, 
         just_sal_obrero = d04_02, 
         age = m0_edad, 
         m01, 
         sex = m0_sexo, 
         ess = d01_01, 
         ideo = c15
         ) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# Percep inequality

frq(elsoc_long_2016_2023$perc_inequality2) # ok

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(
    across(
      .cols = c(perc_inequality2),
      .fns = ~ car::recode(., recodes = c("1='Strongly disagree'; 2='Disagree';
                                          3='Neither agree nor disagree'; 4='Agree';
                                          5='Strongly agree'"), 
                           levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),
                           as.factor = T)
    )
  )


# perc inequality gap 

frq(elsoc_long_2016_2023$perc_sal_gerente)
frq(elsoc_long_2016_2023$perc_sal_obrero)

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(perc_sal_obrero=replace(perc_sal_obrero, perc_sal_obrero <= 40000  | perc_sal_obrero>= 1000000, NA)) %>%
  mutate(perc_sal_gerente=replace(perc_sal_gerente, perc_sal_gerente <= 250000 | perc_sal_gerente>= 100000001, NA)) 


elsoc_long_2016_2023$perc_inequality <- SciViews::ln(elsoc_long_2016_2023$perc_sal_gerente/elsoc_long_2016_2023$perc_sal_obrero)

summary(elsoc_long_2016_2023$perc_inequality)

elsoc_long_2016_2023$perc_inequality <- sjlabelled::set_label(elsoc_long_2016_2023$perc_inequality, 
                                                              label = "Inequality gap perception")


# just inequality gap

frq(elsoc_long_2016_2023$just_sal_gerente)
frq(elsoc_long_2016_2023$just_sal_obrero)

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(just_sal_obrero=replace(just_sal_obrero, just_sal_obrero <= 40000  | just_sal_obrero>=999999999999999, NA)) %>%
  mutate(just_sal_gerente=replace(just_sal_gerente, just_sal_gerente <= 100000 | just_sal_gerente>=999999999999999, NA)) 

elsoc_long_2016_2023$just_inequality <- 
  SciViews::ln(elsoc_long_2016_2023$just_sal_gerente/elsoc_long_2016_2023$just_sal_obrero)

summary(elsoc_long_2016_2023$just_inequality)

elsoc_long_2016_2023$just_inequality <- sjlabelled::set_label(elsoc_long_2016_2023$just_inequality, 
                        label = "Inequality gap justification")



# Educ

elsoc_long_2016_2023$educ <- 
  car::recode(elsoc_long_2016_2023$m01,
              "c(1,2,3,4,5,6,7)=1;c(8,9,10)=2; c(-888,-999)=NA")
elsoc_long_2016_2023$educ <-
  factor(elsoc_long_2016_2023$educ,
         labels = c("Less than Universitary","Universitary"))

#reverse education, reference level is the highest level
#elsoc_long_2016_2023$educ <- forcats::fct_rev(elsoc_long_2016_2023$educ)

elsoc_long_2016_2023$educ <- 
  sjlabelled::set_label(x = elsoc_long_2016_2023$educ,
                        label = "Education")
sjmisc::frq(elsoc_long_2016_2023$educ)

#Recoding of education to years based on casen 2017.
elsoc_long_2016_2023$educyear<- as.numeric(
  car::recode(elsoc_long_2016_2023$m01, 
              "1=0;2=4.3;3=7.5;4=9.8;5=12.02;6=13.9;
               7=14.8;8=14.9;9=16.9;10=19.07;c(-888,-999)=NA", 
              as.numeric = T))

elsoc_long_2016_2023$educyear <- 
  sjlabelled::set_label(x = elsoc_long_2016_2023$educyear,
                        label = "Education in years")

class(elsoc_long_2016_2023$educyear)
sjmisc::frq(elsoc_long_2016_2023$educyear)

# Ess

sjmisc::frq(elsoc_long_2016_2023$ess)

elsoc_long_2016_2023$ess <- as.numeric(elsoc_long_2016_2023$ess)

elsoc_long_2016_2023$ess <-  sjlabelled::set_label(x = elsoc_long_2016_2023$ess,
                                                   label = "Subjective Social Status")

# Ideo
frq(elsoc_long_2016_2023$ideo)

elsoc_long_2016_2023$ideo<-
  factor(
    car::recode(
      elsoc_long_2016_2023$ideo,
      "c(11,12,-888,-999)='Does not identify';c(0,1,2,3,4)='Left';
     c(5)='Center';c(6,7,8,9,10)='Right'"
    ),
    levels = c('Left', 'Center', 'Right', 'Does not identify')
  )

elsoc_long_2016_2023$ideo<- factor(elsoc_long_2016_2023$ideo,levels = levels(elsoc_long_2016_2023$ideo))

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
         merit_effort, merit_talent, perc_inequality, just_inequality, 
         perc_inequality2, educ, educyear, ess, ideo,
         nacionalidad, sexo, m0_edad, etnia, hogar_bip_f,
         educ_orig_f, isei08_orig, 
         isei08_ocupa)

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

db$terc_orig <- ntile(db$isei08_orig, n = 3)

# Reemplazar estratos con los terciles
db$estrato_orig <- db$terc_orig
db$estrato_ocupa <- db$terc_ocupa

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

table(db$estrato_orig, db$estrato_ocupa)

db %>% 
  group_by(estrato_orig, estrato_ocupa) %>% 
  tally()

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
  select(idencuesta, ola, low_orig, m0_edad, sexo, nacionalidad, etnia, hogar_bip_f,
         educ_orig_f) %>% na.omit()

m1 <- multinom(
  low_orig ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip_f) + educ_orig_f,
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
    select(idencuesta, ola, low_orig, plow_orig0, plow_orig1, plow_orig2,
           wlow_orig1, wlow_orig2),
  by = c("idencuesta", "ola"))

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
  select(idencuesta, ola, m_orig, m0_edad, sexo, nacionalidad, etnia, hogar_bip_f,
         educ_orig_f) %>% na.omit()

m2 <- multinom(
  m_orig ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip_f) + educ_orig_f,
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
    select(idencuesta, ola, m_orig, pm_mm, pm_ml, pm_mh, wm_ml, wm_mh),
  by = c("idencuesta", "ola"))

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
  select(idencuesta, ola, h_orig, m0_edad, sexo, nacionalidad, etnia, hogar_bip_f,
         educ_orig_f) %>% na.omit()

m3 <- multinom(
  h_orig ~ m0_edad + factor(sexo) + factor(nacionalidad) + factor(etnia) + 
    factor(hogar_bip_f) + educ_orig_f,
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
    select(idencuesta, ola, h_orig, ph_hh, ph_hl, ph_hm, wh_hl, wh_hm),
  by = c("idencuesta", "ola"))

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

library(texreg)
library(estimatr)

### Just inequality gap

### Low to middle

datos1 %>% 
  select(ola, idencuesta, just_inequality, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(just_inequality = as.numeric(just_inequality)) %>% 
  lm_robust(just_inequality ~ moblm,
            weights = wlow_orig1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Low to high

datos1 %>% 
  select(idencuesta, ola, just_inequality, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(just_inequality = as.numeric(just_inequality)) %>% 
  lm_robust(just_inequality ~ moblh,
            weights = wlow_orig2,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()  

### middle to high

datos2 %>% 
  select(idencuesta, ola, just_inequality, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(just_inequality = as.numeric(just_inequality)) %>% 
  lm_robust(just_inequality ~ mobmh,
            weights = wm_mh,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### middle to low

datos2 %>% 
  select(idencuesta, ola, just_inequality, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(just_inequality = as.numeric(just_inequality)) %>% 
  lm_robust(just_inequality ~ mobml,
            weights = wm_ml,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to middle

datos3 %>% 
  select(idencuesta, ola, just_inequality, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(just_inequality = as.numeric(just_inequality)) %>% 
  lm_robust(just_inequality ~ mobhm,
            weights = wh_hm,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to low

datos3 %>% 
  select(idencuesta, ola, just_inequality, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(just_inequality = as.numeric(just_inequality)) %>% 
  lm_robust(just_inequality ~ mobhl,
            weights = wh_hl,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### ESS

### Low to middle

datos1 %>% 
  select(ola, idencuesta, ess, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(ess = as.numeric(ess)) %>% 
  lm_robust(ess ~ moblm,
            weights = wlow_orig1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Low to high

datos1 %>% 
  select(idencuesta, ola, ess, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(ess = as.numeric(ess)) %>% 
  lm_robust(ess ~ moblh,
            weights = wlow_orig2,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()  

### middle to high

datos2 %>% 
  select(idencuesta, ola, ess, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(ess = as.numeric(ess)) %>% 
  lm_robust(ess ~ mobmh,
            weights = wm_mh,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### middle to low

datos2 %>% 
  select(idencuesta, ola, ess, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(ess = as.numeric(ess)) %>% 
  lm_robust(ess ~ mobml,
            weights = wm_ml,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to middle

datos3 %>% 
  select(idencuesta, ola, ess, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(ess = as.numeric(ess)) %>% 
  lm_robust(ess ~ mobhm,
            weights = wh_hm,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to low

datos3 %>% 
  select(idencuesta, ola, ess, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(ess = as.numeric(ess)) %>% 
  lm_robust(ess ~ mobhl,
            weights = wh_hl,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Educ 

### Low to middle

datos1 %>% 
  select(ola, idencuesta, educ, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(educ = as.numeric(educ)) %>% 
  lm_robust(educ ~ moblm,
            weights = wlow_orig1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Low to high

datos1 %>% 
  select(idencuesta, ola, educ, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(educ = as.numeric(educ)) %>% 
  lm_robust(educ ~ moblh,
            weights = wlow_orig2,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()  

### middle to high

datos2 %>% 
  select(idencuesta, ola, educ, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(educ = as.numeric(educ)) %>% 
  lm_robust(educ ~ mobmh,
            weights = wm_mh,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### middle to low

datos2 %>% 
  select(idencuesta, ola, educ, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(educ = as.numeric(educ)) %>% 
  lm_robust(educ ~ mobml,
            weights = wm_ml,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to middle

datos3 %>% 
  select(idencuesta, ola, educ, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(educ = as.numeric(educ)) %>% 
  lm_robust(educ ~ mobhm,
            weights = wh_hm,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to low

datos3 %>% 
  select(idencuesta, ola, educ, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(educ = as.numeric(educ)) %>% 
  lm_robust(educ ~ mobhl,
            weights = wh_hl,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()
### educyearyears

### Low to middle

datos1 %>% 
  select(ola, idencuesta, educyear, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(educyear = as.numeric(educyear)) %>% 
  lm_robust(educyear ~ moblm,
            weights = wlow_orig1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Low to high

datos1 %>% 
  select(idencuesta, ola, educyear, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(educyear = as.numeric(educyear)) %>% 
  lm_robust(educyear ~ moblh,
            weights = wlow_orig2,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()  

### middle to high

datos2 %>% 
  select(idencuesta, ola, educyear, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(educyear = as.numeric(educyear)) %>% 
  lm_robust(educyear ~ mobmh,
            weights = wm_mh,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### middle to low

datos2 %>% 
  select(idencuesta, ola, educyear, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(educyear = as.numeric(educyear)) %>% 
  lm_robust(educyear ~ mobml,
            weights = wm_ml,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to middle

datos3 %>% 
  select(idencuesta, ola, educyear, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(educyear = as.numeric(educyear)) %>% 
  lm_robust(educyear ~ mobhm,
            weights = wh_hm,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to low

datos3 %>% 
  select(idencuesta, ola, educyear, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(educyear = as.numeric(educyear)) %>% 
  lm_robust(educyear ~ mobhl,
            weights = wh_hl,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Ideo

db$ideo <- if_else(db$ideo == "Right", 1, 0)

### Low to middle

datos1 %>% 
  select(ola, idencuesta, ideo, moblm, wlow_orig1) %>% 
  na.omit() %>% 
  mutate(ideo = as.numeric(ideo)) %>% 
  lm_robust(ideo ~ moblm,
            weights = wlow_orig1,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### Low to high

datos1 %>% 
  select(idencuesta, ola, ideo, moblh, wlow_orig2) %>% 
  na.omit() %>% 
  mutate(ideo = as.numeric(ideo)) %>% 
  lm_robust(ideo ~ moblh,
            weights = wlow_orig2,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()  

### middle to high

datos2 %>% 
  select(idencuesta, ola, ideo, mobmh, wm_mh) %>% 
  na.omit() %>% 
  mutate(ideo = as.numeric(ideo)) %>% 
  lm_robust(ideo ~ mobmh,
            weights = wm_mh,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### middle to low

datos2 %>% 
  select(idencuesta, ola, ideo, mobml, wm_ml) %>% 
  na.omit() %>% 
  mutate(ideo = as.numeric(ideo)) %>% 
  lm_robust(ideo ~ mobml,
            weights = wm_ml,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to middle

datos3 %>% 
  select(idencuesta, ola, ideo, mobhm, wh_hm) %>% 
  na.omit() %>% 
  mutate(ideo = as.numeric(ideo)) %>% 
  lm_robust(ideo ~ mobhm,
            weights = wh_hm,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

### High to low

datos3 %>% 
  select(idencuesta, ola, ideo, mobhl, wh_hl) %>% 
  na.omit() %>% 
  mutate(ideo = as.numeric(ideo)) %>% 
  lm_robust(ideo ~ mobhl,
            weights = wh_hl,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% screenreg()

