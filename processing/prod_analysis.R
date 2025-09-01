# 0. Identification ---------------------------------------------------

# Title: Data analysis for thesis project Social Mobilty and Market Justice Preferencees
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code to data analysis for thesis project
# Date: May 1, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               here,
               sjmisc,
               PSweight,
               texreg,
               estimatr,
               mediation,
               conflicted)

conflict_prefer("select", "dplyr")
options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc/data_v1.RData"))

# 3. Analysis -----------------------------------------------------------

# 3.1 Regresiones con IPW -----------------------------------------------------------

### Pensiones

### Low to middle

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblm, 
         w = wlow_orig1, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### Low to high

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblh, 
         w = wlow_orig2, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### middle to high

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobmh, 
         w = wm_mh, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### middle to low

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobml, 
         w = wm_ml, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### High to middle

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhm, 
         w = wh_hm, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### High to low

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhl, 
         w = wh_hl, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### Salud

### Low to middle

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_health, 
         t = moblm, 
         w = wlow_orig1, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### Low to high

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_health, 
         t = moblh, 
         w = wlow_orig2, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### middle to high

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_health, 
         t = mobmh, 
         w = wm_mh, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### middle to low

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_health, 
         t = mobml, 
         w = wm_ml, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### High to middle

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_health, 
         t = mobhm, 
         w = wh_hm, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### High to low

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_health, 
         t = mobhl, 
         w = wh_hl, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### Educ

### Low to middle

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_educ, 
         t = moblm, 
         w = wlow_orig1, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### Low to high

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_educ, 
         t = moblh, 
         w = wlow_orig2, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### middle to high

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_educ, 
         t = mobmh, 
         w = wm_mh, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### middle to low

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_educ, 
         t = mobml, 
         w = wm_ml, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


### High to middle

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_educ, 
         t = mobhm, 
         w = wh_hm, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()

### High to low

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_educ, 
         t = mobhl, 
         w = wh_hl, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  na.omit() %>% 
  mutate(w = w / mean(w)) 

data

data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) %>% 
  screenreg()


# 3.2 Mediation (ACME) -----------------------------------------------------------

# --- Parámetros de este contraste ---
base    <- datos2
treat   <- "mobmh"    # middle -> high (1) vs middle -> middle (0)
ipw     <- "wm_mh"
outcome <- "just_pension"
mediator<- "merit_i"  # tu índice meritocrático (esfuerzo+talento)

# --- Data listo para mediation ---

d <- base %>%
  select(idencuesta, ola,
         Y = all_of(outcome),
         T = all_of(treat),
         M = all_of(mediator),
         w = all_of(ipw),
         edad = m0_edad, sexo, nac = nacionalidad, etnia,
         hogar = hogar_bip_f, edu0 = educ_orig_f) %>%
  tidyr::drop_na() %>%
  mutate(
    Y = as.numeric(Y),
    w = w / mean(w)   # normalizar
  )

# --- Modelos ponderados (mismo set de X en ambos) ---
m.mod <- lm(M ~ T + edad + sexo + nac + etnia + hogar + edu0,
            data = d, weights = w)

y.mod <- lm(Y ~ T + M + edad + sexo + nac + etnia + hogar + edu0,
            data = d, weights = w)

# --- ACME/ADE/Total ---
set.seed(123)
med.fit <- mediation::mediate(m.mod, y.mod,
                              treat    = "T",
                              mediator = "M",
                              sims     = 1000,
                              cluster  = d$idencuesta)

summary(med.fit)   # ACME, ADE, Total, Prop. Mediated
plot(med.fit)

# --- Sensibilidad a confusor no medido en M->Y ---
sens <- medsens(med.fit, effect.type = "indirect")
summary(sens)
plot(sens, sens.par = "rho")


# 3.3 Moderation ----------------------------------------------------------

# Misma base y nombres que arriba:
d <- datos2 %>%
  select(idencuesta, ola,
         Y = just_pension,
         T = mobmh,
         w = wm_mh,
         merit = merit_i,
         edad = m0_edad, sexo, nac = nacionalidad, etnia,
         hogar = hogar_bip_f, edu0 = educ_orig_f) %>%
  tidyr::drop_na() %>%
  mutate(Y = as.numeric(Y), w = w/mean(w))

fit_int <- estimatr::lm_robust(
  Y ~ T*merit + edad + sexo + nac + etnia + hogar + edu0 + factor(ola),
  data = d, weights = w,
  se_type = "CR2", clusters = idencuesta
)

texreg::screenreg(fit_int)

# Efecto de T a distintos niveles de meritocracia (márgenes):
library(margins)
marg <- margins::margins(fit_int, variables = "T", at = list(merit = quantile(d$merit, c(.1,.5,.9))))
summary(marg)


