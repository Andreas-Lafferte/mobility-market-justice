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
               cobalt,
               conflicted)

conflict_prefer("select", "dplyr")
options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc/data_v1.RData"))

# 3. Balance -----------------------------------------------------------

# Covariables pre-tratamiento (las de tus modelos de PS)
covars_ps <- c("m0_edad","sexo","nacionalidad","etnia","hogar_bip_f","educ_orig_f")

# -----------------------------------------------------------------------
# A) MIDDLE -> HIGH  (datos2: t = mobmh, w = wm_mh; PS: pm_mh (t=1), pm_mm (t=0))
# -----------------------------------------------------------------------
data_mh_att <- datos2 %>%
  select(idencuesta, t = mobmh, pm_mm, pm_mh,
         m0_edad, sexo, nacionalidad, etnia, hogar_bip_f, educ_orig_f) %>%
  na.omit() %>%
  mutate(ps_pair = pm_mh / (pm_mh + pm_mm),
         w_att   = ifelse(t == 1, 1, ps_pair / (1 - ps_pair)))

# Balance SMD + Razón de varianzas
bt_mh_ATT <- cobalt::bal.tab(
  t ~ m0_edad + sexo + nacionalidad + etnia + hogar_bip_f + educ_orig_f,
  data     = data_mh_att,
  weights  = list(IPW = data_mh_att$w_att),
  estimand = "ATT",
  un = TRUE,
  s.d.denom = "pooled",
  disp.v.ratio = TRUE,
  m.threshold = 0.10,
  v.threshold = 2
)
print(bt_mh_ATT)

# Love plot
cobalt::love.plot(bt_mh_ATT, stats = "m", abs = TRUE, threshold = .10,
                  var.order = "unadjusted", line = TRUE,
                  colors = c("grey70","black")) +
  ggtitle("Balance SMD — mobmh (ATT)")

# Tamaño efectivo y resumen de pesos
ESS_mh_ATT <- (sum(data_mh_att$w_att))^2 / sum((data_mh_att$w_att)^2); ESS_mh_ATT
summary(data_mh_att$w_att)
quantile(data_mh_att$w_att, c(.5,.9,.95,.99,.995), na.rm = TRUE)












# -----------------------------------------------------------------------
# B) MIDDLE -> LOW  (datos2: t = mobml, w = wm_ml; PS: pm_ml (t=1), pm_mm (t=0))
# -----------------------------------------------------------------------
data_ml <- datos2 %>%
  select(idencuesta, t = mobml, w = wm_ml, all_of(covars_ps)) %>%
  na.omit() %>% mutate(w = w / mean(w))

bt_ml <- bal.tab(t ~ m0_edad + sexo + nacionalidad + etnia + hogar_bip_f + educ_orig_f,
                 data = data_ml, weights = data_ml$w, estimand = "ATE",
                 un = TRUE, s.d.denom = "pooled", disp.v.ratio = TRUE,
                 m.threshold = .10, v.threshold = c(0.5,2))
print(bt_ml)

love.plot(bt_ml, stats = "m", abs = TRUE, threshold = .10,
          var.order = "unadjusted", line = TRUE,
          colors = c("grey70","black")) +
  ggtitle("Balance SMD — mobml (middle→low)")

ESS_ml <- (sum(data_ml$w))^2 / sum(data_ml$w^2); ESS_ml
summary(data_ml$w); quantile(data_ml$w, c(.5,.9,.95,.99,.995), na.rm = TRUE)

data_ml_ps <- data_ml %>%
  left_join(dplyr::select(datos2, idencuesta, pm_mm, pm_ml), by = "idencuesta") %>%
  mutate(ps = ifelse(t == 1, pm_ml, pm_mm))
ggplot(data_ml_ps, aes(x = ps, fill = factor(t), color = factor(t))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = .3, position = "identity") +
  labs(x = "Propensity score", title = "Distribución del PS — mobml") +
  theme_minimal()

# -----------------------------------------------------------------------
# C) LOW -> MIDDLE  (datos1: t = moblm, w = wlow_orig1; PS: plow_orig1 (t=1), plow_orig0 (t=0))
# -----------------------------------------------------------------------
data_lm <- datos1 %>%
  select(idencuesta, t = moblm, w = wlow_orig1, all_of(covars_ps)) %>%
  na.omit() %>% mutate(w = w / mean(w))

bt_lm <- bal.tab(t ~ m0_edad + sexo + nacionalidad + etnia + hogar_bip_f + educ_orig_f,
                 data = data_lm, weights = data_lm$w, estimand = "ATE",
                 un = TRUE, s.d.denom = "pooled", disp.v.ratio = TRUE,
                 m.threshold = .10, v.threshold = c(0.5,2))
print(bt_lm)

love.plot(bt_lm, stats = "m", abs = TRUE, threshold = .10,
          var.order = "unadjusted", line = TRUE,
          colors = c("grey70","black")) +
  ggtitle("Balance SMD — moblm (low→middle)")

ESS_lm <- (sum(data_lm$w))^2 / sum(data_lm$w^2); ESS_lm
summary(data_lm$w); quantile(data_lm$w, c(.5,.9,.95,.99,.995), na.rm = TRUE)

data_lm_ps <- data_lm %>%
  left_join(dplyr::select(datos1, idencuesta, plow_orig0, plow_orig1), by = "idencuesta") %>%
  mutate(ps = ifelse(t == 1, plow_orig1, plow_orig0))
ggplot(data_lm_ps, aes(x = ps, fill = factor(t), color = factor(t))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = .3, position = "identity") +
  labs(x = "Propensity score", title = "Distribución del PS — moblm") +
  theme_minimal()

# -----------------------------------------------------------------------
# D) LOW -> HIGH  (datos1: t = moblh, w = wlow_orig2; PS: plow_orig2 (t=1), plow_orig0 (t=0))
# -----------------------------------------------------------------------
data_lh <- datos1 %>%
  select(idencuesta, t = moblh, w = wlow_orig2, all_of(covars_ps)) %>%
  na.omit() %>% mutate(w = w / mean(w))

bt_lh <- bal.tab(t ~ m0_edad + sexo + nacionalidad + etnia + hogar_bip_f + educ_orig_f,
                 data = data_lh, weights = data_lh$w, estimand = "ATE",
                 un = TRUE, s.d.denom = "pooled", disp.v.ratio = TRUE,
                 m.threshold = .10, v.threshold = c(0.5,2))
print(bt_lh)

love.plot(bt_lh, stats = "m", abs = TRUE, threshold = .10,
          var.order = "unadjusted", line = TRUE,
          colors = c("grey70","black")) +
  ggtitle("Balance SMD — moblh (low→high)")

ESS_lh <- (sum(data_lh$w))^2 / sum(data_lh$w^2); ESS_lh
summary(data_lh$w); quantile(data_lh$w, c(.5,.9,.95,.99,.995), na.rm = TRUE)

data_lh_ps <- data_lh %>%
  left_join(dplyr::select(datos1, idencuesta, plow_orig0, plow_orig2), by = "idencuesta") %>%
  mutate(ps = ifelse(t == 1, plow_orig2, plow_orig0))
ggplot(data_lh_ps, aes(x = ps, fill = factor(t), color = factor(t))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = .3, position = "identity") +
  labs(x = "Propensity score", title = "Distribución del PS — moblh") +
  theme_minimal()

# -----------------------------------------------------------------------
# E) HIGH -> MIDDLE  (datos3: t = mobhm, w = wh_hm; PS: ph_hm (t=1), ph_hh (t=0))
# -----------------------------------------------------------------------
data_hm <- datos3 %>%
  select(idencuesta, t = mobhm, w = wh_hm, all_of(covars_ps)) %>%
  na.omit() %>% mutate(w = w / mean(w))

bt_hm <- bal.tab(t ~ m0_edad + sexo + nacionalidad + etnia + hogar_bip_f + educ_orig_f,
                 data = data_hm, weights = data_hm$w, estimand = "ATE",
                 un = TRUE, s.d.denom = "pooled", disp.v.ratio = TRUE,
                 m.threshold = .10, v.threshold = c(0.5,2))
print(bt_hm)

love.plot(bt_hm, stats = "m", abs = TRUE, threshold = .10,
          var.order = "unadjusted", line = TRUE,
          colors = c("grey70","black")) +
  ggtitle("Balance SMD — mobhm (high→middle)")

ESS_hm <- (sum(data_hm$w))^2 / sum(data_hm$w^2); ESS_hm
summary(data_hm$w); quantile(data_hm$w, c(.5,.9,.95,.99,.995), na.rm = TRUE)

data_hm_ps <- data_hm %>%
  left_join(dplyr::select(datos3, idencuesta, ph_hh, ph_hm), by = "idencuesta") %>%
  mutate(ps = ifelse(t == 1, ph_hm, ph_hh))
ggplot(data_hm_ps, aes(x = ps, fill = factor(t), color = factor(t))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = .3, position = "identity") +
  labs(x = "Propensity score", title = "Distribución del PS — mobhm") +
  theme_minimal()

# -----------------------------------------------------------------------
# F) HIGH -> LOW  (datos3: t = mobhl, w = wh_hl; PS: ph_hl (t=1), ph_hh (t=0))
# -----------------------------------------------------------------------
data_hl <- datos3 %>%
  select(idencuesta, t = mobhl, w = wh_hl, all_of(covars_ps)) %>%
  na.omit() %>% mutate(w = w / mean(w))

bt_hl <- bal.tab(t ~ m0_edad + sexo + nacionalidad + etnia + hogar_bip_f + educ_orig_f,
                 data = data_hl, weights = data_hl$w, estimand = "ATE",
                 un = TRUE, s.d.denom = "pooled", disp.v.ratio = TRUE,
                 m.threshold = .10, v.threshold = c(0.5,2))
print(bt_hl)

love.plot(bt_hl, stats = "m", abs = TRUE, threshold = .10,
          var.order = "unadjusted", line = TRUE,
          colors = c("grey70","black")) +
  ggtitle("Balance SMD — mobhl (high→low)")

ESS_hl <- (sum(data_hl$w))^2 / sum(data_hl$w^2); ESS_hl
summary(data_hl$w); quantile(data_hl$w, c(.5,.9,.95,.99,.995), na.rm = TRUE)

data_hl_ps <- data_hl %>%
  left_join(dplyr::select(datos3, idencuesta, ph_hh, ph_hl), by = "idencuesta") %>%
  mutate(ps = ifelse(t == 1, ph_hl, ph_hh))
ggplot(data_hl_ps, aes(x = ps, fill = factor(t), color = factor(t))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = .3, position = "identity") +
  labs(x = "Propensity score", title = "Distribución del PS — mobhl") +
  theme_minimal()

# ===== Fin balance ======================================================
