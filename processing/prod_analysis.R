# 0. Identification ---------------------------------------------------

# Title: Data analysis for thesis project Social Mobilty and Market Justice Preferencees
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code to data analysis for thesis project
# Date: May 1, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               here,
               sjPlot,
               sjmisc,
               texreg,
               estimatr,
               conflicted,
               ggdist,
               summarytools)

conflicts_prefer(dplyr::filter)
options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

#load(url("https://github.com/Andreas-Lafferte/mobility-market-justice/raw/refs/heads/main/input/data/proc/data_v1.RData"))

load(here("input/data/proc/data_v2.RData"))

glimpse(db)

# 3. Analysis -----------------------------------------------------------

bases <- c("datos1", "datos2", "datos3", "db")

for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    mutate(
      merit_effort = if_else(merit_effort >= 4, 1, 0),
      merit_effort = factor(merit_effort, levels = c(0,1), labels = c("Baja", "Alta"))
      #just_pension = if_else(
      #  just_pension %in% 4:5, 1L,
      #  if_else(just_pension %in% 1:3, 0L, NA_integer_)),
      #just_health = if_else(
      #  just_health %in% 4:5, 1L,
      #  if_else(just_health %in% 1:3, 0L, NA_integer_)),
      #just_educ = if_else(
      #  just_educ %in% 4:5, 1L,
      #  if_else(just_educ %in% 1:3, 0L, NA_integer_))
    )
  
  assign(nm, df, inherits = TRUE)
}

t1 <- db %>% 
  select(idencuesta, wave, just_pension, estrato_orig, estrato_ocupa, merit_effort, nacionalidad, sexo,
         m0_edad, etnia, hogar_bip_f, educ_orig_f)

dfSummary(t1,
          plain.ascii = FALSE,
          style = "multiline",
          tmp.img.dir = "/tmp",
          graph.magnif = 0.75,
          headings = F,  # encabezado
          varnumbers = F, # num variable
          labels.col = T, # etiquetas
          na.col = F,    # missing
          graph.col = F, # plot
          valid.col = T, # n valido
          col.widths = c(30,10,10,10))

g1 <- db %>% 
  mutate(just_pension = car::recode(just_pension, recodes = c("1='Strongly disagree'; 2='Disagree';
                                          3='Neither agree nor disagree'; 4='Agree';
                                          5='Strongly agree'"), 
                                    levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
                                    as.factor = T)) %>% 
  group_by(ola, just_pension) %>% 
  count() %>% 
  na.omit() %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(wave = case_when(ola == 1 ~ "2016",
                          ola == 2 ~ "2018",
                          ola == 3 ~ "2023"),
         wave = factor(wave, levels = c("2016",
                                        "2018",
                                        "2023"))) %>% 
  ggplot(aes(x = wave, y = porcentaje, group = just_pension)) +
  geom_col(aes(fill = just_pension)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    values = c("#0571B0", "#92C5DE", "#b3b3b3ff", "#F4A582", "#CA0020"),
    breaks = c("Strongly disagree", "Disagree", "Neither agree nor disagree","Agree", "Strongly agree")
  )  +
  coord_flip() +
  geom_text(
    aes(label = paste0(round(porcentaje * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5,
    fontface = "bold"
  ) +
  labs(y = NULL,
       x = NULL,
       fill = NULL,
       title = "It is fair that people with higher incomes have better pensions than people\nwith lower incomes?",
       caption = "Source: own elaboration with data from ELSOC 2016-2023 (N obs = 9,371)") +
  theme_ggdist() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        text = element_text(size = 16)) 

g1

sjPlot::sjt.xtab(db$estrato_orig, 
                 db$estrato_ocupa, 
                 show.row.prc = T,
                 show.col.prc = T,
                 statistics = NULL)

# 3.2 Regresiones con IPW -----------------------------------------------------------

### Pensiones DR

### Low to middle

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblm, 
         w = w_low_lm_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_lm <- data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_lm %>%  
  screenreg()

### Low to high

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblh, 
         w = w_low_lh_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_lh <- data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 
            
mp_lh %>% 
  screenreg()


### middle to high

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobmh, 
         w = w_mid_mh_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_mh <- data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 
    
mp_mh %>% 
  screenreg()

### middle to low

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobml, 
         w = w_mid_ml_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_ml <- data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 
            
mp_ml %>% 
  screenreg()


### High to middle

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhm, 
         w = w_hig_hm_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_hm <- data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 
            
mp_hm %>% 
  screenreg()

### High to low

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhl, 
         w = w_hig_hl_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_hl <- data %>% 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 
            
mp_hl %>% 
  screenreg()

conflicts_prefer(dplyr::filter)

df_pension <- bind_rows(
  tidy(mp_lm, conf.int = TRUE) %>%
    filter(term == "t") %>%
    mutate(term = paste0("Low→Middle\n", "(N obs. = ", mp_hl$nobs, ";", "\nN clusters = ", mp_hl$nclusters, ")")
) 
  ,
  tidy(mp_lh, conf.int = TRUE) %>%
    filter(term == "t") %>%
  mutate(term = paste0("Low→High\n", "(N obs. = ", mp_lh$nobs, ";", "\nN clusters = ", mp_lh$nclusters, ")"))
         ,
  tidy(mp_mh, conf.int = TRUE) %>%
    filter(term == "t") %>%
    mutate(term = paste0("Middle→High\n", "(N obs. = ", mp_mh$nobs, ";", "\nN clusters = ", mp_mh$nclusters, ")"))
           ,
  tidy(mp_ml, conf.int = TRUE) %>%
    filter(term == "t") %>%
  mutate(term = paste0("Middle→Low\n", "(N obs. = ", mp_ml$nobs, ";", "\nN clusters = ", mp_ml$nclusters, ")"))
,
  tidy(mp_hm, conf.int = TRUE) %>%
    filter(term == "t") %>%
  mutate(term = paste0("High→Middle\n", "(N obs. = ", mp_hm$nobs, ";", "\nN clusters = ", mp_hm$nclusters, ")"))
,
  tidy(mp_hl, conf.int = TRUE) %>%
    filter(term == "t") %>%
  mutate(term = paste0("High→Low\n", "(N obs. = ", mp_hl$nobs, ";", "\nN clusters = ", mp_hl$nclusters, ")"))
) 

df_pension_lab <- df_pension %>%
  mutate(
    sig     = ifelse(conf.low * conf.high > 0, "*", ""),  # IC no cruza 0
    est_lab = sprintf("%.2f%s", estimate, sig),
    low_lab = sprintf("%.2f", conf.low),
    high_lab= sprintf("%.2f", conf.high)
  )

df_pension_lab <- df_pension_lab %>%
  mutate(
    term = factor(
      term,
      levels = c(
        "Low→Middle\n(N obs. = 615;\nN clusters = 449)",
        "Low→High\n(N obs. = 485;\nN clusters = 359)",
        "Middle→High\n(N obs. = 462;\nN clusters = 331)",
        "Middle→Low\n(N obs. = 503;\nN clusters = 363)",
        "High→Middle\n(N obs. = 662;\nN clusters = 460)",
        "High→Low\n(N obs. = 615;\nN clusters = 449)"
      )
    )
  )

g2 <- ggplot(df_pension_lab, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 1.2) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  coord_flip() +
  geom_text(aes(label = est_lab), hjust = -0.2, size = 5, vjust = 1.5) +
  labs(x = "Estimate (β)", y = NULL) +
  theme_ggdist() 

# 3.3 Efectos agrupados ---------------------------------------------------

# --- 1) helper para sacar beta_t y SE_t de lm_robust (estimatr) ---
grab_t <- function(mod, lab){
  co <- summary(mod)$coefficients
  stopifnot("t" %in% rownames(co))
  tibble(model = lab,
         beta  = co["t","Estimate"],
         se    = co["t","Std. Error"],
         var   = se^2)
}

# --- 2) extrae de tus 6 modelos ya estimados ---
# Ascensos: L->M (mp_lm), L->H (mp_lh), M->H (mp_mh)
up <- bind_rows(
  grab_t(mp_lm, "L→M"),
  grab_t(mp_lh, "L→H"),
  grab_t(mp_mh, "M→H")
) %>% mutate(direction = "Upward")

# Descensos: M->L (mp_ml), H->M (mp_hm), H->L (mp_hl)
down <- bind_rows(
  grab_t(mp_ml, "M→L"),
  grab_t(mp_hm, "H→M"),
  grab_t(mp_hl, "H→L")
) %>% mutate(direction = "Downward")

eff <- bind_rows(up, down)

# --- 3) meta-análisis fijo por dirección ---
pool_dir <- eff %>%
  group_by(direction) %>%
  summarise(
    k      = n(),
    w_sum  = sum(1/var),
    beta_p = sum((1/var)*beta)/w_sum,            # efecto agrupado
    se_p   = sqrt(1/w_sum),                      # SE agrupado
    z_p    = beta_p / se_p,
    p_p    = 2*pnorm(-abs(z_p)),
    ci_lo  = beta_p - qnorm(0.975)*se_p,
    ci_hi  = beta_p + qnorm(0.975)*se_p,
    # Heterogeneidad (Cochran Q, I2) – informativo
    Q      = sum((1/var)*(beta - beta_p)^2),
    I2     = pmax(0, (Q - (k-1))/Q)*100
  )

print(eff)       # efectos individuales
print(pool_dir)  # efectos agrupados Upward / Downward


# 3.4 Interacciones -------------------------------------------------------


### Interacciones

### Low to middle

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblm, 
         w = w_low_lm_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_effort) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_lm_i <- data %>% 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_lm_i %>%  
  screenreg()

### Low to high

data <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblh, 
         w = w_low_lh_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_effort) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_lh_i <- data %>% 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_lh_i %>% 
  screenreg()


### middle to high

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobmh, 
         w = w_mid_mh_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_effort) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_mh_i <- data %>% 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_mh_i %>% 
  screenreg()

### middle to low

data <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobml, 
         w = w_mid_ml_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_effort) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_ml_i <- data %>% 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_ml_i %>% 
  screenreg()


### High to middle

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhm, 
         w = w_hig_hm_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_effort) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_hm_i <- data %>% 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_hm_i %>% 
  screenreg()

### High to low

data <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhl, 
         w = w_hig_hl_t, 
         edad = m0_edad, 
         sexo, 
         nac = nacionalidad,
         etnia,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_effort) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_hl_i <- data %>% 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = .) 

mp_hl_i %>% 
  screenreg()

df_pension_i <-  bind_rows(
  tidy(mp_lm_i, conf.int = TRUE) %>%
    filter(term == "t:mAlta") %>%
    mutate(term = paste0("Low→Middle\n", "(N obs. = ", mp_hl_i$nobs, ";", "\nN clusters = ", mp_hl_i$nclusters, ")")
    ) 
  ,
  tidy(mp_lh_i, conf.int = TRUE) %>%
    filter(term == "t:mAlta") %>%
    mutate(term = paste0("Low→High\n", "(N obs. = ", mp_lh_i$nobs, ";", "\nN clusters = ", mp_lh_i$nclusters, ")"))
  ,
  tidy(mp_mh_i, conf.int = TRUE) %>%
    filter(term == "t:mAlta") %>%
    mutate(term = paste0("Middle→High\n", "(N obs. = ", mp_mh_i$nobs, ";", "\nN clusters = ", mp_mh_i$nclusters, ")"))
  ,
  tidy(mp_ml_i, conf.int = TRUE) %>%
    filter(term == "t:mAlta") %>%
    mutate(term = paste0("Middle→Low\n", "(N obs. = ", mp_ml_i$nobs, ";", "\nN clusters = ", mp_ml_i$nclusters, ")"))
  ,
  tidy(mp_hm_i, conf.int = TRUE) %>%
    filter(term == "t:mAlta") %>%
    mutate(term = paste0("High→Middle\n", "(N obs. = ", mp_hm_i$nobs, ";", "\nN clusters = ", mp_hm_i$nclusters, ")"))
  ,
  tidy(mp_hl_i, conf.int = TRUE) %>%
    filter(term == "t:mAlta") %>%
    mutate(term = paste0("High→Low\n", "(N obs. = ", mp_hl_i$nobs, ";", "\nN clusters = ", mp_hl_i$nclusters, ")"))
) 

df_pension_lab_i <- df_pension_i %>%
  mutate(
    sig     = ifelse(conf.low * conf.high > 0, "*", ""),  # IC no cruza 0
    est_lab = sprintf("%.2f%s", estimate, sig),
    low_lab = sprintf("%.2f", conf.low),
    high_lab= sprintf("%.2f", conf.high)
  )

df_pension_lab_i <- df_pension_lab_i %>%
  mutate(
    term = factor(
      term,
      levels = c(
        "Low→Middle\n(N obs. = 615;\nN clusters = 449)",
        "Low→High\n(N obs. = 481;\nN clusters = 356)",
        "Middle→High\n(N obs. = 460;\nN clusters = 329)",
        "Middle→Low\n(N obs. = 501;\nN clusters = 362)",
        "High→Middle\n(N obs. = 662;\nN clusters = 460)",
        "High→Low\n(N obs. = 615;\nN clusters = 449)"
      )
    )
  )

g3 <- ggplot(df_pension_lab_i, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 1.2) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  coord_flip() +
  geom_text(aes(label = est_lab), hjust = -0.2, size = 5, vjust = 1.5) +
  labs(x = "Estimate (β)", y = NULL) +
  theme_ggdist() 
g3
