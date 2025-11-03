# 0. Identification ---------------------------------------------------

# Title: Data analysis for thesis project Preferences for the commodification of pensions in Chile: the rol of intergenerational social mobility
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code to data analysis for thesis project
# Date: Nov 2, 2025

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
               summarytools,
               broom)

conflicts_prefer(dplyr::filter)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc/data_v2.3.RData"))

glimpse(db)

# 3. Analysis -----------------------------------------------------------

# 3.1 Descriptive statistics

# Table summary

t1 <- db %>% 
  select(idencuesta, 
         wave, 
         just_pension_f, 
         estrato_orig, 
         estrato_ocupa, 
         merit_i, 
         nacionalidad_f, 
         sexo_f,
         m0_edad_f, 
         etnia_f, 
         hogar_bip_f, 
         educ_orig_f)

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

# Mobility matrix

sjPlot::sjt.xtab(db$estrato_orig, 
                 db$estrato_ocupa, 
                 show.row.prc = T,
                 show.col.prc = T,
                 statistics = NULL)

# MJP per year

g1 <- db %>% 
  mutate(just_pension_f = factor(just_pension_f, levels = c(0,1), labels = c("Disagree", "Agree"))) %>%
  group_by(wave, just_pension_f) %>% 
  count() %>% 
  group_by(wave) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = porcentaje, group = just_pension_f)) +
  geom_col(aes(fill = just_pension_f)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    values = c("#5b59d4", "#c8237d"),
    breaks = c("Agree","Disagree")
  )  +
  coord_flip() +
  geom_text(
    aes(label = paste0(round(porcentaje * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3,
    fontface = "bold"
  ) +
  labs(y = NULL,
       x = NULL,
       fill = NULL,
       caption = "Source: own elaboration with data from ELSOC 2016-2023 (N obs = 3,435)") +
  theme_ggdist() +
  theme(legend.position = "top") 


# MJP per mobility

pdat <- db %>% 
  select(wave, estrato_orig, estrato_ocupa, just_pension_f) %>% 
  drop_na() %>% 
  mutate(
    mobility = paste(estrato_orig, estrato_ocupa, sep = "-"),
    facet = case_when(
      mobility %in% c("High-Middle","High-Low","Middle-Low") ~ "Downward",
      estrato_orig == estrato_ocupa ~ "Inmobile",
      mobility %in% c("Middle-High","Low-Middle","Low-High") ~ "Upward"
    )
  ) %>% 
  group_by(wave, mobility, facet) %>% 
  summarise(y = mean(just_pension_f), .groups = "drop") %>% 
  group_by(mobility, facet) %>% 
  mutate(gm = mean(y)) %>% 
  ungroup()

# datos SOLO para el punto (uno por categoría)
pdat_gm <- pdat %>% distinct(mobility, facet, gm)

g2 <- ggplot(pdat, aes(x = mobility, y = y, group = wave)) +
  geom_col(aes(fill = wave), position = "dodge", alpha = 0.9) +
  # Punto "X" (pch 4) con su propia leyenda (shape)
  geom_point(
    data = pdat_gm,
    aes(x = mobility, y = gm, shape = "All waves"),
    inherit.aes = FALSE,
    size = 3, stroke = 1, color = "black"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
  scale_fill_manual(
    values = c("2016" = "#5b59d4", "2018" = "#b3b3b3ff", "2023" = "#c8237d"),
    name   = "Wave"
  ) +
  scale_shape_manual(                     # <-- leyenda separada para el shape
    name = NULL,                          # o "Marker" si quieres título
    values = c("All waves" = 4)          # 4 = "X"
  ) +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(order = 2, override.aes = list(color = "black"))
  ) +
  facet_wrap(~ facet, scales = "free_x") +
  labs(
    y = "% of Agreement",
    x = NULL,
    caption = "Source: own elaboration with data from ELSOC 2016–2023 (N obs = 3,435)"
  ) +
  theme_ggdist() +
  theme(legend.position = "top")


# 3.2 Pooled WLS with IPW-ATT and CR2 -----------------------------------------------------------

### Pensiones 

### Low to middle

data_lm <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = moblm, 
         w = w_low_lm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_lm <-
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm) 

### Low to high

data_lh <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = moblh, 
         w = w_low_lh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_lh <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh) 

### middle to high

data_mh <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobmh, 
         w = w_mid_mh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_mh <-  
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh) 

### middle to low

data_ml <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobml, 
         w = w_mid_ml_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_ml <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml) 

### High to middle

data_hm <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobhm, 
         w = w_hig_hm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_hm <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm) 


### High to low

data_hl <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobhl, 
         w = w_hig_hl_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_hl <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hl) 

# Table

texreg::screenreg(list(mp_lm,
                       mp_lh,
                       mp_mh,
                       mp_ml,
                       mp_hm,
                       mp_hl),
                  custom.model.names = c(
                    "Low-Middle",
                    "Low-High",
                    "Middle-High",
                    "Middle-Low",
                    "High-Middle",
                    "High-Low"
                  ))


# Coef plot

conflicts_prefer(dplyr::filter)

df_pension <- bind_rows(
  tidy(mp_lm, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("Low-Middle\n", "(N obs. = ", mp_hl$nobs, ";", "\nN clusters = ", mp_hl$nclusters, ")")
    ) 
  ,
  tidy(mp_lh, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("Low-High\n", "(N obs. = ", mp_lh$nobs, ";", "\nN clusters = ", mp_lh$nclusters, ")"))
  ,
  tidy(mp_mh, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("Middle-High\n", "(N obs. = ", mp_mh$nobs, ";", "\nN clusters = ", mp_mh$nclusters, ")"))
  ,
  tidy(mp_ml, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("Middle-Low\n", "(N obs. = ", mp_ml$nobs, ";", "\nN clusters = ", mp_ml$nclusters, ")"))
  ,
  tidy(mp_hm, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("High-Middle\n", "(N obs. = ", mp_hm$nobs, ";", "\nN clusters = ", mp_hm$nclusters, ")"))
  ,
  tidy(mp_hl, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("High-Low\n", "(N obs. = ", mp_hl$nobs, ";", "\nN clusters = ", mp_hl$nclusters, ")"))
) 

df_pension_lab <- df_pension %>%
  dplyr::mutate(
    sig     = ifelse(conf.low * conf.high > 0, "*", ""),  # IC no cruza 0
    est_lab = sprintf("%.2f%s", estimate, sig),
    low_lab = sprintf("%.2f", conf.low),
    high_lab= sprintf("%.2f", conf.high)
  )

df_pension_lab <- df_pension_lab %>%
  dplyr::mutate(
    term = factor(
      term,
      levels = term
    )
  )

g3 <- ggplot(df_pension_lab, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  #geom_text(aes(label = est_lab), hjust = -0.05, size = 4, vjust = -0.5) +
  labs(x = "Estimate", y = NULL) +
  theme_ggdist() 

# 3.3 Heterogeneity effects -------------------------------------------------------

# -----------
# Meritocracy 
# -----------

### Interacciones

### Low to middle

data_lm_i <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = moblm, 
         w = w_low_lm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_lm_i <-
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm_i) 

### Low to high

data_lh_i <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = moblh, 
         w = w_low_lh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>%  
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_lh_i <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh_i) 

### middle to high

data_mh_i <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobmh, 
         w = w_mid_mh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_mh_i <-  
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh_i) 

### middle to low

data_ml_i <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobml, 
         w = w_mid_ml_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_ml_i <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml_i) 

### High to middle

data_hm_i <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobhm, 
         w = w_hig_hm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_hm_i <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm_i) 

### High to low

data_hl_i <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mobhl, 
         w = w_hig_hl_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_hl_i <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hl_i) 

# Table

screenreg(list(mp_lm_i,
               mp_lh_i,
               mp_mh_i,
               mp_ml_i,
               mp_hm_i,
               mp_hl_i),
          custom.model.names = c(
            "Low-Middle",
            "Low-High",
            "Middle-High",
            "Middle-Low",
            "High-Middle",
            "High-Low"
          ))


# Coef plot
df_pension_i <-  bind_rows(
  tidy(mp_lm_i, conf.int = TRUE) %>%
    filter(term %in% c("t:mHigh", "t")) %>%
    mutate(name = paste0("Low-Middle\n", "(N obs. = ", mp_hl_i$nobs, ";", "\nN clusters = ", mp_hl_i$nclusters, ")")
    ) 
  ,
  tidy(mp_lh_i, conf.int = TRUE) %>%
    filter(term %in% c("t:mHigh", "t")) %>%
    mutate(name = paste0("Low-High\n", "(N obs. = ", mp_lh_i$nobs, ";", "\nN clusters = ", mp_lh_i$nclusters, ")"))
  ,
  tidy(mp_mh_i, conf.int = TRUE) %>%
    filter(term %in% c("t:mHigh", "t")) %>%
    mutate(name = paste0("Middle-High\n", "(N obs. = ", mp_mh_i$nobs, ";", "\nN clusters = ", mp_mh_i$nclusters, ")"))
  ,
  tidy(mp_ml_i, conf.int = TRUE) %>%
    filter(term %in% c("t:mHigh", "t")) %>%
    mutate(name = paste0("Middle-Low\n", "(N obs. = ", mp_ml_i$nobs, ";", "\nN clusters = ", mp_ml_i$nclusters, ")"))
  ,
  tidy(mp_hm_i, conf.int = TRUE) %>%
    filter(term %in% c("t:mHigh", "t")) %>%
    mutate(name = paste0("High-Middle\n", "(N obs. = ", mp_hm_i$nobs, ";", "\nN clusters = ", mp_hm_i$nclusters, ")"))
  ,
  tidy(mp_hl_i, conf.int = TRUE) %>%
    filter(term %in% c("t:mHigh", "t")) %>%
    mutate(name = paste0("High-Low\n", "(N obs. = ", mp_hl_i$nobs, ";", "\nN clusters = ", mp_hl_i$nclusters, ")"))
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
    name = factor(
      name,
      levels = unique(name)
    ),
    term = if_else(term == "t", "Low Meritocracy", "High Meritocracy"),
    term = factor(term, levels = c("High Meritocracy", "Low Meritocracy"))
  )


g4 <- ggplot(df_pension_lab_i, aes(x = estimate, y = name)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  facet_wrap(~term) + 
  #geom_text(aes(label = est_lab), hjust = -0.05, size = 4, vjust = -0.5) +
  labs(x = "Estimate", y = NULL) +
  theme_ggdist()

# -----------
# Time 
# -----------

# Low to middle

mp_lm_t <- lm_robust(y ~ t*ola + edad + sexo + nac + etnia + hogar + edu0,
                     weights = w,
                     se_type = "CR2",
                     clusters = idencuesta,
                     data = data_lm) 

# Low to high

mp_lh_t <- lm_robust(y ~ t*ola+ edad + sexo + nac + etnia + hogar + edu0,
                     weights = w,
                     se_type = "CR2",
                     clusters = idencuesta,
                     data = data_lh) 

# Middle to high

mp_mh_t <- lm_robust(y ~ t*ola + edad + sexo + nac + etnia + hogar + edu0,
                     weights = w,
                     se_type = "CR2",
                     clusters = idencuesta,
                     data = data_mh) 


# Middle to low

mp_ml_t <- lm_robust(y ~ t*ola + edad + sexo + nac + etnia + hogar + edu0,
                     weights = w,
                     se_type = "CR2",
                     clusters = idencuesta,
                     data = data_ml) 

# High to middle

# tiempo: descender se vuelve un efecto positivo en la ultima ola (con mi plata no)
mp_hm_t <- lm_robust(y ~ t*ola + edad + sexo + nac + etnia + hogar + edu0,
                     weights = w,
                     se_type = "CR2",
                     clusters = idencuesta,
                     data = data_hm) 

# High to low

mp_hl_t <- lm_robust(y ~ t*ola + edad + sexo + nac + etnia + hogar + edu0,
                     weights = w,
                     se_type = "CR2",
                     clusters = idencuesta,
                     data = data_hl) 

# Table
screenreg(list(mp_lm_t,
               mp_lh_t,
               mp_ml_t,
               mp_mh_t,
               mp_hm_t,
               mp_hl_t),
          custom.model.names = c(
            "Low-Middle",
            "Low-High",
            "Middle-Low",
            "Middle-High",
            "High-Middle",
            "High-Low"
          ))

# 3.4 Sensitivity analysis ------------------------------------------------

# -----------------------------
# Holm
# -----------------------------

p <- c(mp_lm$p.value[2],
       mp_lh$p.value[2],
       mp_ml$p.value[2],
       mp_mh$p.value[2],
       mp_hm$p.value[2],
       mp_hl$p.value[2]
)

p_holm <- p.adjust(p, method = "holm")   # FWER

data.frame(traj = c("Low→Middle","Low→High","Middle→Low","Middle→High","High→Middle","High→Low"),
           p_raw = p, p_holm = p_holm)


# -----------------------------
# E values
# -----------------------------


mods <- list(
  "Low→Middle"  = mp_lm,
  "Low→High"    = mp_lh,
  "Middle→Low"  = mp_ml,
  "Middle→High" = mp_mh,
  "High→Middle" = mp_hm,
  "High→Low"    = mp_hl
)


rr_from_beta <- function(beta, sdY) {
  exp(0.91 * (beta / sdY))
}

rr_to_upper <- function(rr) {
  ifelse(rr < 1, 1/rr, rr)
}

evalue_from_rr <- function(rr) {
  # rr ya debe ser >= 1
  ifelse(rr <= 1, 1, rr + sqrt(rr * (rr - 1)))
}

extract_effects_and_sdY <- function(mod, term_name = "t") {
  tid <- broom::tidy(mod, conf.int = TRUE)
  eff <- tid %>% filter(term == term_name)
  if (nrow(eff) != 1) stop("No encontré el término de tratamiento '", term_name, "' en el modelo.")
  
  mf <- tryCatch(model.frame(mod), error = function(e) NULL)
  if (!is.null(mf)) {
    y <- tryCatch(model.response(mf), error = function(e) NULL)
    w <- tryCatch(model.weights(mf), error = function(e) NULL)
  } else {
    y <- NULL; w <- NULL
  }
  
  if (!is.null(y)) {
    if (!is.null(w)) {
      p_hat <- sum(w * y, na.rm = TRUE) / sum(w, na.rm = TRUE)
    } else {
      p_hat <- mean(y, na.rm = TRUE)
    }
    sdY <- sqrt(p_hat * (1 - p_hat))
    if (!is.finite(sdY) || sdY == 0) sdY <- 0.5
  } else {
    sdY <- 0.5  # fallback
  }
  
  tibble(
    beta   = eff$estimate,
    ci_low = eff$conf.low,
    ci_high= eff$conf.high,
    sdY    = sdY
  )
}


compute_evalues <- function(beta, ci_low, ci_high, sdY) {
  # RR punto
  rr_point <- rr_from_beta(beta, sdY)
  rr_point_u <- rr_to_upper(rr_point)
  E_point <- evalue_from_rr(rr_point_u)
  
  # IC: el límite "más cercano al 0" en la dirección del efecto
  bound <- ifelse(beta >= 0, ci_low, ci_high)
  
  # Si el IC cruza 0, E_CI = 1
  crosses <- (ci_low <= 0 & ci_high >= 0)
  
  rr_ci <- rr_from_beta(bound, sdY)
  rr_ci_u <- rr_to_upper(rr_ci)
  E_ci <- evalue_from_rr(rr_ci_u)
  E_ci[crosses] <- 1
  
  tibble(
    RR_point = rr_point,
    RR_point_upper = rr_point_u,
    E_point  = E_point,
    RR_CI    = rr_ci,
    RR_CI_upper = rr_ci_u,
    E_CI     = E_ci,
    CI_crosses_0 = crosses
  )
}


evalues_tbl <- imap_dfr(mods, function(mod, name) {
  pars <- extract_effects_and_sdY(mod, term_name = "t")
  ev   <- compute_evalues(pars$beta, pars$ci_low, pars$ci_high, pars$sdY)
  bind_cols(
    tibble(traj = name),
    round(bind_cols(pars, ev), 4)
  )
})

kableExtra::kable(evalues_tbl,
                  caption = "E-values for ATT estimates by mobility trajectory (Ding & VanderWeele, 2016 approximation)",
                  align = "lrrrrrrrrr")


