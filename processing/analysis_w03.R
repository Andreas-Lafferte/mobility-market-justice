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

db <- subset(db, ola == 3)

bases <- c("datos1", "datos2", "datos3")

for (nm in bases) {
  df <- get(nm)
  
  df <- subset(df, ola == 3
    )
  
  assign(nm, df, inherits = TRUE)
}

# 3. Analysis -----------------------------------------------------------

my_pretty_theme <- function(border_width = 0.4) {
  theme_ggdist() %+replace%
    theme(
      panel.border = element_blank(),
      axis.line.x  = element_line(colour = "black", linewidth = border_width),
      axis.line.y  = element_line(colour = "black", linewidth = border_width),
      axis.ticks = element_line(colour = "black", linewidth = border_width),
      axis.text = element_text(colour = "black"),
      #legend.text = element_text(size = 12),
      #plot.caption = element_text(size = 11, hjust = 1)
    )
}


# 3.1 Descriptive statistics

# Table summary

t1 <- db %>% 
  select(just_pension_f, 
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
  mutate(just_pension_f = factor(just_pension_f, levels = c(0,1), 
                                 labels = c("Disagreement", "Agreement"))) %>%
  group_by(wave, just_pension_f) %>% 
  count() %>% 
  group_by(wave) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = porcentaje, group = just_pension_f)) +
  geom_col(aes(fill = just_pension_f)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    values = c("#DE4968FF", "grey20"),
    breaks = c("Agreement","Disagreement")
  )  +
  coord_flip() +
  geom_text(
    aes(label = paste0(round(porcentaje * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  labs(y = NULL,
       x = NULL,
       fill = NULL,
       caption = "Source: own elaboration with data from ELSOC 2016-2023 (N obs = 3,435)\nNote: Agreement 5-4, Disagreement 1-3") +
  my_pretty_theme() +
  theme(legend.position = "bottom") 


# MJP per mobility

pdat <- db %>% 
  select(wave, estrato_orig, estrato_ocupa, just_pension_f) %>% 
  drop_na() %>% 
  mutate(
    mobility = paste(estrato_orig, estrato_ocupa, sep = "-"),
    mobility = factor(mobility, 
                      levels = c("High-Middle",
                                 "High-Low",
                                 "Middle-Low",
                                 "Low-Low",
                                 "Middle-Middle",
                                 "High-High",
                                 "Low-Middle",
                                 "Low-High",
                                 "Middle-High")),
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

pdat <- pdat %>% 
  pivot_longer(cols = c(y, gm),
               names_to = "name",
               values_to = "value") %>% 
  mutate(wave = if_else(name == "gm", "All waves", wave),
         wave = factor(wave, levels = c("2016", "2018", "2023", "All waves")))


# datos SOLO para el punto (uno por categoría)
#pdat_gm <- pdat %>% distinct(mobility, facet, gm)

breaks_w <- c("2016", "2018", "2023", "All waves")

g2 <- ggplot(pdat, aes(x = mobility, y = value, group = wave)) +
  geom_point(aes(color = wave, shape = wave), 
             size = 3, stroke = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
  scale_color_manual(
    name   = "Wave",
    breaks = breaks_w,
    values = c("2016" = "#DE4968FF",
               "2018" = "#8C2981FF",
               "2023" = "#3B0F70FF",
               "All waves" = "#000004FF")) +
  scale_shape_manual(
    name   = "Wave",
    breaks = breaks_w,
    values = c("2016" = 16,  # círculo
               "2018" = 15,  # triángulo
               "2023" = 17,  # cuadrado
               "All waves" = 4)  # X
  ) +
  facet_wrap(~ facet, scales = "free_x") +
  labs(
    y = "% of Agreement",
    x = NULL,
    caption = "Source: own elaboration with data from ELSOC 2016-2023 (N obs = 3,435)"
  ) +
  my_pretty_theme() +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45,hjust = 1))


# 3.2 Pooled WLS with IPW-ATT and CR2 -----------------------------------------------------------

### Pensiones 

### Low to middle

data_lm <- datos1 %>% 
  select(idencuesta, 
         y = just_pension_f, 
         t = moblm, 
         w = w_low_lm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w))

mp_lm <-
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm) 

### Low to high

data_lh <- datos1 %>% 
  select(idencuesta, 
         y = just_pension_f, 
         t = moblh, 
         w = w_low_lh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w))

mp_lh <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh) 

### middle to high

data_mh <- datos2 %>% 
  select(idencuesta, 
         y = just_pension_f, 
         t = mobmh, 
         w = w_mid_mh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_mh <-  
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh) 

### middle to low

data_ml <- datos2 %>% 
  select(idencuesta, 
         y = just_pension_f, 
         t = mobml, 
         w = w_mid_ml_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w))

mp_ml <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml) 

### High to middle

data_hm <- datos3 %>% 
  select(idencuesta, 
         y = just_pension_f, 
         t = mobhm, 
         w = w_hig_hm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w))

mp_hm <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm) 


### High to low

data_hl <- datos3 %>% 
  select(idencuesta, 
         y = just_pension_f, 
         t = mobhl, 
         w = w_hig_hl_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_hl <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0,
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
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 2, 
                  size = 1, color = "#000004FF") +
  #geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.7) +
  #geom_point(size = 2.5) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  #geom_text(aes(label = est_lab), hjust = -0.05, size = 4, vjust = -0.5) +
  labs(x = "Treatment Effect", y = NULL) +
  my_pretty_theme()

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

library(dplyr)
library(broom)

get_marginals_merit <- function(model, label){
  b <- coef(model)[c("t", "t:mHigh")]
  
  V <- vcov(model)[c("t", "t:mHigh"),
                   c("t", "t:mHigh")]
  L <- rbind(
    "Low Meritocracy"  = c(1, 0),
    "High Meritocracy" = c(1, 1)
  )
  
  est  <- as.numeric(L %*% b)
  se   <- sqrt(diag(L %*% V %*% t(L)))
  z    <- 1.96
  ci_l <- est - z * se
  ci_h <- est + z * se
  
  tibble(
    term     = rownames(L),
    estimate = est,
    std.error = se,
    conf.low = ci_l,
    conf.high = ci_h,
    name = paste0(
      label, "\n",
      "(N obs. = ", model$nobs, ";",
      "\nN clusters = ", model$nclusters, ")"
    )
  )
}


df_pension_i <- bind_rows(
  get_marginals_merit(mp_lm_i, "Low-Middle"),
  get_marginals_merit(mp_lh_i, "Low-High"),
  get_marginals_merit(mp_mh_i, "Middle-High"),
  get_marginals_merit(mp_ml_i, "Middle-Low"),
  get_marginals_merit(mp_hm_i, "High-Middle"),
  get_marginals_merit(mp_hl_i, "High-Low")
)

df_pension_lab_i <- df_pension_i %>%
  mutate(
    sig     = ifelse(conf.low * conf.high > 0, "*", ""),  # IC no cruza 0
    est_lab = sprintf("%.2f%s", estimate, sig),
    low_lab = sprintf("%.2f", conf.low),
    high_lab= sprintf("%.2f", conf.high),
    name = factor(name, levels = unique(name)),
    term = factor(term, levels = c("High Meritocracy", "Low Meritocracy"))
  )

pos <- position_dodge(width = 0.55)

g4 <- ggplot(
  df_pension_lab_i,
  aes(x = estimate, y = name, group = term, colour = term, shape = term)
) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  fatten = 2.5,
                  size = 1,
                  position = pos) +
  scale_shape_manual(values = c(
    "High Meritocracy" = 16,  # círculo
    "Low Meritocracy"  = 15   # cuadrado
  )) +
  scale_colour_manual(values = c(
    "High Meritocracy" = "#000004FF",
    "Low Meritocracy"  = "#DE4968FF"
  ))+
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  labs(x = "Marginal Effect", y = NULL, colour = NULL, shape = NULL, linetype = NULL) +
  my_pretty_theme() +
  theme(legend.position = "bottom")

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

get_marginals_wave <- function(model, label){
  
  b <- coef(model)[c("t", "t:ola2018", "t:ola2023")]
  
  V <- vcov(model)[c("t", "t:ola2018", "t:ola2023"),
                   c("t", "t:ola2018", "t:ola2023")]
  L <- rbind(
    "2016" = c(1, 0, 0),
    "2018" = c(1, 1, 0),
    "2023" = c(1, 0, 1)
  )
  
  est  <- as.numeric(L %*% b)
  se   <- sqrt(diag(L %*% V %*% t(L)))
  z    <- 1.96
  ci_l <- est - z * se
  ci_h <- est + z * se
  
  tibble(
    term     = rownames(L),
    estimate = est,
    std.error = se,
    conf.low = ci_l,
    conf.high = ci_h,
    name = paste0(
      label, "\n",
      "(N obs. = ", model$nobs, ";",
      "\nN clusters = ", model$nclusters, ")"
    )
  )
}


df_pension_t <- bind_rows(
  get_marginals_wave(mp_lm_t, "Low-Middle"),
  get_marginals_wave(mp_lh_t, "Low-High"),
  get_marginals_wave(mp_mh_t, "Middle-High"),
  get_marginals_wave(mp_ml_t, "Middle-Low"),
  get_marginals_wave(mp_hm_t, "High-Middle"),
  get_marginals_wave(mp_hl_t, "High-Low")
)

df_pension_lab_t <- df_pension_t %>%
  mutate(
    sig     = ifelse(conf.low * conf.high > 0, "*", ""),  # IC no cruza 0
    est_lab = sprintf("%.2f%s", estimate, sig),
    low_lab = sprintf("%.2f", conf.low),
    high_lab= sprintf("%.2f", conf.high),
    name = factor(name, levels = unique(name)),
    term = factor(term, levels = c("2016", "2018", "2023"))
  )

pos <- position_dodge(width = 0.55)

g5 <- ggplot(
  df_pension_lab_t,
  aes(x = estimate, y = name, group = term, colour = term, shape = term)
) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  fatten = 2.5,
                  size = 1,
                  position = pos) +
  scale_colour_manual(values = c(
    "2016" = "#000004FF",
    "2018"  = "#DE4968FF",
    "2023"  = "#8C2981FF"
  )) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  labs(x = "Marginal Effect", y = NULL, colour = NULL, shape = NULL, linetype = NULL) +
  my_pretty_theme() +
  theme(legend.position = "bottom")



# 3.4 Robustness check ------------------------------------------------


bases <- c("datos1", "datos2", "datos3", "db")

for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    mutate(
      just_pension_i = if_else(just_pension >= 3, 1, 0),
      
    )
  
  assign(nm, df, inherits = TRUE)
}


## (i) 3 to agreement
### Low to middle

data_lm_r_i <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_i, 
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

mp_lm_r_i <-
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm_r_i) 

### Low to high

data_lh_r_i <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_i, 
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

mp_lh_r_i <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh_r_i) 

### middle to high

data_mh_r_i <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_i, 
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

mp_mh_r_i <-  
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh_r_i) 

### middle to low

data_ml_r_i <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_i, 
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

mp_ml_r_i <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml_r_i) 

### High to middle

data_hm_r_i <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_i, 
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

mp_hm_r_i <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm_r_i) 


### High to low

data_hl_r_i <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_i, 
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

mp_hl_r_i <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hl_r_i) 

# Table

texreg::screenreg(list(mp_lm_r_i,
                       mp_lh_r_i,
                       mp_mh_r_i,
                       mp_ml_r_i,
                       mp_hm_r_i,
                       mp_hl_r_i),
                  custom.model.names = c(
                    "Low-Middle",
                    "Low-High",
                    "Middle-High",
                    "Middle-Low",
                    "High-Middle",
                    "High-Low"
                  ))

## (ii) exclude 3

### Low to middle

data_lm_r_ii <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblm, 
         w = w_low_lm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w), y != 3) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")),
         y = case_when(y == 1 ~ 1,
                       y == 2 ~ 2,
                       y == 4 ~ 3,
                       y == 5 ~ 4)) 

mp_lm_r_ii <-
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm_r_ii) 

### Low to high

data_lh_r_ii <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = moblh, 
         w = w_low_lh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w), y != 3) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")),
         y = case_when(y == 1 ~ 1,
                       y == 2 ~ 2,
                       y == 4 ~ 3,
                       y == 5 ~ 4)) 

mp_lh_r_ii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh_r_ii) 

### middle to high

data_mh_r_ii <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobmh, 
         w = w_mid_mh_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w), y != 3) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")),
         y = case_when(y == 1 ~ 1,
                       y == 2 ~ 2,
                       y == 4 ~ 3,
                       y == 5 ~ 4)) 

mp_mh_r_ii <-  
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh_r_ii) 

### middle to low

data_ml_r_ii <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobml, 
         w = w_mid_ml_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w), y != 3) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")),
         y = case_when(y == 1 ~ 1,
                       y == 2 ~ 2,
                       y == 4 ~ 3,
                       y == 5 ~ 4)) 

mp_ml_r_ii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml_r_ii) 

### High to middle

data_hm_r_ii <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhm, 
         w = w_hig_hm_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w), y != 3) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")),
         y = case_when(y == 1 ~ 1,
                       y == 2 ~ 2,
                       y == 4 ~ 3,
                       y == 5 ~ 4)) 

mp_hm_r_ii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm_r_ii) 


### High to low

data_hl_r_ii <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
         t = mobhl, 
         w = w_hig_hl_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w), y != 3) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")),
         y = case_when(y == 1 ~ 1,
                       y == 2 ~ 2,
                       y == 4 ~ 3,
                       y == 5 ~ 4)) 

mp_hl_r_ii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hl_r_ii) 

# Table

texreg::screenreg(list(mp_lm_r_ii,
                       mp_lh_r_ii,
                       mp_mh_r_ii,
                       mp_ml_r_ii,
                       mp_hm_r_ii,
                       mp_hl_r_ii),
                  custom.model.names = c(
                    "Low-Middle",
                    "Low-High",
                    "Middle-High",
                    "Middle-Low",
                    "High-Middle",
                    "High-Low"
                  ))


## (iii) original scale
### Low to middle

data_lm_r_iii <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
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

mp_lm_r_iii <-
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm_r_iii) 

### Low to high

data_lh_r_iii <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
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

mp_lh_r_iii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh_r_iii) 

### middle to high

data_mh_r_iii <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
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

mp_mh_r_iii <-  
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh_r_iii) 

### middle to low

data_ml_r_iii <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
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

mp_ml_r_iii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml_r_iii) 

### High to middle

data_hm_r_iii <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
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

mp_hm_r_iii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm_r_iii) 


### High to low

data_hl_r_iii <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension, 
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

mp_hl_r_iii <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hl_r_iii) 

# Table

texreg::screenreg(list(mp_lm_r_iii,
                       mp_lh_r_iii,
                       mp_mh_r_iii,
                       mp_ml_r_iii,
                       mp_hm_r_iii,
                       mp_hl_r_iii),
                  custom.model.names = c(
                    "Low-Middle",
                    "Low-High",
                    "Middle-High",
                    "Middle-Low",
                    "High-Middle",
                    "High-Low"
                  ))


# 3.5 Sensitivity analysis ------------------------------------------------

# -----------------------------
# E values
# -----------------------------

library(sensemakr)

lm_mh <-  
  lm(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
     weights = w,
     data = data_mh) 


coef_name <- "t"
coef_est  <- mp_mh$coefficients[coef_name]
coef_se   <- mp_mh$std.error[coef_name]
df_CR2    <- mp_mh$df[coef_name]


mh_evalue <- sensemakr(model = lm_mh, 
                       treatment = coef_name,
                       benchmark_covariates = "sexo",
                       robust.se = coef_se,
                       dof = df_CR2,
                       kd = 1:3) 

lm_hm <-  
  lm(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
     weights = w,
     data = data_hm) 


coef_name <- "t"
coef_est  <- mp_hm$coefficients[coef_name]
coef_se   <- mp_hm$std.error[coef_name]
df_CR2    <- mp_hm$df[coef_name]


hm_evalue <- sensemakr(model = lm_hm, 
                       treatment = coef_name,
                       benchmark_covariates = "sexo",
                       robust.se = coef_se,
                       dof = df_CR2,
                       kd = 1:3) 

lm_hl <-  
  lm(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
     weights = w,
     data = data_hl) 


coef_name <- "t"
coef_est  <- mp_hl$coefficients[coef_name]
coef_se   <- mp_hl$std.error[coef_name]
df_CR2    <- mp_hl$df[coef_name]


hl_evalue <- sensemakr(model = lm_hl, 
                       treatment = coef_name,
                       benchmark_covariates = "sexo",
                       robust.se = coef_se,
                       dof = df_CR2,
                       kd = 1:3) 
