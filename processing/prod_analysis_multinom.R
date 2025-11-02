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

load(here("input/data/proc/data_v2.3.RData"))

glimpse(db)

# 3. Analysis -----------------------------------------------------------

# Descriptive statistics

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
db %>% 
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

ggplot(pdat, aes(x = mobility, y = y, group = wave)) +
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


# 3.2 Regresiones con IPW -----------------------------------------------------------

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

data_lm %>% 
  arrange(idencuesta)

mp_lm <-
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm) 

mp_lm %>%  
  screenreg()

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
            
mp_lh %>% 
  screenreg()


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
    
mp_mh %>% 
  screenreg()

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
            
mp_ml %>% 
  screenreg()


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
            
mp_hm %>% 
  screenreg()

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
            
mp_hl %>% 
  screenreg()

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
  tidy(mp_ml, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("Middle-Low\n", "(N obs. = ", mp_ml$nobs, ";", "\nN clusters = ", mp_ml$nclusters, ")"))
  ,
  tidy(mp_mh, conf.int = TRUE) %>%
    dplyr::filter(term == "t") %>%
    dplyr::mutate(term = paste0("Middle-High\n", "(N obs. = ", mp_mh$nobs, ";", "\nN clusters = ", mp_mh$nclusters, ")"))
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

ggplot(df_pension_lab, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  #geom_text(aes(label = est_lab), hjust = -0.05, size = 4, vjust = -0.5) +
  labs(x = "Estimate", y = NULL) +
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



# 3.4 Sensitivity analysis ------------------------------------------------

# Holm
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

# Paquetes
library(broom)
library(dplyr)
library(purrr)
library(tibble)
library(knitr)   # para kable (tabla bonita)
# library(gt)    # opcional si prefieres gt

# -----------------------------
# (1) Arma tu lista de modelos
# Reemplaza con tus objetos reales:
mods <- list(
  "Low→Middle"  = mp_lm,
  "Low→High"    = mp_lh,
  "Middle→Low"  = mp_ml,
  "Middle→High" = mp_mh,
  "High→Middle" = mp_hm,
  "High→Low"    = mp_hl
)

# -----------------------------
# (2) Helpers para RR y E-value

rr_from_beta <- function(beta, sdY) {
  exp(0.91 * (beta / sdY))
}

# Ajusta RR a >= 1 (para efectos en valor absoluto)
rr_to_upper <- function(rr) {
  ifelse(rr < 1, 1/rr, rr)
}

evalue_from_rr <- function(rr) {
  # rr ya debe ser >= 1
  ifelse(rr <= 1, 1, rr + sqrt(rr * (rr - 1)))
}

# -----------------------------
# (3) Extrae beta, IC y calcula sdY (por modelo)
#    - sdY ponderada si el modelo tiene weights
#    - si no hay weights, simple
#    - si por alguna razón falla, usa sdY = 0.5 (conservador para Y binaria)

extract_effects_and_sdY <- function(mod, term_name = "t") {
  # Toma el coeficiente y CI del término de tratamiento
  tid <- broom::tidy(mod, conf.int = TRUE)
  eff <- tid %>% filter(term == term_name)
  if (nrow(eff) != 1) stop("No encontré el término de tratamiento '", term_name, "' en el modelo.")
  
  # Recupera Y y weights desde el modelo
  mf <- tryCatch(model.frame(mod), error = function(e) NULL)
  if (!is.null(mf)) {
    y <- tryCatch(model.response(mf), error = function(e) NULL)
    w <- tryCatch(model.weights(mf), error = function(e) NULL)
  } else {
    y <- NULL; w <- NULL
  }
  
  # sdY
  if (!is.null(y)) {
    if (!is.null(w)) {
      p_hat <- sum(w * y, na.rm = TRUE) / sum(w, na.rm = TRUE)
    } else {
      p_hat <- mean(y, na.rm = TRUE)
    }
    sdY <- sqrt(p_hat * (1 - p_hat))
    # En casos extremos (p≈0 o 1), protege:
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

# -----------------------------
# (4) Computa RR y E-values (punto e IC) por modelo

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

# -----------------------------
# (5) Ejecuta todo y arma una tabla final

evalues_tbl <- imap_dfr(mods, function(mod, name) {
  pars <- extract_effects_and_sdY(mod, term_name = "t")
  ev   <- compute_evalues(pars$beta, pars$ci_low, pars$ci_high, pars$sdY)
  bind_cols(
    tibble(traj = name),
    round(bind_cols(pars, ev), 4)
  )
})

# Tabla bonita (kable); puedes usar 'gt(evalues_tbl)' si prefieres
kable(evalues_tbl,
      caption = "E-values for ATT estimates by mobility trajectory (Ding & VanderWeele, 2016 approximation)",
      align = "lrrrrrrrrr")



# 3.4 Interacciones -------------------------------------------------------

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

data_lm_i %>% 
  arrange(idencuesta)

mp_lm_i <-
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm_i) 

mp_lm_i %>%  
  screenreg()

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

mp_lh_i %>% 
  screenreg()


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

mp_mh_i %>% 
  screenreg()

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

mp_ml_i %>% 
  screenreg()


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

mp_hm_i %>% 
  screenreg()

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

mp_hl_i %>% 
  screenreg()


df_pension_i <-  bind_rows(
  tidy(mp_lm_i, conf.int = TRUE) %>%
    filter(term == "t:mHigh") %>%
    mutate(term = paste0("Low→Middle\n", "(N obs. = ", mp_hl_i$nobs, ";", "\nN clusters = ", mp_hl_i$nclusters, ")")
    ) 
  ,
  tidy(mp_lh_i, conf.int = TRUE) %>%
    filter(term == "t:mHigh") %>%
    mutate(term = paste0("Low→High\n", "(N obs. = ", mp_lh_i$nobs, ";", "\nN clusters = ", mp_lh_i$nclusters, ")"))
  ,
  tidy(mp_mh_i, conf.int = TRUE) %>%
    filter(term == "t:mHigh") %>%
    mutate(term = paste0("Middle→High\n", "(N obs. = ", mp_mh_i$nobs, ";", "\nN clusters = ", mp_mh_i$nclusters, ")"))
  ,
  tidy(mp_ml_i, conf.int = TRUE) %>%
    filter(term == "t:mHigh") %>%
    mutate(term = paste0("Middle→Low\n", "(N obs. = ", mp_ml_i$nobs, ";", "\nN clusters = ", mp_ml_i$nclusters, ")"))
  ,
  tidy(mp_hm_i, conf.int = TRUE) %>%
    filter(term == "t:mHigh") %>%
    mutate(term = paste0("High→Middle\n", "(N obs. = ", mp_hm_i$nobs, ";", "\nN clusters = ", mp_hm_i$nclusters, ")"))
  ,
  tidy(mp_hl_i, conf.int = TRUE) %>%
    filter(term == "t:mHigh") %>%
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
      levels = term
    )
  )


ggplot(df_pension_lab, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
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




# Tablas ------------------------------------------------------------------




screenreg(list(mp_lm,
               mp_lh,
               mp_ml,
               mp_mh,
               mp_hm,
               mp_hl),
          custom.model.names = c(
            "Low-Middle",
            "Low-High",
            "Middle-Low",
            "Middle-High",
            "High-Middle",
            "High-Low"
          ))



screenreg(list(mp_lm_i,
               mp_lh_i,
               mp_ml_i,
               mp_mh_i,
               mp_hm_i,
               mp_hl_i),
          custom.model.names = c(
            "Low-Middle",
            "Low-High",
            "Middle-Low",
            "Middle-High",
            "High-Middle",
            "High-Low"
          ))

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


library(broom)
library(ggplot2)
library(dplyr)

coef_df <- tidy(mp_hm_t, conf.int = TRUE) %>%
  filter(grepl("t:ola", term))

ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(labels = c("t:factor(ola)2" = "Wave 2 (2018)",
                              "t:factor(ola)3" = "Wave 3 (2023)")) +
  labs(x = "Interaction term", y = "Coefficient (Δ effect vs. 2016)",
       title = "Interaction coefficients: change in effect of mobility by wave") +
  theme_ggdist()

