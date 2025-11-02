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

load(here("input/data/proc/data_v3.RData"))

glimpse(db)

# 3. Analysis -----------------------------------------------------------

bases <- c("datos1", "datos2", "datos3", "db")

for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    mutate(
      just_pension_f = if_else(just_pension >= 4, 1, 0),
      merit_i = if_else(merit_i >= 4, 1, 0),
      merit_i = factor(merit_i, levels = c(0,1), labels = c("Baja", "Alta"))
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

bases <- c("datos1", "datos2", "datos3")

for (nm in bases) {
  df <- get(nm)
  
  df <- df %>%
    mutate(ola = case_when(ola == 1 ~ 1,
                           ola == 3 ~ 2,
                           ola == 7 ~ 3,
                           TRUE ~ NA_real_)
    )
  
  assign(nm, df, inherits = TRUE)
}

t1 <- db %>% 
  select(idencuesta, 
         wave, 
         just_pension, 
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

### Pensiones 

### Downward

data_downward <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mob_down, 
         w = w_dw_att_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

data_downward %>% 
  arrange(idencuesta)

mp_downward <-  
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0 + factor(ola),
            weights = w, 
            se_type = "CR2",
            clusters = idencuesta,
            data = data_downward) 

mp_downward %>%  
  screenreg()


### Inmobile

data_inmobile <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mob_inmobile, 
         w = w_inm_att_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_inmobile <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_inmobile) 

mp_inmobile %>% 
  screenreg()


### Upward

data_upward <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mob_upward, 
         w = w_up_att_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_upward <- 
  lm_robust(y ~ t + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_upward) 

mp_upward %>% 
  screenreg()

conflicts_prefer(dplyr::filter)

df_pension <- bind_rows(
  tidy(mp_downward, conf.int = TRUE) %>%
    filter(term == "t") %>%
    mutate(term = paste0("Downward\n", "(N obs. = ", mp_downward$nobs, ";", "\nN clusters = ", mp_downward$nclusters, ")")
    ) 
  ,
  tidy(mp_inmobile, conf.int = TRUE) %>%
    filter(term == "t") %>%
    mutate(term = paste0("Inmobile\n", "(N obs. = ", mp_inmobile$nobs, ";", "\nN clusters = ", mp_inmobile$nclusters, ")")
    ) 
  ,
  tidy(mp_upward, conf.int = TRUE) %>%
    filter(term == "t") %>%
    mutate(term = paste0("Upward\n", "(N obs. = ", mp_upward$nobs, ";", "\nN clusters = ", mp_upward$nclusters, ")"))
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
        "Downward\n(N obs. = 2286;\nN clusters = 1296)",
        "Inmobile\n(N obs. = 3156;\nN clusters = 1648)",
        "Upward\n(N obs. = 2210;\nN clusters = 1242)"
      )
    )
  )

g2 <- ggplot(df_pension_lab, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", size = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 1.2) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  geom_text(aes(label = est_lab), hjust = -0.2, size = 5, vjust = 1.5) +
  labs(x = "Estimate (β)", y = NULL) +
  theme_ggdist() 


# 3.4 Interacciones -------------------------------------------------------


### Interacciones
### Downward

data_downward_i <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mob_down, 
         w = w_dw_att_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_downward_i <-  
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + factor(ola),
            weights = w, 
            se_type = "CR2",
            clusters = idencuesta,
            data = data_downward_i) 

mp_downward_i %>%  
  screenreg()

### Inmobile

data_inmobile_i <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mob_inmobile, 
         w = w_inm_att_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_inmobile_i <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_inmobile_i) 

mp_inmobile_i %>% 
  screenreg()

### Upward

data_upward_i <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = just_pension_f, 
         t = mob_upward, 
         w = w_up_att_t, 
         edad = m0_edad_f, 
         sexo = sexo_f, 
         nac = nacionalidad_f,
         etnia = etnia_f,
         hogar = hogar_bip_f, 
         edu0 = educ_orig_f,
         m = merit_i) %>% 
  filter(!is.na(t), !is.na(w)) 

mp_upward_i <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ factor(ola),
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_upward_i) 

mp_upward_i %>% 
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
