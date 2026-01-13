# -----------
# Meritocracy 
# -----------

### Interacciones

### Low to middle

data_lm_m <- datos1 %>% 
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
         m = merit_m) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_lm_m <-
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm_m) 

### Low to high

data_lh_m <- datos1 %>% 
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
         m = merit_m) %>%  
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_lh_m <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh_m) 

### middle to high

data_mh_m <- datos2 %>% 
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
         m = merit_m) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_mh_m <-  
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh_m) 

### middle to low

data_ml_m <- datos2 %>% 
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
         m = merit_m) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_ml_m <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml_m) 

### High to middle

data_hm_m <- datos3 %>% 
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
         m = merit_m) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_hm_m <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0+ ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm_m) 

### High to low

data_hl_m <- datos3 %>% 
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
         m = merit_m) %>% 
  filter(!is.na(t), !is.na(w)) %>% 
  mutate(ola = factor(ola, levels = c("1", "2", "3"),
                      labels = c("2016", "2018", "2023")))

mp_hl_m <- 
  lm_robust(y ~ t*m + edad + sexo + nac + etnia + hogar + edu0 + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hl_m) 

# Table

screenreg(list(mp_lm_m,
               mp_lh_m,
               mp_mh_m,
               mp_ml_m,
               mp_hm_m,
               mp_hl_m),
          custom.model.names = c(
            "Low-Middle",
            "Low-High",
            "Middle-High",
            "Middle-Low",
            "High-Middle",
            "High-Low"
          ))


df_pension_m <- bind_rows(
  get_marginals_merit(mp_lm_m, "Low-Middle"),
  get_marginals_merit(mp_lh_m, "Low-High"),
  get_marginals_merit(mp_mh_m, "Middle-High"),
  get_marginals_merit(mp_ml_m, "Middle-Low"),
  get_marginals_merit(mp_hm_m, "High-Middle"),
  get_marginals_merit(mp_hl_m, "High-Low")
)

df_pension_lab_m <- df_pension_m %>%
  mutate(
    sig     = ifelse(conf.low * conf.high > 0, "*", ""),  # IC no cruza 0
    est_lab = sprintf("%.2f%s", estimate, sig),
    low_lab = sprintf("%.2f", conf.low),
    high_lab= sprintf("%.2f", conf.high),
    name = factor(name, levels = unique(name)),
    term = factor(term, levels = c("High Meritocracy", "Low Meritocracy"))
  )

pos <- position_dodge(width = 0.55)

ggplot(
  df_pension_lab_m,
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
