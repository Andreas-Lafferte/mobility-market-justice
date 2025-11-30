### Low to middle

data_lm <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = merit_i, 
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
                      labels = c("2016", "2018", "2023")),
         y = as.numeric(y))

mp_lm <-
  lm_robust(y ~ t + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lm) 

### Low to high

data_lh <- datos1 %>% 
  select(idencuesta, 
         ola, 
         y = merit_i, 
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
                      labels = c("2016", "2018", "2023")),
         y = as.numeric(y))


mp_lh <- 
  lm_robust(y ~ t + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_lh) 

### middle to high

data_mh <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = merit_i, 
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
                      labels = c("2016", "2018", "2023")),
         y = as.numeric(y))


mp_mh <-  
  lm_robust(y ~ t +  ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_mh) 

### middle to low

data_ml <- datos2 %>% 
  select(idencuesta, 
         ola, 
         y = merit_i, 
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
                      labels = c("2016", "2018", "2023")),
         y = as.numeric(y))


mp_ml <- 
  lm_robust(y ~ t + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_ml) 

### High to middle

data_hm <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = merit_i, 
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
                      labels = c("2016", "2018", "2023")),
         y = as.numeric(y))


mp_hm <- 
  lm_robust(y ~ t + ola,
            weights = w,
            se_type = "CR2",
            clusters = idencuesta,
            data = data_hm) 


### High to low

data_hl <- datos3 %>% 
  select(idencuesta, 
         ola, 
         y = merit_i, 
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
                      labels = c("2016", "2018", "2023")),
         y = as.numeric(y))


mp_hl <- 
  lm_robust(y ~ t  + ola,
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

names(db)
table(db$mobility, db$merit_i)
