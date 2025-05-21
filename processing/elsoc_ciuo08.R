# 0. Identification ---------------------------------------------------

# Title: Data preparation for CIUO08 ELSOC
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
               httr,
               occupar)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

glimpse(elsoc_long_2016_2023)

# 3. Processing -----------------------------------------------------------

elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

# 3.1 Glosa INE ----

elsoc_long_2016_2023 %>% 
  group_by(ola) %>% 
  tally(!is.na(m57))

df_glosa <- elsoc_long_2016_2023 %>% 
  filter(ola == 6)

request <-  httr::POST("https://rapps.ine.cl:9292/predict", 
                       encode = "json",
                       body =  list(text = df_glosa$m57,
                                    classification = "ciuo",
                                    digits = 2))

httr::status_code(request)

ciuo08 <- httr::content(request, encoding = "text")

ciuo08_predicciones <- data.frame(
  cod_final = unlist(ciuo08$cod_final),
  prob = as.numeric(unlist(ciuo08$prob))
)

ciuo08_predicciones <- ciuo08_predicciones %>% 
  mutate(
    isco08_ine = (as.numeric(cod_final)*100),
    isei_08 = occupar::isco08toISEI08(isco08_ine),
    isei_92 = occupar::isco08toISEI92(isco08_ine)
  )

df <- bind_cols(df_glosa, ciuo08_predicciones) %>% 
  rename(isco08_orig = isco08_ine,
         isei08_orig = isei_08,
         isei92_orig = isei_92)

save(df, file = here("input/data/elsoc_ciuo08.RData"))