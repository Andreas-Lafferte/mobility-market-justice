# Libraries
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               sjmisc, 
               sjPlot, 
               lme4, 
               here, 
               performance,
               texreg, 
               shadowtext,
               MetBrewer,
               sjlabelled,
               plm)


options(scipen=999)
rm(list = ls())

# Data

load(file = here("input/data/proc/df_study1_long_t7.RData"))

# Generate analytical sample

df_study1 <- df_study1_long_t7 %>%
  select(idencuesta,
         ola,
         ponderador_long_total,
         segmento,
         estrato,
         just_health,
         just_pension,
         just_educ,
         mjp, 
         mobility,
         mobility_f,
         merit_effort, 
         merit_talent, 
         educ,
         quintil1,
         sex,
         age,
         ideo) %>% 
  na.omit() %>% 
  mutate(ola = case_when(ola == 1 ~ 1,
                         ola == 2 ~ 2, 
                         ola == 3 ~ 3,
                         ola == 4 ~ 4,
                         ola == 6 ~ 5,
                         ola == 7 ~ 6)) %>% 
  mutate(ola = as.factor(ola),
         ola_num = as.numeric(ola),
         ola_2=as.numeric(ola)^2)

df_study1 <- df_study1 %>%
  group_by(idencuesta) %>%             # Agrupar por el identificador del participante
  mutate(n_participaciones = n()) %>%  # Contar el número de filas (participaciones) por participante
  ungroup()

df_study1 <- df_study1 %>% filter(n_participaciones>1)


# Analysis

df_study1$just_health <- as_numeric(df_study1$just_health)
df_study1$just_pension <- as_numeric(df_study1$just_pension)
df_study1$just_educ <- as_numeric(df_study1$just_educ)


frq(df_study1$mobility_f)

plm(formula = mjp ~  mobility_f*merit_effort, 
    data = df_study1,
    model = "within",
    effect = "twoways") %>% summary()


df_study1$merit_effort <- as_numeric(df_study1$merit_effort)
df_study1$merit_talent <- as_numeric(df_study1$merit_talent)

df_study1 <- df_study1 %>% 
  mutate(ola = case_when(ola == 1 ~ "2016",
                         ola == 2 ~ "2017",
                         ola == 3 ~ "2018",
                         ola == 4 ~ "2019",
                         ola == 5 ~ "2022",
                         ola == 6 ~ "2023"),
         ola = factor(ola, levels = c("2016",
                                      "2017",
                                      "2018",
                                      "2019",
                                      "2022",
                                      "2023")))


df_study1 <- df_study1 %>% 
  group_by(idencuesta) %>% 
  mutate(perc_inequality_mean = mean(perc_inequality, na.rm = T),
         perc_inequality_cwc = perc_inequality - perc_inequality_mean,
         merit_effort_mean = mean(merit_effort, na.rm = T),
         merit_effort_cwc = merit_effort - merit_effort_mean,
         merit_talent_mean = mean(merit_talent, na.rm = T),
         merit_talent_cwc = merit_talent - merit_talent_mean,
  ) %>% 
  ungroup()


