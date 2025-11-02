
# tabla cruzada con totales, % fila y % columna


tab_mob <- db %>%
  count(estrato_orig, estrato_ocupa) %>%
  group_by(estrato_orig) %>%
  mutate(row_total = sum(n),
         row_pct = 100 * n / row_total) %>%
  ungroup() %>%
  group_by(estrato_ocupa) %>%
  mutate(col_total = sum(n),
         col_pct = 100 * n / col_total) %>%
  ungroup()

# totales por fila y por columna
row_totals <- tab_mob %>%
  group_by(estrato_orig) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(estrato_ocupa = "Total")

col_totals <- tab_mob %>%
  group_by(estrato_ocupa) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(estrato_orig = "Total")

grand_total <- tibble(estrato_orig = "Total", estrato_ocupa = "Total",
                      n = sum(tab_mob$n))

tab_mob <- tab_mob %>% 
  select(-c(row_total, col_total, col_pct)) %>% 
  pivot_longer(cols = c(n, row_pct),
               names_to = "name",
               values_to = "value") %>% 
  pivot_wider(
    id_cols   = c(estrato_orig, name),   
    names_from  = estrato_ocupa,         
    values_from = value,
    values_fill = 0
  ) 

tab_mob <- dplyr::add_row(tab_mob, 
                          estrato_orig = "Total",
                          name = "n",
                          Low = as.numeric(col_totals[1,2]),
                          Middle = as.numeric(col_totals[2,2]),
                          High = as.numeric(col_totals[3,2]),
)


tab_mob <- tab_mob %>% 
  rowwise() %>% 
  mutate(total = sum(Low, Middle, High)) %>% 
  ungroup()

tab_mob <- add_row(tab_mob, 
                   estrato_orig = "Total",
                   name = "row_pct",
                   Low = round((tab_mob$Low[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                   Middle = round((tab_mob$Middle[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                   High = round((tab_mob$High[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                   total = round((tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1))

tab_mob <- add_column(tab_mob,
                      "Col" = c(
                        round((tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Low"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                        NA,
                        round((tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Middle"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                        NA,
                        round((tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="High"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                        NA,
                        round((tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"]/tab_mob$total[tab_mob$name=="n"&tab_mob$estrato_orig=="Total"])*100,1),
                        NA
                      ))

#tab_mob$estrato_orig <- if_else(tab_mob$name == "row_pct", "Row %", tab_mob$estrato_orig)

tab_mob <- tab_mob %>% 
  mutate_at(.vars = 3:7, .funs = ~ round(.,1)) %>% 
  mutate_at(.vars = 3:7, .funs = ~ as.character(.)) %>% 
  mutate_at(.vars = 3:6, .funs = ~ if_else(name == "n", big_mark(., ","), .)) %>% 
  rename(Offspring = name) %>% 
  mutate(Offspring = if_else(Offspring == "n", "N", "Row %"))

tab_mob %>% 
  kableExtra::kable(format = "latex", 
                    booktabs= T,
                    align = 'c', 
                    col.names = c("Father↓", "Offspring→", "Low", "Middle", "High", "Total", "Col.%"),
                    caption = NULL) %>%
  kableExtra::kable_styling(latex_options = "hold_position", bootstrap_options = "striped", full_width = T)  %>%
  kableExtra::collapse_rows(columns = 1:2, valign = "top")
