
library(ggeffects)
pred_mh <- ggpredict(mp_mh, terms = "t [0,1]", vcov_fun = vcov) |> as.data.frame()
pred_hm <- ggpredict(mp_hm, terms = "t [0,1]", vcov_fun = vcov) |> as.data.frame()
pred_hl <- ggpredict(mp_hl, terms = "t [0,1]", vcov_fun = vcov) |> as.data.frame()

pred_mh$model <- "Middle→High"
pred_hm$model <- "High→Middle"
pred_hl$model <- "High→Low"

pred_sig <- dplyr::bind_rows(pred_mh, pred_hm, pred_hl)
pred_sig


library(marginaleffects)

# Grid: t=0/1 en cada ola (mismo modelo pooled)
nd_mh <- datagrid(model = mp_mh, t = c(0,1), ola = levels(data_mh$ola))
nd_hm <- datagrid(model = mp_hm, t = c(0,1), ola = levels(data_hm$ola))
nd_hl <- datagrid(model = mp_hl, t = c(0,1), ola = levels(data_hl$ola))

# Promedio sobre las 3 olas (igual peso) + IC usando CR2 vcov
pmh <- avg_predictions(mp_mh, variables = "t", vcov = vcov(mp_mh))
phm <- avg_predictions(mp_hm, variables = "t", vcov = vcov(mp_hm))
phl <- avg_predictions(mp_hl, variables = "t", vcov = vcov(mp_hl))

pmh$model <- "Middle→High"
phm$model <- "High→Middle"
phl$model <- "High→Low"

pred_sig_pooled <- dplyr::bind_rows(pmh, phm, phl)
pred_sig_pooled

pred_sig_pooled_plot <- pred_sig_pooled %>% 
  mutate(
    t = factor(t, levels = c(0, 1), labels = c("Inmobile (t=0)", "Mobile (t=1)")),
    model = factor(model, levels = c("Middle→High", "High→Middle", "High→Low"))
  )

ggplot(pred_sig_pooled_plot, aes(x = t, y = estimate)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12) +
  facet_wrap(~ model, nrow = 1) +
  labs(x = NULL, y = "Predicción (Ŷ)", title = "Valores predichos (pooled) por trayectoria") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")