library(dagitty)
library(ggdag)
library(ggplot2)

# Coordenadas simples
coords <- list(
  x = c(Mobility=0, Meritocracy=1.5, Preferences=3),
  y = c(Mobility=1.5, Meritocracy=0.5, Preferences=1.5)
)

# DAG teórico simplificado
dag_mjp_simple <- dagitty("
dag {
  Preferences [pos=\"3,1.5\"]
  Mobility [pos=\"0,1.5\"]
  Meritocracy [pos=\"1.5,0.5\"]

  Preferences <- Mobility
  Preferences <- Meritocracy
  Meritocracy <- Mobility
}
")

coordinates(dag_mjp_simple) <- coords

# Graficar
ggdag(dag_mjp_simple, text = FALSE, node_size = 18) +
  geom_dag_point(aes(color = name), show.legend = FALSE) +
  geom_dag_text(color="white", size=4) +
  theme_dag_blank() +
  labs(title = "DAG simplificado: Movilidad social, Meritocracia y Preferencias por comodificación")



library(dagitty) # Instalar V8
library(ggdag)

coords <- list(
  x = c(Y=2, T=0, U=-1, F=1, X=0, T1=-1, T2=0, Z=1.2),
  y = c(Y=1, T=1, U=1, F=2, X=0, T1=-1, T2=-1, Z=1.2)
)

HPTbestcase <- dagify(
  Y ~ T + F + X + Z,
  F ~ U,
  Z ~ T + F,
  T ~ U + X,
  exposure = "T",
  outcome = "Y",
  coords = coords
)

tidy_dag <- tidy_dagitty(HPTbestcase)

tidy_dag$data$time <- c("t3", "t3", "t2", "t2", "t1", "t1", "t2", "t2", "t3", "t4")

ggdag(tidy_dag, node_size=8, text_size = 4) +
  theme_classic() + remove_axes() + theme(axis.line=element_blank()) +
  ylim(-1.5, 2.5) + xlim(-1.5,2.5) +
  geom_text(x=-1, y=-.5, label=expression(t[1]), size=3) +
  geom_text(x=0, y=-.5, label=expression(t[2]), size=3) +
  geom_text(x=1, y=-.5, label=expression(t[3]), size=3) +
  geom_text(x=2, y=-.5, label=expression(t[4]), size=3) +
  geom_text(x=-1.5, y=-1, label="Y = Outcome (Tolerance),
T = Treatment (ln(Distance to camps))", hjust=0) +
  geom_text(x=-1.5, y=-1.4, label="F = Fixed Effects,
Z = Post-Treatment Confounders", hjust=0) +
  geom_text(x=1, y=-1, label="X = Pre-Treatment Confounders,
U = Unobserved Pre-Treatment Confounders", hjust=0)


library(dagitty)   # si no lo tienes: install.packages("dagitty")
library(ggdag)
library(ggplot2)

# Coordenadas para ubicar los nodos en línea
coords <- list(
  x = c(T=0, M=1.2, Y=2.4),
  y = c(T=1, M=0,   Y=1)
)

# Definición del DAG simplificado
DAG_mjp <- dagify(
  Y ~ T + M,   # Outcome depende de movilidad (directo, H1) y de meritocracia (mediación H2)
  M ~ T,       # Meritocracia depende de movilidad
  exposure = "T",
  outcome  = "Y",
  coords   = coords
)

# Pasar a formato tidy
tidy_dag <- tidy_dagitty(DAG_mjp)

# Asignar tiempos para mostrar secuencia (opcional)
tidy_dag$data$time <- c("t1")

# Graficar
ggdag(tidy_dag, node_size=12, text_size=4) +
  theme_classic() + remove_axes() + theme(axis.line=element_blank()) +
  ylim(-1.5, 1.5) + xlim(-0.5,3) +
  geom_text(x=-0.2, y=-0.5, label="T = Movilidad social\n(upward / downward)", hjust=0) +
  geom_text(x=-0.2, y=-0.9, label="M = Creencias\nmeritocráticas", hjust=0) +
  geom_text(x=-0.2, y=-1.3, label="Y = Preferencias por\ncomodificación", hjust=0) 
