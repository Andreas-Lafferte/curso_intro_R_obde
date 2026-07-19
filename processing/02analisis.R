# 0. Identification ---------------------------------------------------

# Title: Preparación de datos — Taller de introducción a R (Encuesta ICSOH-UDP)
# Institution: ICSOH / OBDE, Universidad Diego Portales
# Responsible: Andreas Laffert Tamayo (andreas.laffert@uchile.cl)

# Executive Summary: Este script contiene el código de análisis de datos para el taller de introducción a R (Encuesta ICSOH-UDP)
# Date: 2026-07-18


# 1. Packages ----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, # manipular datos
               sjmisc,    # resumen de datos
               sjPlot, # resumen de datos
               psych  # resumen de datos
              )

options(scipen = 999) # desactivar notacion cientifica
rm(list = ls())       # limpiar el enviroment

# 2. Data ----------------------------------------------------------------

load("output/data/datos_proc.RData") # datos procesados en paso 1

glimpse(datos_proc)


# datos_proc trae NA a propósito. Antes de analizar,
# aplicamos listwise deletion: nos quedamos solo con los casos completos
# en las variables que vamos a usar hoy. 

datos_analisis <- datos_proc |> 
  na.omit()

nrow(datos_proc)      # casos originales
nrow(datos_analisis)  # casos tras listwise deletion

# 3. Analysis --------------------------------------------------------

## 3.1 Descriptivo ----

# --- Variables categóricas ---

# sexo
sjmisc::frq(datos_analisis$sexo)
sjPlot::plot_frq(datos_analisis$sexo) # histograma simple

# edad
sjmisc::frq(datos_analisis$tramo_edad)

# ideologia
sjmisc::frq(datos_analisis$ideologia)
sjPlot::plot_frq(datos_analisis$ideologia) # histograma simple

# ingresos
sjmisc::frq(datos_analisis$quintil_ingreso)
sjPlot::plot_frq(datos_analisis$quintil_ingreso) # histograma simple

# --- Variables numéricas ---

# fichas_seguridad viene de una pregunta donde cada persona reparte 10
# fichas de gasto público entre distintas áreas.

summary(datos_analisis$fichas_seguridad) # resumen simple

psych::describe(datos_analisis$fichas_seguridad,
                quant = c(.25,.75),
                IQR = T) # resumen completo

datos_analisis %>% 
  summarise(media = mean(fichas_seguridad),
            mediana = median(fichas_seguridad),
            q1 = quantile(fichas_seguridad, probs = .25),
            q2 = quantile(fichas_seguridad, probs = .75),
            rango = max(fichas_seguridad) - min(fichas_seguridad),
            desviacion_estandar = sd(fichas_seguridad),
            varianza = var(fichas_seguridad)) # con dplyr

hist(datos_analisis$fichas_seguridad) # histograma simple

## 3.2 Bivariado ----

# --- categórica x categórica ---
# Tabla de contingencia con porcentajes de fila: quintil de ingreso por posición política.

sjPlot::tab_xtab(datos_analisis$quintil_ingreso, # fila
                 datos_analisis$ideologia, # columna
                 show.row.prc = TRUE) # porcentaje fila (podemos ponerle porcentaje columna también)

# ---  numérica x categórica ---
# ¿Varían las fichas de seguridad según el sexo? Comparamos los promedios
# y confirmamos la diferencia con una prueba t.

datos_analisis |>
  group_by(sexo) |> # agrupamos por sexo
  summarize(fichas_seguridad_prom = mean(fichas_seguridad, na.rm = TRUE), # calcular la media por grupo
            n = n()) # n casos por grupo

t.test(fichas_seguridad ~ sexo, data = datos_analisis) # prueba T de diferencia de medias

# ---  numérica x categórica ---
# ¿Varían las fichas de seguridad según la posición política? Comparamos los promedios
# y confirmamos la diferencia con ANOVA

datos_analisis |>
  group_by(ideologia) |> # agrupamos por sexo
  summarize(fichas_seguridad_prom = mean(fichas_seguridad, na.rm = TRUE), # calcular la media por grupo
            n = n()) # n casos por grupo

prueba_anova <- aov(fichas_seguridad ~ ideologia, data = datos_analisis) # ejecutamos y guardamos

summary(prueba_anova) # vemos su resumen

# Boxplot: fichas de seguridad según posición política.
ggplot(datos_analisis, aes(x = ideologia, y = fichas_seguridad)) +
  geom_boxplot() +
  labs(title = "Fichas para seguridad según posición política",
       x = "Posición política", y = "Fichas asignadas a seguridad")

# ---  numérica x numérica ---
# ¿Se relaciona el ingreso del hogar con las fichas asignadas a pensiones?

cor.test(datos_analisis$decil_ingreso, 
         datos_analisis$fichas_pensiones, 
         method = "pearson") # correlacion de pearson

# Scatter plot: ingreso del hogar y fichas asignadas a pensiones.
ggplot(datos_analisis, aes(x = decil_ingreso, y = fichas_pensiones)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Ingreso del hogar y fichas asignadas a pensiones",
       x = "Ingreso del hogar",
       y = "Fichas asignadas a pensiones")

# 5. Save results (opcional) -----------------------------------------

# saveRDS(datos_analisis, "../output/data/datos_analisis.rds")
# readr::write_csv(datos_analisis, "../output/data/datos_analisis.csv")
