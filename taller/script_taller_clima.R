# Taller de introducción a R con la Encuesta ICSOH-UDP
# Script de práctica: cómo varía la prioridad de gasto en seguridad según
# el sexo y la posición política de las personas, y cómo se relaciona el
# ingreso del hogar con la prioridad de gasto en pensiones.
# Ejecuta cada sección de arriba hacia abajo, con control/cmd + enter.


# 1. Paquetes ----

# pacman instala (si falta) y carga los paquetes en un solo paso. Piensa en
# cada paquete como un cajón de cocina: cada uno guarda herramientas
# específicas para una tarea distinta.
pacman::p_load(dplyr, ggplot2, sjlabelled, sjmisc, naniar, sjPlot)


# 2. Cargar datos ----

# La base viene en formato .RData, así que se carga con load().
# El objeto llega con el nombre que traía guardado: bbdd_clima_sample.
load("../input/data/BBDD_Clima_Junio_Sample.RData")
ls()

# Las bases de encuesta suelen llegar etiquetadas (clase haven_labelled):
# cada número representa una categoría. Convertimos esas etiquetas en
# factores de una vez, así trabajamos con texto legible desde el inicio.
datos <- sjlabelled::as_label(bbdd_clima_sample)


# 3. Observar los datos ----

# Un primer vistazo: cuántas filas (personas) y columnas (variables) tenemos.
dim(datos)

# Los nombres de las primeras variables.
names(datos) |> head(15)

# glimpse() muestra el tipo de cada columna y sus primeros valores.
# Aquí miramos solo un subconjunto para que la salida no sea gigante.
datos |>
  select(sexo, tramo_edad, gse, ideologia, ingreso_hogar, fichas_seguridad) |>
  glimpse()

# get_label() muestra la pregunta original de una variable, y frq() sus
# frecuencias, ya con las categorías como texto (no como números).
sjlabelled::get_label(datos$ingreso_hogar)
sjmisc::frq(datos$ideologia)


# 4. Valores perdidos ----

# naniar nos deja diagnosticar rápido cuántos datos perdidos hay y dónde.
colSums(is.na(datos)) |> head(6)
naniar::n_miss(datos)
naniar::prop_miss(datos) * 100
naniar::miss_var_summary(datos) |> head(5)

# na.omit() elimina toda fila con al menos un NA en cualquier columna, así
# que en una base tan ancha como esta conviene aplicarlo solo sobre las
# variables que efectivamente vamos a usar en el análisis de hoy.
datos_analisis <- datos |>
  select(sexo, ideologia, gse, ingreso_hogar, fichas_seguridad, fichas_pensiones) |>
  na.omit()

nrow(datos)
nrow(datos_analisis)


# 5. Recodificar ----

# as.integer() sobre un factor devuelve la posición de cada categoría. En
# ingreso_hogar esa posición ya sigue el orden de los tramos de ingreso, así
# que la usamos para construir una versión numérica de 0 a 10, donde el
# tramo 11 ("no hubo ingresos en el hogar") pasa a valer 0.
datos_analisis <- datos_analisis |>
  mutate(ingreso_decil = if_else(
    as.integer(ingreso_hogar) == 11, 0L, as.integer(ingreso_hogar)))

datos_analisis |> count(ingreso_decil)


# 6. Descriptivos: variables categóricas ----

# ¿Cómo se distribuye el grupo socioeconómico (GSE) en la muestra?
sjmisc::frq(datos_analisis$gse)

# ¿Y la posición política?
sjmisc::frq(datos_analisis$ideologia)


# 7. Descriptivos: variables numéricas ----

# fichas_seguridad viene de una pregunta donde cada persona reparte 10
# fichas de gasto público entre distintas áreas. Veamos cuánto se prioriza
# seguridad en general.
summary(datos_analisis$fichas_seguridad)
sd(datos_analisis$fichas_seguridad, na.rm = TRUE)


# 8. Bivariado categórica x categórica ----

# Tabla de contingencia con porcentajes de fila: GSE por posición política.
sjPlot::tab_xtab(datos_analisis$gse, datos_analisis$ideologia, show.row.prc = TRUE)


# 9. Bivariado numérica x categórica ----

# ¿Varían las fichas de seguridad según el sexo? Comparamos los promedios
# y confirmamos la diferencia con una prueba t.
datos_analisis |>
  group_by(sexo) |>
  summarize(fichas_seguridad_prom = mean(fichas_seguridad, na.rm = TRUE), n = n())

t.test(fichas_seguridad ~ sexo, data = datos_analisis)


# 10. Bivariado numérica x numérica ----

# ¿Se relaciona el ingreso del hogar con las fichas asignadas a pensiones?
cor.test(datos_analisis$ingreso_decil, datos_analisis$fichas_pensiones)


# 11. Gráficos ----

# Barras: distribución del grupo socioeconómico.
ggplot(datos_analisis, aes(x = gse)) +
  geom_bar() +
  labs(title = "Distribución por grupo socioeconómico", x = "GSE", y = "Casos")

# Densidad: cómo se reparten las fichas de gasto en seguridad.
ggplot(datos_analisis, aes(x = fichas_seguridad)) +
  geom_density(fill = "grey80") +
  labs(title = "Distribución de fichas asignadas a seguridad",
       x = "Fichas (de 10)", y = "Densidad")

# Boxplot: fichas de seguridad según posición política.
ggplot(datos_analisis, aes(x = ideologia, y = fichas_seguridad)) +
  geom_boxplot() +
  labs(title = "Fichas para seguridad según posición política",
       x = "Posición política", y = "Fichas asignadas a seguridad")

# Scatter: ingreso del hogar y fichas asignadas a pensiones.
ggplot(datos_analisis, aes(x = ingreso_decil, y = fichas_pensiones)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Ingreso del hogar y fichas asignadas a pensiones",
       x = "Ingreso del hogar (0 = sin ingresos, 10 = tramo más alto)",
       y = "Fichas asignadas a pensiones")


# 12. Guardar resultados (opcional) ----

# Así se guardaría la base ya procesada, para retomarla después.
# saveRDS(datos_analisis, "datos_analisis.rds")
# readr::write_csv(datos_analisis, "datos_analisis.csv")
