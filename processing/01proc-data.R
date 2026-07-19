# 0. Identification ---------------------------------------------------

# Title: Preparación de datos — Taller de introducción a R (Encuesta ICSOH-UDP)
# Institution: ICSOH / OBDE, Universidad Diego Portales
# Responsible: Andreas Laffert Tamayo (andreas.laffert@uchile.cl)

# Executive Summary: Este script contiene el código de preparación de datos para el taller de introducción a R (Encuesta ICSOH-UDP)
# Date: 2026-07-18


# 1. Packages ----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, # manipular datos
               sjmisc,    # resumen de datos
               sjlabelled # etiquetas
              )

options(scipen = 999) # desactivar notacion cientifica
rm(list = ls())       # limpiar el enviroment

# 2. Data ----------------------------------------------------------------

load("input/data/BBDD_Clima_Junio.RData") # cargar datos con ruta

dim(bbdd_clima)      # dimensiones
glimpse(bbdd_clima)  # estructura
View(bbdd_clima) # ver base completa

# 3. Processing ------------------------------------------------------

## 3.1 Select ----

# La base cruda trae nombres de variable técnicos (idp22, idp67, etc.). Con select() elegimos solo las
# variables que vamos a usar en el taller, y con el mismo select() las
# renombramos: a la izquierda el nombre nuevo, a la derecha el original.


datos_proc <- bbdd_clima |>
  dplyr::select(
    id,
    sexo                   = resp_gender,
    tramo_edad             = quotagerange,
    region                 = cl02state,
    autoubicacion_izq_der  = idp67,
    ingreso_hogar          = idp62,
    fichas_pensiones       = idp46l395,
    fichas_seguridad       = idp46l396,
    eval_gobierno_kast     = idp36
    )

glimpse(datos_proc)

## 3.2 Filter ----

# filter() conserva solo las filas que cumplen una condición. Para este caso,
# vamos a quedarnos con todos los casos menos aquellos que son de la región de Aysen (solo ejemplo)

frq(datos_proc$region) # frecuencias variable region

datos_proc <- datos_proc |>
  filter(region != 11) # nos quedamos con todo menos aquello que es 11 y NA

datos_proc |> 
  head()

## 3.3 Recode and transform ----

# Cada vez que recodificamos una variable, hacemos tres pasos recomendados: 1) frq()
# para ver la variable cruda, 2) mutate() para transformarla, y 3) otro
# frq() para revisar que la variable nueva quedó como esperábamos.

# --- sexo ----

frq(datos_proc$sexo) 

datos_proc <- datos_proc |> 
  mutate(sexo = if_else(sexo == 1, "Hombre", "Mujer"), # transformamos a caracter
         sexo = factor(sexo, levels = c("Hombre", "Mujer"))) # convertimos a factor ordenado

frq(datos_proc$sexo)

# --- tramo edad ----

frq(datos_proc$tramo_edad)

datos_proc <- datos_proc |> 
  mutate(tramo_edad = case_when( # transformamos a caracter
      tramo_edad == 1 ~ "18-29",
      tramo_edad == 2 ~ "30-49",
      tramo_edad == 3 ~ "50+"
    ),
    tramo_edad = factor(tramo_edad,
                         levels = c("18-29", "30-49", "50+"),
                         ordered = TRUE)) # convertimos a factor ordenado

frq(datos_proc$tramo_edad)

# --- ideologia, a partir de la escala cruda de 0 a 10 ---

frq(datos_proc$autoubicacion_izq_der)

datos_proc <- datos_proc |>
  mutate(ideologia = case_when(
      autoubicacion_izq_der >= 1 & autoubicacion_izq_der <= 5  ~ "Izquierda", # 0 a 5 en la escala original
      autoubicacion_izq_der == 6  ~ "Centro",    # el punto medio, 6
      autoubicacion_izq_der >= 7 & autoubicacion_izq_der <= 11 ~ "Derecha",   # 7 a 11 en la escala original
      autoubicacion_izq_der == 12 ~ "Ninguno"    # "sin identificación política"
    ),
    ideologia = factor(ideologia,
                       levels = c("Izquierda", "Centro", "Derecha", "Ninguno"),
                       ordered = TRUE))

frq(datos_proc$ideologia)

# --- quintil_ingreso, a partir de los 11 tramos de ingreso del hogar ---
frq(datos_proc$ingreso_hogar)

datos_proc <- datos_proc |>
  mutate(quintil_ingreso = case_when(
      ingreso_hogar == 11 ~ "Q1", # "no hubo ingresos" cae en el tramo más bajo
      ingreso_hogar %in% c(1,2)   ~ "Q1",
      ingreso_hogar %in% c(3,4)   ~ "Q2",
      ingreso_hogar %in% c(5,6)   ~ "Q3",
      ingreso_hogar %in% c(7,8)   ~ "Q4",
      ingreso_hogar %in% c(9,10)  ~ "Q5"
    ),
    quintil_ingreso = factor(quintil_ingreso,
                       levels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                       ordered = TRUE))

frq(datos_proc$quintil_ingreso)

# --- ingreso_decil_ingresodecil, versión numérica de ingreso_hogar ---
frq(datos_proc$ingreso_hogar)

datos_proc <- datos_proc |>
  mutate(decil_ingreso = if_else(ingreso_hogar == 11, 0, ingreso_hogar),
         decil_ingreso = if_else(decil_ingreso == 0, NA, decil_ingreso))

frq(datos_proc$decil_ingreso)

# --- fichas pensiones ----

frq(datos_proc$fichas_pensiones) # ok 

# --- fichas seguridad ----

frq(datos_proc$fichas_seguridad) # ok 

# --- evaluacion gobierno ----

frq(datos_proc$eval_gobierno_kast) 

datos_proc <- datos_proc |> 
  mutate(eval_gobierno_kast = case_when( # transformamos a caracter
      eval_gobierno_kast == 1 ~ "Positiva",
      eval_gobierno_kast == 2 ~ "Negativa",
      eval_gobierno_kast == 3 ~ "Ni positiva ni negativa"
    ),
    eval_gobierno_kast = factor(eval_gobierno_kast,
                         levels = c("Positiva", "Negativa", "Ni positiva ni negativa"),
                         ordered = TRUE)) # convertimos a factor ordenado

frq(datos_proc$eval_gobierno_kast) 

## 3.4 Missing values ----

# ¿Cuántos valores perdidos (NA) hay?
sum(is.na(datos_proc))

# ¿Y en toda la base, columna por columna?
colSums(is.na(datos_proc))

## 3.5 Labels ----

# Volvemos a seleccionar solo aquellas variables que usaremos posteriormente

datos_proc <- datos_proc |> 
  select(id,
         sexo,
         tramo_edad,
         ideologia,
         quintil_ingreso,
         decil_ingreso,
         fichas_pensiones,
         fichas_seguridad,
         eval_gobierno_kast 
        )

# Fijamos la etiqueta de cada variable (qué pregunta o contenido
# representa), una por una, con sjlabelled::set_label(). Así, quien reciba
# esta base después puede usar get_label() para saber qué es cada columna
# sin tener que ir a buscar el codebook.
datos_proc$id                    <- set_label(datos_proc$id, "ID")
datos_proc$sexo                  <- set_label(datos_proc$sexo, "Sexo")
datos_proc$tramo_edad            <- set_label(datos_proc$tramo_edad, "Tramo etario")
datos_proc$ideologia             <- set_label(datos_proc$ideologia, "Identificación política")
datos_proc$quintil_ingreso       <- set_label(datos_proc$quintil_ingreso, "Quintil de ingreso del hogar")
datos_proc$decil_ingreso         <- set_label(datos_proc$decil_ingreso, "Decil de ingreso del hogar")
datos_proc$fichas_pensiones      <- set_label(datos_proc$fichas_pensiones, "Fichas de gasto público pensiones")
datos_proc$fichas_seguridad      <- set_label(datos_proc$fichas_seguridad, "Fichas de gasto público seguridad")
datos_proc$eval_gobierno_kast    <- set_label(datos_proc$eval_gobierno_kast, "Evaluación del gobierno")

# Ejemplo de verificación:
get_label(datos_proc$ideologia)

# 4. Save and export -------------------------------------------------

save(datos_proc, file = "output/data/datos_proc.RData")

# Alternativas, por si se necesita el archivo en otro formato:
# saveRDS(datos_proc, "../output/data/datos_proc.rds")
# readr::write_csv(datos_proc, "../output/data/datos_proc.csv")
