# =============================================================================
# PREPARACIÓN Y LIMPIEZA DE DATOS 
# Problem Set  3 - Big Data y Machine Learning
# =============================================================================

# =============================================================================
# 1. LIMPIEZA DEL AMBIENTE
# =============================================================================
# Limpiamos el ambiente de RStudio para comenzar desde cero
remove(list = ls())

# =============================================================================
# 2. CARGA DE LIBRERÍAS
# =============================================================================

library(tidyverse)      # Manipulación y manejo de datos
library(data.table)     # Manejo de grandes volúmenes de datos
library(rvest)          # Extracción y análisis de texto desde web
library(stringi)        # Manipulación de datos en caracteres
library(readxl)         # Importar bases en formato Excel
library(sf)             # Importación y uso de archivos shapefile
library(osmdata)        # Datos de OpenStreetMap
library(leaflet)        # Visualización de mapas interactivos
library(fuzzyjoin)      # Join aproximado de datos
library(scales)         # Formateo de escalas y porcentajes

# =============================================================================
# 3. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# =============================================================================

setwd('C:/Users/fevid/Documents/MEcA/Big Data/Taller_3/datos_taller_3')
#setwd("C:/Users/investigacion/Desktop/SP3BDML")


# =============================================================================
# 4. CARGA Y PREPROCESAMIENTO DE ARCHIVOS CSV
# =============================================================================

# Guardamos en una lista los nombres de los archivos csv (Train y Test)
lista_t3_txt <- list.files(pattern = "\\.csv$", 
                           full.names = TRUE)

# Usamos el loop map (librería purrr) para leer y guardar en una lista los datos
lista_t3 <- lista_t3_txt %>%
  set_names(basename(.)) %>%
  map(\(f) read.csv(f, stringsAsFactors = FALSE)) %>%
  # Convertimos nombres de columnas a letra minúscula
  map(\(x) { names(x) <- tolower(names(x)); x })

# Extraemos el data frame que contiene el título "train" en el nombre
train <- lista_t3 %>% 
  pluck(which(str_detect(names(.), "train")))

# Convertimos el df de train a un objeto tipo data.table
setDT(train)

# =============================================================================
# 5. NORMALIZACIÓN DE TEXTOS 
# =============================================================================

# Corregimos errores ortográficos específicos en la variable title
train[, title := stringr::str_replace_all(title,
                                          stringr::regex("\\bbrbara\\b", ignore_case = TRUE),
                                          "barbara")]

# Normalizamos la variable title: minúsculas, sin acentos, sin puntuación
train[, title_norm := title %>% 
        stringr::str_to_lower() %>% 
        stringi::stri_trans_general("Latin-ASCII")  %>% 
        stringr::str_replace_all("[[:punct:]]", " ") %>% 
        stringr::str_squish()]

# Creamos vector con palabras a excluir de las descripciones
excluir_desc <- c("balcon", 
                  "patio", 
                  "estacion", 
                  "torre", 
                  "jardin")

# Construimos el patrón regex con las palabras a excluir
pat_excluir <- paste0("\\b(?:", 
                      paste(excluir_desc, 
                            collapse = "|"), 
                      ")\\b")

# Normalizamos la variable description: minúsculas, sin acentos, sin puntuación
train[, desc_norm := description %>% 
        stringr::str_to_lower() %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        stringr::str_replace_all("[[:punct:]]", " ") %>%
        stringr::str_squish() %>%
        stringr::str_remove_all(pat_excluir) %>%
        stringr::str_squish()]