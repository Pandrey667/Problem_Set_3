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