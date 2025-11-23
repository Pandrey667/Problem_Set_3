#Limpiamos la consola
rm(list = ls())

#Cargar librerías 
require("pacman")
p_load("tidyverse",  # Conjunto de paquetes para manipulación, visualización y análisis de datos 
       "caret",       # Herramientas para preprocesamiento, selección de modelos y evaluación de algoritmos de machine learning.
       "glmnet",      # Implementación eficiente de modelos de regresión regularizados (EN, Lasso y Ridge).
       "MLeval",      # Funciones para evaluar modelos de clasificación y regresión con métricas y gráficos.
       "MLmetrics",   # Colección de métricas de evaluación para modelos de machine learning.
       "randomForest",
       "ranger",
       "xgboost",
       "dplyr",
       "readr",
       "Matrix",
       "sf",
       "forcats",
       "stringi",
       "xtable")   

set.seed(123)  # Fijar semilla para reproducibilidad de resultados.

#Obtenemos el usuario del sistema
username <- Sys.getenv("USERNAME")
#Creamos ruta dinámica
ruta_datos <- file.path("C:/Users", username,"OneDrive - Universidad de los Andes/Big Data y Machine Learning/Taller 3/Parte 2")
#Establecemos el directorio de trabajo
setwd(ruta_datos)

### Cargamos el archivo rds del Train
raw_train <- readRDS("df_def_entrenamiento.rds")

### Cargamos el archivo rds del Test
raw_test  <- readRDS("df_def_testeo.rds")

### Comprobar si existe una columna geometry en Train y si cumple la elimina
if ("geometry" %in% names(raw_train)) raw_train <- sf::st_drop_geometry(raw_train)

### Comprobar si existe una columna geometry en Test y si cumple la elimina
if ("geometry" %in% names(raw_test))  raw_test  <- sf::st_drop_geometry(raw_test)

### Eliminando los duplicados en la columna property_id del Test
raw_test <- raw_test %>% 
  distinct(property_id, 
           .keep_all = TRUE)

### La función safe_coalesce_area crea una variable con el área consolidada
safe_coalesce_area <- function(df) {
  vars <- intersect(c("area_reportada", 
                      "surface_covered", 
                      "area_m2"), 
                    names(df))
  stopifnot(length(vars) > 0)
  x <- df[[vars[1]]]
  if (length(vars) > 1)
    for (j in 2:length(vars))
      x <- dplyr::coalesce(x, 
                           df[[vars[j]]])
  df$area_m2_final <- x
  df
}


### Limpiamos la variable de estratos y la dejamos como un número entero en el Train
train_imp <- safe_coalesce_area(raw_train) %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year = factor(year)
  ) %>%
  select(-any_of("estrato"))


### Limpiamos la variable de estratos y la dejamos como un número entero en el Test
test_imp <- safe_coalesce_area(raw_test) %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year = factor(year)
  ) %>%
  select(-any_of("estrato"))


###############################################################
# 3) IMPUTACIÓN Y ALINEACIÓN
###############################################################

mode_val <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA_character_)
  as.character(ux[which.max(tabulate(match(x, ux)))])
}

impute_fit <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, "price")
  
  med <- vapply(df[num_cols], function(x) median(x, na.rm = TRUE), numeric(1))
  med[!is.finite(med)] <- 0
  
  cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  mod <- vapply(cat_cols, function(nm) mode_val(df[[nm]]), character(1))
  
  list(med = med, mod = mod)
}

impute_apply <- function(df, imp) {
  out <- df
  for (nm in intersect(names(imp$med), names(out))) {
    v <- out[[nm]]
    v[is.na(v)] <- imp$med[[nm]]
    out[[nm]] <- v
  }
  for (nm in intersect(names(imp$mod), names(out))) {
    v <- as.character(out[[nm]])
    v[is.na(v)] <- imp$mod[[nm]]
    out[[nm]] <- factor(v)
  }
  out
}

align_factor_levels <- function(train_df, test_df, fac_cols = c("month","year")) {
  out <- test_df
  for (fc in fac_cols) {
    if (fc %in% names(train_df) && fc %in% names(test_df)) {
      levs <- levels(train_df[[fc]])
      if (length(levs) == 0) levs <- unique(na.omit(train_df[[fc]]))
      if (length(levs) == 0) levs <- "missing"
      out[[fc]] <- as.character(out[[fc]])
      out[[fc]][!(out[[fc]] %in% levs) | is.na(out[[fc]])] <- levs[1]
      out[[fc]] <- factor(out[[fc]], levels = levs)
    }
  }
  out
}

imp_par <- impute_fit(train_imp)
train_imp <- impute_apply(train_imp, imp_par)
test_imp  <- impute_apply(test_imp, imp_par)

test_imp <- align_factor_levels(train_imp, test_imp)

if (!("price" %in% names(train_imp)) && "price" %in% names(raw_train))
  train_imp$price <- raw_train$price

train_imp$price <- suppressWarnings(readr::parse_number(as.character(train_imp$price)))

stopifnot(length(train_imp$price) == nrow(train_imp))

