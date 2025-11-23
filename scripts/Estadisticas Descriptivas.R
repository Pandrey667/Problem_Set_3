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

##########################################################
# GENERACIÓN DE TABLAS CON ESTADÍSTICAS DESCRIPTIVAS
###############################################################
# ------------------------
# Variables de numéricas
# ------------------------

dummy_cols <- grep("^dummy_", names(train_imp), value = TRUE)

num_cols <- names(train_imp)[sapply(train_imp, is.numeric)]
num_cols <- setdiff(num_cols, c("price", dummy_cols))#excluimos price y dummys

# Función para tabla numérica
tabla_numericas <- function(df, base_name){
  df %>%
    select(all_of(num_cols)) %>%
    summarise(
      across(
        everything(),
        list(
          n      = ~sum(!is.na(.)),
          mean   = ~mean(., na.rm = TRUE),
          median = ~median(., na.rm = TRUE)
        ),
        .names = "{.col}___{.fn}"
      )
    ) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = c("variable","stat"),
      names_sep = "___",
      values_to = "valor"
    ) %>%
    pivot_wider(
      names_from  = stat,
      values_from = valor
    ) %>%
    mutate(Base = base_name)
}


# Tablas Train y Test
tab_train_num <- tabla_numericas(train_imp, "Train")
tab_test_num  <- tabla_numericas(test_imp,  "Test")

# Unión final

tabla_num_base <- bind_rows(tab_train_num, tab_test_num) %>%
  select(Base, variable, n, mean, median)

#Convertir la tabla a Data frame
tabla_numerica_final_df <- as.data.frame(tabla_num_base )

## Ajustamos el diseño de la tabla

tabla_numerica_final <- tabla_num_base %>%
  mutate(
    n          = as.integer(n),
    mean_num   = as.numeric(mean),
    median_num = as.numeric(median),
    # formateo:
    mean_print = if_else(
      grepl("^perc_variacion", variable),
      sprintf("%.2f", mean_num),   # porcentajes: 2 decimales
      sprintf("%.0f", mean_num)    # resto: sin decimales
    ),
    median_print = if_else(
      grepl("^perc_variacion", variable),
      sprintf("%.2f", median_num),
      sprintf("%.0f", median_num)
    )
  ) %>%
  transmute(
    Base,
    variable,
    n,
    mean   = mean_print,
    median = median_print
  )

tabla_numerica_final %>% filter(grepl("^perc_variacion", variable))

tabla_numerica_tex <- tabla_numerica_final %>%
  mutate(variable = paste0("\\texttt{", variable, "}"))

## Exportamos a LaTeX

print(
  xtable(
    tabla_numerica_tex,
    caption = "Tabla X. Estadísticas descriptivas de variables numéricas en las bases \\textit{Train} y \\textit{Test}.",
    label   = "tab:descriptivas_num",
    align   = c("l","l","l","c","r","r")  # Base, variable, n, mean, median
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity
)


# ------------------------
# Variables de categóricas
# ------------------------

# Dummies: numéricas que solo tienen 0/1 (ignorando NA)
dummy_cols <- names(train_imp)[sapply(train_imp, function(x) {
  is.numeric(x) && all(na.omit(x) %in% c(0, 1))
})]

# Categóricas "naturales": factor o character
cat_base_raw <- names(train_imp)[sapply(train_imp, function(x) {
  is.factor(x) || is.character(x)
})]


# Excluir IDs y texto largo (títulos / descripciones)
cat_base <- setdiff(
  cat_base_raw,
  c("property_id",
    "title", "title_norm",
    "description", "desc_norm",
    "titulo", "descripcion",
    "year","city")  
)

cat_base <- cat_base[!grepl("title|desc", cat_base)]

cat_base <- cat_base[sapply(train_imp[cat_base], function(x) {
  dplyr::n_distinct(x, na.rm = TRUE) <= 10
})]

#Unimos las bases
cat_cols <- union(cat_base, dummy_cols)

# Función para construir tabla de categóricas por base
tabla_categoricas <- function(df, base_name){
  df %>%
    select(all_of(cat_cols)) %>%
    mutate(across(everything(), as.character)) %>%   # todo a texto
    pivot_longer(
      cols      = everything(),
      names_to  = "variable",
      values_to = "categoria"
    ) %>%
    group_by(variable, categoria) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(variable) %>%
    mutate(prop = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    mutate(Base = base_name)
}


# Tablas Train y Test
tab_train_cat <- tabla_categoricas(train_imp, "Train")
tab_test_cat  <- tabla_categoricas(test_imp,  "Test")

# Tabla final ordenada para exportar
tabla_categorica_final <- bind_rows(tab_train_cat, tab_test_cat) %>%
  select(Base, variable, categoria, n, prop)

#Convertimos las variables a LaTeX
tabla_categorica_final_latex <- tabla_categorica_final %>%
  mutate(
    variable  = paste0("\\verb|", variable, "|"),
    categoria = paste0("\\verb|", categoria, "|")
  )

#Exportar a LaTeX
print(
  xtable(
    tabla_categorica_final_latex,
    caption = "Variables categóricas en las bases \\textit{Train} y \\textit{Test}.",
    label = "tab:variables_categoricas",
    align = c("l","l","l","l","r","r")
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity
)

