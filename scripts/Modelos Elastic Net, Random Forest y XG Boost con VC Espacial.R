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
       "xgboost")   

set.seed(1011)  # Fijar semilla para reproducibilidad de resultados.

#Obtenemos el usuario del sistema
username <- Sys.getenv("USERNAME")
#Creamos ruta dinámica
ruta_datos <- file.path("C:/Users", username,"OneDrive - Universidad de los Andes/Big Data y Machine Learning/Taller 3/Parte 2")
#Establecemos el directorio de trabajo
setwd(ruta_datos)

#Cargamos las bases de datos
base_train <- readRDS("df_def_entrenamiento.rds")
base_test <- readRDS("df_def_testeo.rds")

#Convertimos a data frame las bases
base_train_df <- as.data.frame(base_train)
base_test_df  <- as.data.frame(base_test)

#Revisamos las dimensiones de las bases
dim(base_train_df)
dim(base_test_df)

#Visualizamos los nombres de las variables
names(base_train_df)[1:30]  

#Creamos un vector con las variables que vamos a eliminar porque no aportan nada
#en el modelo
cols_no_model <- c("geometry", "city","title_norm", "description", "desc_norm")  

#Le quitamos a las bases las variables que no vamos a usar
base_train_df <- base_train_df %>% select(-any_of(cols_no_model))
base_test_df <- base_test_df %>%
  select(-any_of(c(cols_no_model, "price")))


#LIMPIEZA DE DATOS

#Convertimos a logáritmo la variable precio
base_train_df <- base_train_df %>% mutate(log_price = log(price))

#Transformamos a factor las variables cateogoricas
cols_factor <- c(
  "property_type", "property_type2",
  "LocNombre", "upz", "barrio_oficial", "estrato"
)

## Solo convertimos a factor las variables existentes en las bases
for (col in cols_factor) {
  if (col %in% names(base_train_df)) {
    base_train_df[[col]] <- as.factor(base_train_df[[col]])
  }
  if (col %in% names(base_test_df)) {
    base_test_df[[col]] <- as.factor(base_test_df[[col]])
  }
}

#Aseguramos que las bases train y test tengan los mismos niveles de factores
for (col in cols_factor) {
  if (col %in% names(base_train_df) && col %in% names(base_test_df)) {
    base_test_df[[col]] <- factor(
      base_test_df[[col]],
      levels = levels(base_train_df[[col]])
    )
  }
}

#Imputamos la información que tiene NA

#Creamos clusters en Train para imputar 
coords_train <- base_train_df %>%
  dplyr::select(lat, lon) %>%
  as.matrix()

cl_km <- kmeans(coords_train, centers = 5, nstart = 20)

base_train_df$cluster5 <- cl_km$cluster


# Asignamos a TEST el cluster más cercano según los centros del k-means
coords_test <- base_test_df %>%
  dplyr::select(lat, lon) %>%
  as.matrix()

asignar_cluster <- function(punto, centros) {
  dists <- rowSums(
    (centros - matrix(punto, nrow = nrow(centros), ncol = 2, byrow = TRUE))^2
  )
  which.min(dists)
}

base_test_df$cluster5 <- apply(coords_test, 1, asignar_cluster, centros = cl_km$centers)

#Ahora imputamos variables númericas y categoricas con base a cada cluster

# Funciómn de moda para variables categoricas
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Variables numéricas que vamos a imputar
vars_num_imp <- names(base_train_df)[
  sapply(base_train_df, is.numeric) &
    !names(base_train_df) %in% c("log_price", "cluster5")
]

# Calculamos medianas por cluster en TRAIN
medianas_cluster <- base_train_df %>%
  group_by(cluster5) %>%
  summarise(
    across(
      all_of(vars_num_imp),
      ~ median(.x, na.rm = TRUE),
      .names = "med_{.col}"
    ),
    .groups = "drop"
  )

# Imputamos TRAIN con esas medianas
base_train_df <- base_train_df %>%
  left_join(medianas_cluster, by = "cluster5")

for (v in vars_num_imp) {
  med_col <- paste0("med_", v)
  idx_na  <- is.na(base_train_df[[v]])
  base_train_df[[v]][idx_na] <- base_train_df[[med_col]][idx_na]
}

# Quitamos columnas temporales med_
base_train_df <- base_train_df %>%
  select(-starts_with("med_"))

# Imputamos TEST usando las mismas medianas de TRAIN
base_test_df <- base_test_df %>%
  left_join(medianas_cluster, by = "cluster5")

for (v in vars_num_imp) {
  med_col <- paste0("med_", v)
  idx_na  <- is.na(base_test_df[[v]])
  base_test_df[[v]][idx_na] <- base_test_df[[med_col]][idx_na]
}

base_test_df <- base_test_df %>%
  select(-starts_with("med_"))

#Imputación categórica por cluster (moda del TRAIN por cluster)

# Variables categóricas que vamos a imputar
vars_cat_imp <- names(base_train_df)[sapply(base_train_df, is.factor)]

# Calculamos modas por cluster en TRAIN
modas_cluster <- base_train_df %>%
  group_by(cluster5) %>%
  summarise(
    across(
      all_of(vars_cat_imp),
      ~ get_mode(.x),
      .names = "mod_{.col}"
    ),
    .groups = "drop"
  )

# Imputamos TRAIN
base_train_df <- base_train_df %>%
  left_join(modas_cluster, by = "cluster5")

for (v in vars_cat_imp) {
  mod_col <- paste0("mod_", v)
  idx_na  <- is.na(base_train_df[[v]])
  base_train_df[[v]][idx_na] <- base_train_df[[mod_col]][idx_na]
  base_train_df[[v]] <- factor(base_train_df[[v]])
}

base_train_df <- base_train_df %>%
  select(-starts_with("mod_"))

# Imputamos TEST con las mismas modas de TRAIN
base_test_df <- base_test_df %>%
  left_join(modas_cluster, by = "cluster5")

for (v in vars_cat_imp) {
  mod_col <- paste0("mod_", v)
  idx_na  <- is.na(base_test_df[[v]])
  base_test_df[[v]][idx_na] <- base_test_df[[mod_col]][idx_na]
  base_test_df[[v]] <- factor(
    base_test_df[[v]],
    levels = levels(base_train_df[[v]])
  )
}

base_test_df <- base_test_df %>%
  select(-starts_with("mod_"))

#Quitamos price de la base train porque es la variable a predecir y no esta en test
base_train_df <- base_train_df %>% select(-price)


#Verificamo NAs después de imputar por cluster
colSums(is.na(base_train_df))
colSums(is.na(base_test_df))

