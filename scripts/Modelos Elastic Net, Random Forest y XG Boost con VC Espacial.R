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
#MODELO Boosting (XGBoost)

#Elegimos las variables a usar en el modelo
form_x <- log_price ~ . - property_id - log_price

#Construimos model.frame sin que se borre TODO
mf_train <- model.frame(form_x, data = base_train_df, na.action = na.omit)

#Verificamos que las filas se mantengan
nrow(mf_train)

# Construimos los TERMS usando también la data
terms_full <- terms(form_x, data = base_train_df)

# Armamos el model.frame del TRAIN
mf_train <- model.frame(terms_full, data = base_train_df, na.action = na.omit)

# Generamos y_train
y_train  <- model.response(mf_train)

# Matriz X_train
X_train  <- model.matrix(terms_full, data = base_train_df)[, -1, drop = FALSE]

# TERMS para TEST
terms_x  <- delete.response(terms_full)

#Matriz X_test con la misma estructura de columnas
X_test   <- model.matrix(terms_x, data = base_test_df)[, -1, drop = FALSE]

# Chequeos
dim(X_train)
dim(X_test)
length(y_train)

#Convertimos las matrices creadas al modelo XGBoost
X_train_mat <- as.matrix(X_train)
X_test_mat  <- as.matrix(X_test)
y_vec       <- as.numeric(y_train)   

#Creamos train / valid interno para evaluar (MAE)
#Vamos a separar 80% para entrenar y 20% para validar:

n <- nrow(X_train_mat)
idx_train <- sample(1:n, size = floor(0.8 * n))
idx_valid <- setdiff(1:n, idx_train)

X_tr <- X_train_mat[idx_train, ]
X_va <- X_train_mat[idx_valid, ]
y_tr <- y_vec[idx_train]
y_va <- y_vec[idx_valid]

#Construimos los DMatrix que usa XGBoost
dtrain <- xgb.DMatrix(data = X_tr, label = y_tr)
dvalid <- xgb.DMatrix(data = X_va, label = y_va)

# También uno con TODO el train (para el modelo final después)
dall   <- xgb.DMatrix(data = X_train_mat, label = y_vec)

# Y el test
dtest  <- xgb.DMatrix(data = X_test_mat)

#Asignamos los hiperparametros del XGBoost
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # predice log_price con MSE en log
  eval_metric = "mae",             # monitoreamos MAE (sobre log_price)
  eta = 0.05,                      # learning rate (más pequeño = más estable)
  max_depth = 6,                   # profundidad de los árboles
  subsample = 0.8,                 # fracción de filas usada por árbol
  colsample_bytree = 0.8,          # fracción de columnas por árbol
  min_child_weight = 5,            # regularización (no hacer hojas con muy pocos datos)
  lambda = 1,                      # L2 (ridge) en pesos
  alpha = 0                        # L1 (Lasso) en pesos (puedes probar > 0)
)

#Entrenamos con early stopping
watchlist <- list(
  train = dtrain,
  valid = dvalid
)

xgb_fit <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 2000,              # máximo de iteraciones
  watchlist = watchlist,
  early_stopping_rounds = 50,  # si 50 rondas seguidas no mejora el MAE, se para
  print_every_n = 50
)


#Generamos el número optimo de arboles y el minimo
xgb_fit$best_iteration  # número óptimo de árboles
xgb_fit$best_score      # MAE mínimo (en log_price)

#Entrenamos el modelo final y generamos predicciones
best_nrounds <- xgb_fit$best_iteration

xgb_final <- xgb.train(
  params = params,
  data   = dall,
  nrounds = best_nrounds
)

#Pasamos a pesos la predicción y redondeamos
pred_log   <- predict(xgb_final, dtest)
pred_price <- exp(pred_log)
pred_price <- round(pred_price, -3)

#Verificamos que todo esté alineado
length(pred_price)
nrow(base_test_df)

# Armamos el data frame de submission
submission_xgb <- data.frame(
  property_id = base_test_df$property_id,
  price       = pred_price
)

#Guardamos el archivo para Kaggle
write.csv(submission_xgb, "Boosting(XGBoost)YC.csv", row.names = FALSE)

#Visualizamos una parte de los datos
head(submission_xgb)
#ELASTIC NET
# Folds espaciales: cada cluster5 es una "zona" distinta de Bogotá
foldid_en <- base_train_df$cluster5

# Grid de alphas (mezcla Ridge/Lasso)
alpha_grid_en <- seq(0.1, 0.9, by = 0.2)  # 0.1, 0.3, 0.5, 0.7, 0.9

# Validación cruzada ESPACIAL optimizando MAE (en log_price)
results_cv_en <- purrr::map_df(alpha_grid_en, function(a) {
  cv_fit <- cv.glmnet(
    X_train_mat, y_vec,
    alpha        = a,
    family       = "gaussian",
    foldid       = foldid_en,   # <- CV espacial por cluster5
    nlambda      = 40,
    type.measure = "mae"        # MAE (en log_price)
  )
  
  tibble(
    alpha       = a,
    lambda_min  = cv_fit$lambda.min,
    lambda_1se  = cv_fit$lambda.1se,
    cv_mae_min  = min(cv_fit$cvm),
    cv_mae_1se  = cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.1se]
  )
})

# Revisas cómo le fue a cada alpha
print(results_cv_en)

# Elegimos el alpha / lambda con menor MAE

library(dplyr)

best_row_en <- results_cv_en[order(results_cv_en$cv_mae_min), ][1, , drop = FALSE]
best_row_en

best_alpha_en  <- best_row_en$alpha
best_lambda_en <- best_row_en$lambda_min

best_alpha_en 
best_lambda_en

# Entrenamos Elastic Net final con TODOS los datos de entrenamiento
modelo_en <- glmnet(
  X_train_mat,
  y_vec,
  alpha  = best_alpha_en,
  lambda = best_lambda_en,
  family = "gaussian"
)

# Predicciones en el test (df_def_testeo)
pred_log_en <- predict(modelo_en, newx = X_test_mat)
pred_log_en <- as.numeric(pred_log_en)

# Volvemos a pesos (COP) y redondeamos a miles
pred_price_en <- exp(pred_log_en)
pred_price_en <- round(pred_price_en, -3)

# Armamos submission con la base test RDS (que ya está alineada con Kaggle)
submission_en <- data.frame(
  property_id = base_test_df$property_id,
  price       = pred_price_en
)

# Guardamos CSV para Kaggle
write.csv(submission_en, "ElasticNet_YC_dfdef.csv", row.names = FALSE)


