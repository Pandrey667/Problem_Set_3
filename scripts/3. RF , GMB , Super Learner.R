# =============================================================================
# MODELOS ANDREY RINCÓN  
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

library(tidyverse)
library(caret)
library(glmnet)
install.packages("ranger")
library(ranger)
install.packages("gbm")
library(gbm)
library(rpart)
install.packages("SuperLearner")
library(SuperLearner)
library(parallel)
library(doParallel)
install.packages("xgboost")
library(xgboost)



# =============================================================================
# 3. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# =============================================================================

df_def_entrenamiento <- readRDS("C:/Users/investigacion/Desktop/SP3BDML/df_def_entrenamiento.rds")
df_def_testeo <- readRDS("C:/Users/investigacion/Desktop/SP3BDML/df_def_testeo.rds")

# =============================================================================
# 4. CREAMOS INTERACCIONES
# =============================================================================



data <- df_def_entrenamiento %>%
  mutate(
    int_homicidio_casa      = dummy_casa * num_homicidios_anual,
    int_homicidio_apto      = dummy_apartamento * num_homicidios_anual,
    int_homicidio_apartaestudio = dummy_apartaestudio * num_homicidios_anual,
    
    int_hurto_casa          = dummy_casa * num_hurto_pe_anual,
    int_hurto_apto          = dummy_apartamento * num_hurto_pe_anual,
    int_hurto_apartaestudio = dummy_apartaestudio * num_hurto_pe_anual,
    
    int_var_homicidio_casa      = dummy_casa * perc_variacion_homicidios,
    int_var_homicidio_apto      = dummy_apartamento * perc_variacion_homicidios,
    int_var_homicidio_apartaestudio = dummy_apartaestudio * perc_variacion_homicidios,
    
    int_var_hurto_casa          = dummy_casa * perc_variacion_hurto_pe,
    int_var_hurto_apto          = dummy_apartamento * perc_variacion_hurto_pe,
    int_var_hurto_apartaestudio = dummy_apartaestudio * perc_variacion_hurto_pe
  )

colnames(data)

testeo <- df_def_testeo %>%
  mutate(
    int_homicidio_casa      = dummy_casa * num_homicidios_anual,
    int_homicidio_apto      = dummy_apartamento * num_homicidios_anual,
    int_homicidio_apartaestudio = dummy_apartaestudio * num_homicidios_anual,
    
    int_hurto_casa          = dummy_casa * num_hurto_pe_anual,
    int_hurto_apto          = dummy_apartamento * num_hurto_pe_anual,
    int_hurto_apartaestudio = dummy_apartaestudio * num_hurto_pe_anual,
    
    int_var_homicidio_casa      = dummy_casa * perc_variacion_homicidios,
    int_var_homicidio_apto      = dummy_apartamento * perc_variacion_homicidios,
    int_var_homicidio_apartaestudio = dummy_apartaestudio * perc_variacion_homicidios,
    
    int_var_hurto_casa          = dummy_casa * perc_variacion_hurto_pe,
    int_var_hurto_apto          = dummy_apartamento * perc_variacion_hurto_pe,
    int_var_hurto_apartaestudio = dummy_apartaestudio * perc_variacion_hurto_pe
  )



# ========================================
# 5 SPLIT DE ENTRENAMIENTO Y TEST
# ========================================
set.seed(1453)
in_train <- createDataPartition(y = data$price, p = 0.7, list = FALSE)
train <- data[in_train, ]
test  <- data[-in_train, ]

train <- train %>% select(-geometry)
test  <- test  %>% select(-geometry)
testeo <- testeo %>% select(-geometry)


# ========================================
# 6 MODELOS
# ========================================

# ----------- RANDOM FOREST BASICO  -------------

# Activamos paralelización usando todos los núcleos menos uno
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)
# Verificamos cuántos núcleos están registrados
getDoParWorkers()
getDoParName()

# Entrenamos un modelo Random Forest con ranger
# price es la variable objetivo
# usa todas las demás variables del dataset de entrenamiento
# num.trees: cantidad de árboles
# mtry: número de variables probadas por split
# min.node.size: tamaño mínimo de nodos terminales
# sample.fraction: proporción de datos usados por árbol

set.seed(1453)

rf_model <- ranger(
  price ~ .,
  data = train,
  num.trees = 800,
  mtry = floor(sqrt(ncol(train))), 
  min.node.size = 5,
  sample.fraction = 0.8,
  importance = "impurity"
)

# Obtenemos predicciones para train y test
pred_train <- predict(rf_model, train)$predictions
pred_test  <- predict(rf_model, test)$predictions

# Calculamos el MAE para train y test
MAE_train_rf <- mean(abs(train$price - pred_train))
MAE_test_rf  <- mean(abs(test$price - pred_test))

MAE_train_rf
MAE_test_rf

# Generamos predicción final sobre el dataset de testeo
pred_testeo <- predict(rf_model, testeo)$predictions
pred_testeo_round <- round(pred_testeo, -3)

# Creamos archivo final para Kaggle
resultado <- testeo %>%
  select(property_id) %>%
  mutate(price = pred_testeo_round)

write_csv(resultado, "rf_prediction.csv")

# Verificamos fecha de modificación del archivo y directorio actual
file.info("rf_prediction.csv")$mtime
getwd()


# ----------- GBM -------------

# Convertimos variables tipo character en factores
# Esto es necesario para que GBM y SuperLearner puedan trabajar correctamente
train <- train %>% mutate(across(where(is.character), as.factor))
test  <- test  %>% mutate(across(where(is.character), as.factor))
testeo <- testeo %>% mutate(across(where(is.character), as.factor))


# Eliminamos factores que solo tienen un nivel (no aportan variabilidad)

one_level_factors <- names(Filter(function(x) is.factor(x) && length(unique(x)) < 2, train))
one_level_factors

train_clean <- train %>% select(-all_of(one_level_factors))
test_clean <- test %>% select(-all_of(one_level_factors))

# Lo mismo para testeo
one_level_factors <- names(Filter(function(x) is.factor(x) && length(unique(x)) < 2, train))
testeo <- testeo %>% select(-all_of(one_level_factors))

# Identificamos variables categóricas con demasiados niveles
# para evitar explosión de memoria al entrenar GBM

sapply(train_clean, function(x) length(unique(x)))


high_cardinality <- names(which(
  sapply(train_clean, function(x) !is.numeric(x) && length(unique(x)) > 100)
))
high_cardinality

# Eliminamos las variables de alta cardinalidad

train_reduced <- train_clean %>% select(-all_of(high_cardinality))
test_reduced  <- test_clean  %>% select(-all_of(high_cardinality))
testeo <- testeo %>% select(-all_of(high_cardinality))

# Reconfiguramos paralelización

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

getDoParWorkers()
getDoParName()

# Imputamos valores faltantes en variables numéricas del train

num_vars <- names(Filter(is.numeric, train_reduced))

for (v in num_vars) {
  med <- median(train_reduced[[v]], na.rm = TRUE)
  train_reduced[[v]][is.na(train_reduced[[v]])] <- med
}

# Imputamos valores faltantes en variables categóricas del train

fac_vars <- names(Filter(is.factor, train_reduced))

for (v in fac_vars) {
  mode_v <- names(sort(table(train_reduced[[v]]), decreasing = TRUE))[1]
  train_reduced[[v]][is.na(train_reduced[[v]])] <- mode_v
}

# Verificamos que no queden NA
sum(is.na(train_reduced))


# Imputamos NA en test_reduced y testeo usando valores del train

for (v in num_vars) {
  med <- median(train_reduced[[v]], na.rm = TRUE)
  test_reduced[[v]][is.na(test_reduced[[v]])] <- med
  testeo[[v]][is.na(testeo[[v]])] <- med
}

for (v in fac_vars) {
  mode_v <- names(sort(table(train_reduced[[v]]), decreasing = TRUE))[1]
  test_reduced[[v]][is.na(test_reduced[[v]])] <- mode_v
  testeo[[v]][is.na(testeo[[v]])] <- mode_v
}




# Configuramos la grilla de hiperparámetros para GBM

grid_gbm <- expand.grid(
  n.trees = c(300, 600, 1000),
  interaction.depth = c(3, 5),
  shrinkage = c(0.01, 0.005),
  n.minobsinnode = c(10, 20)
)

# Configuramos validación cruzada con paralelización

ctrl_gbm <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE   # <--- CLAVE
)

# Entrenamos el modelo GBM optimizando MAE

set.seed(1453)

gbm_opt <- train(
  price ~ .,
  data = train_reduced,
  method = "gbm",
  trControl = ctrl_gbm,
  tuneGrid = grid_gbm,
  metric = "MAE",
  distribution = "gaussian",
  verbose = FALSE
)

# Igualamos niveles de factores entre train, test y testeo

for (v in names(Filter(is.factor, train_reduced))) {
  test_reduced[[v]] <- factor(test_reduced[[v]], levels = levels(train_reduced[[v]]))
  testeo[[v]] <- factor(testeo[[v]], levels = levels(train_reduced[[v]]))
}

# Predicciones en train y test

pred_train_gbm <- predict(gbm_opt, train_reduced)
pred_test_gbm  <- predict(gbm_opt, test_reduced)

# Métricas de error

MAE_train_gbm <- mean(abs(train_reduced$price - pred_train_gbm))
MAE_test_gbm  <- mean(abs(test_reduced$price - pred_test_gbm))

MAE_train_gbm
MAE_test_gbm

# Predicción final para Kaggle

pred_testeo_gbm <- predict(gbm_opt, testeo)
pred_testeo_gbm_round <- round(pred_testeo_gbm, -3)

# Revisamos que testeo tenga todas las filas y detectamos filas incompleta

nrow(testeo)           
length(pred_testeo_gbm)   
rows_with_NA <- which(!complete.cases(testeo))
rows_with_NA

# Reimputamos NA generados en testeo después de igualar niveles

for (v in num_vars) {
  med <- median(train_reduced[[v]], na.rm = TRUE)
  testeo[[v]][is.na(testeo[[v]])] <- med
}

for (v in fac_vars) {
  mode_v <- names(sort(table(train_reduced[[v]]), decreasing = TRUE))[1]
  testeo[[v]][is.na(testeo[[v]])] <- mode_v
}


pred_testeo_gbm <- predict(gbm_opt, testeo)
pred_testeo_gbm_round <- round(pred_testeo_gbm, -3)

resultado_gbm <- df_def_testeo %>%
  select(property_id) %>%
  mutate(price = pred_testeo_gbm_round)

write_csv(resultado_gbm, "gbm_prediction.csv")

# Información de archivo generado

file.info("gbm_prediction.csv")$mtime
getwd()


# -----------  SUPER LEARNER   -------------

# Para que no colapse memoria RAM 
options(mc.cores = 1)

# también aseguramos que no queden clusters activos
stopImplicitCluster()

# Ajustes livianos para los modelos base
options(SL.ranger.num.trees = 300)
options(SL.gbm.trees = 100)
options(SL.gbm.interaction.depth = 2)
options(SL.gbm.shrinkage = 0.01)

# Activar mensajes para ver avance
options(SL.verbose = TRUE)


# Definimos nuestro vector objetivo a partir del dataset reducido.
# Este dataset ya pasó por limpieza: eliminación de niveles únicos, variables
# con demasiados niveles y toda la imputación numérica y categórica.

ySL <- train_reduced$price

# Construimos XSL, el conjunto de predictores que vamos a usar en el SuperLearner.
# Seleccionamos un subconjunto razonable de variables relevantes

XSL <- train_reduced %>% select(
  bedrooms, banos_tot, area_m2,
  distancia_parque, distancia_supermercado,
  distancia_avenida_principal, distancia_universidad,
  dummy_casa, dummy_apartamento, dummy_apartaestudio,
  int_homicidio_casa, int_hurto_casa,
  int_var_homicidio_casa, int_var_hurto_casa
)


# Definimos la librería de modelos base

sl.lib <- c(
  "SL.mean",
  "SL.lm",
  "SL.glm",
  "SL.ridge",
  "SL.glmnet",
  "SL.ranger",
  "SL.gbm"
)



# Establecemos la forma en que el SuperLearner va a evaluar

set.seed(1453)

fitSL <- SuperLearner(
  Y = ySL,
  X = as.data.frame(XSL),
  SL.library = sl.lib,
  method = "method.NNLS",
  cvControl = list(V = 3)
)

# Predicción sobre el conjunto "test_reduced"


X_test_SL <- test_reduced %>% select(
  bedrooms, banos_tot, area_m2,
  distancia_parque, distancia_supermercado,
  distancia_avenida_principal, distancia_universidad,
  dummy_casa, dummy_apartamento, dummy_apartaestudio,
  int_homicidio_casa, int_hurto_casa,
  int_var_homicidio_casa, int_var_hurto_casa
)

# Generamos las predicciones del SuperLearner en el conjunto de prueba.

pred_test_SL <- predict(fitSL, X_test_SL, onlySL = TRUE)$pred

# Calculamos el error medio absoluto.

MAE_test_SL <- mean(abs(test_reduced$price - pred_test_SL))
MAE_test_SL


# Predicción final sobre el testeo para enviar a Kaggle

X_testeo_SL <- testeo %>% select(
  bedrooms, banos_tot, area_m2,
  distancia_parque, distancia_supermercado,
  distancia_avenida_principal, distancia_universidad,
  dummy_casa, dummy_apartamento, dummy_apartaestudio,
  int_homicidio_casa, int_hurto_casa,
  int_var_homicidio_casa, int_var_hurto_casa
)

# Predicción final con SL para el archivo de Kaggle

pred_testeo_SL <- predict(fitSL, newdata = X_testeo_SL, onlySL = TRUE)$pred
pred_testeo_SL_round <- round(pred_testeo_SL, -3)


# Armamos el archivo final para entregar en Kaggle

resultado_SL <- df_def_testeo %>%
  select(property_id) %>%
  mutate(price = pred_testeo_SL_round)

write.csv(resultado_SL, "superlearner_prediction.csv", row.names = FALSE)

file.info("superlearner_prediction.csv")$mtime
getwd()

