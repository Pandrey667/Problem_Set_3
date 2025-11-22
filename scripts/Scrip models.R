# ===================================================================
#             SCRIPT XGBOOST COMPLETO (+ variante corregida)
# ===================================================================

library(tidyverse)
library(glmnet)
library(ranger)
library(caret)
library(Matrix)
library(xgboost)

options(scipen = 999)

# ==================================================================================
# 1. CARGA DE DATOS
# ==================================================================================
train <- readRDS("C:/Users/user}/Downloads/df_def_entrenamiento.rds")
test  <- readRDS("C:/Users/user}/Downloads/df_def_testeo.rds")

cat("Train:", nrow(train), " | Test:", nrow(test), "\n")

# ==================================================================================
# 2. VARIABLES ECONÓMICAMENTE RELEVANTES
# ==================================================================================
predictors <- c(
  "area_m2","surface_covered","bedrooms","banos_tot",
  "dummy_garaje","dummy_terraza","dummy_apartaestudio",
  "dummy_apartamento","dummy_casa","piso_numerico",
  "n_pisos_num","estrato","num_homicidios_anual",
  "num_hurto_re_anual","num_hurto_pe_anual",
  "perc_variacion_homicidios","perc_variacion_hurto_re",
  "perc_variacion_hurto_pe","distancia_parque","area_parque",
  "distancia_colegio","distancia_universidad","distancia_hospital",
  "distancia_supermercado","distancia_bus","distancia_cai",
  "distancia_avenida_principal","distancia_centrocomercial",
  "LocNombre","upz","barrio_oficial"
)

predictors <- predictors[predictors %in% colnames(train)]

train2 <- train %>% select(property_id, price, all_of(predictors))
test2  <- test  %>% select(property_id, all_of(predictors))

# ==================================================================================
# 3. CREAR AREA_FINAL
# ==================================================================================
train2 <- train2 %>% mutate(area_final = coalesce(as.numeric(area_m2), as.numeric(surface_covered)))
test2  <- test2  %>% mutate(area_final = coalesce(as.numeric(area_m2), as.numeric(surface_covered)))

predictors <- c("area_final", setdiff(predictors, c("area_m2","surface_covered")))

# ==================================================================================
# 4. IMPUTACIÓN BAJO MEDIANA
# ==================================================================================
num_vars <- train2 %>% select(-property_id, -price) %>% select(where(is.numeric)) %>% colnames()

for(v in num_vars){
  med <- median(train2[[v]], na.rm=TRUE)
  if(is.na(med)) med <- 0
  train2[[v]][is.na(train2[[v]])] <- med
  if(!(v %in% colnames(test2))) test2[[v]] <- NA_real_
  test2[[v]][is.na(test2[[v]])] <- med
}

# ==================================================================================
# 5. ARMONIZACIÓN DE FACTORES TRAIN–TEST
# ==================================================================================
char_vars <- train2 %>% select(-property_id, -price) %>% select(where(is.character)) %>% colnames()
for(v in char_vars) train2[[v]] <- as.factor(train2[[v]])

for(v in char_vars){
  if(!(v %in% colnames(test2))) test2[[v]] <- NA
  test2[[v]] <- factor(test2[[v]], levels=levels(train2[[v]]))
}

# NA en factores → OTHER
factor_vars <- train2 %>% select(-property_id, -price) %>% select(where(is.factor)) %>% colnames()
for(v in factor_vars){
  if(any(is.na(test2[[v]]))){
    levels(train2[[v]]) <- unique(c(levels(train2[[v]]), "OTHER"))
    train2[[v]][is.na(train2[[v]])] <- "OTHER"
    test2[[v]][is.na(test2[[v]])]  <- "OTHER"
  }
}

