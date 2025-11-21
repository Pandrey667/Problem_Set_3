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
