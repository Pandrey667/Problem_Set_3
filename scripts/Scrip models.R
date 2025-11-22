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

# ==================================================================================
# 6. UNIR FULL
# ==================================================================================
test2$price <- NA_real_
train2$.is_test <- 0
test2$.is_test  <- 1

full <- bind_rows(train2, test2)
cat("FULL:", nrow(full), "filas\n")

# ==================================================================================
# 7. LISTA DE PREDICTORAS FINALES
# ==================================================================================
predictors_final <- setdiff(colnames(full), c("property_id","price",".is_test"))

# ==================================================================================
# 8. FUNCIÓN ROBUSTA PARA CREAR MATRICES COLUMNA A COLUMNA
# ==================================================================================
build_block <- function(var, df){
  
  x <- df[[var]]
  n <- nrow(df)
  
  # numérico → ok
  if(is.numeric(x) || is.integer(x)){
    m <- matrix(x, ncol=1); colnames(m) <- var; return(m)
  }
  
  # factor/char → dummies
  if(is.factor(x) || is.character(x)){
    tmp <- data.frame(tmp=x)
    mm <- model.matrix(~ tmp - 1, tmp, na.action=na.pass)
    colnames(mm) <- make.names(paste0(var,"_",colnames(mm)))
    return(mm)
  }
  
  # lógico
  if(is.logical(x)){
    m <- matrix(as.numeric(x), ncol=1); colnames(m) <- var; return(m)
  }
  
  # cualquier otra cosa → convertir a numeric
  tmp <- suppressWarnings(as.numeric(x))
  tmp[is.na(tmp)] <- 0
  m <- matrix(tmp, ncol=1); colnames(m) <- var
  return(m)
}

# ==================================================================================
# 9. CONSTRUIR X_full
# ==================================================================================
blocks <- list()
bad <- c()

for(v in predictors_final){
  blk <- tryCatch(build_block(v, full), error=function(e) NULL)
  if(is.null(blk) || nrow(blk)!=nrow(full)){
    bad <- c(bad, v)
    next
  }
  blocks[[v]] <- blk
}

if(length(bad)>0){
  cat("Variables eliminadas:", paste(bad,collapse=", "), "\n")
}

X_full <- do.call(cbind, blocks)
cat("X_full:", dim(X_full)[1],"x",dim(X_full)[2],"\n")

# Separar
X_train <- X_full[full$.is_test==0,,drop=FALSE]
X_test  <- X_full[full$.is_test==1,,drop=FALSE]
y_train <- full$price[full$.is_test==0]

# ==================================================================================
# 10. HOLDOUT PARA MÉTRICAS
# ==================================================================================
set.seed(123)
idx <- sample(seq_along(y_train), floor(0.2*length(y_train)))
X_tr <- X_train[-idx,,drop=FALSE]
X_val<- X_train[idx,,drop=FALSE]
y_tr <- y_train[-idx]
y_val<- y_train[idx]

metricas <- function(real, pred){
  data.frame(
    RMSE = sqrt(mean((real-pred)^2)),
    MAE  = mean(abs(real-pred)),
    R2   = 1 - sum((real-pred)^2)/sum((real-mean(real))^2),
    MAPE = mean(abs((real-pred)/real))*100
  )
}

to_dmatrix <- function(X,y=NULL){
  if(is.null(y)) return(xgb.DMatrix(X))
  xgb.DMatrix(data=X,label=y)
}

guardar_submission <- function(property_id, pred, fname){
  out <- data.frame(property_id=property_id, price=pred)
  write.csv(out, file.path(Sys.getenv("USERPROFILE"),"Downloads",fname), row.names=FALSE)
  cat("Guardado:", fname, "\n")
}

# ==================================================================================
# 11. DEFINICIÓN DE VARIANTES
# ==================================================================================
variants <- list()

# ================================
# (1) XGB BASE
# ================================
variants[[1]] <- list(
  name="xgb_base",
  feature_func=function(X) X,
  params=list(booster="gbtree",objective="reg:squarederror",eval_metric="rmse",
              eta=0.05,max_depth=10,subsample=0.8,colsample_bytree=0.8),
  nrounds_max=2000,
  early_stop_rounds=80
)

# ================================
# (2) XGB INTERACTIONS
# ================================
variants[[2]] <- list(
  name="xgb_interactions",
  feature_func=function(X){
    df <- as.data.frame(X)
    if("area_final" %in% colnames(df) && "bedrooms" %in% colnames(df))
      df$areaXrooms <- df$area_final * df$bedrooms
    as.matrix(df)
  },
  params=variants[[1]]$params,
  nrounds_max=2000, early_stop_rounds=80
)

# ================================
# (3) XGB SPATIAL MEAN UPZ (BASE)
# ================================
variants[[3]] <- list(
  name="xgb_spatial_mean_upz",
  feature_func=function(X){
    df <- as.data.frame(X)
    if("upz" %in% colnames(df)){
      df$upz_num <- as.integer(as.factor(df$upz))
    } else df$upz_num <- 0
    as.matrix(df)
  },
  params=variants[[1]]$params,
  nrounds_max=1500, early_stop_rounds=80
)

# ================================
# (4) XGB REGULARIZED
# ================================
variants[[4]] <- list(
  name="xgb_regularized",
  feature_func=function(X) X,
  params=list(booster="gbtree",objective="reg:squarederror",
              eval_metric="rmse",eta=0.03,max_depth=12,
              lambda=3,alpha=1,subsample=0.75,colsample_bytree=0.7),
  nrounds_max=2500, early_stop_rounds=120
)

# ================================
# (5) XGB LOG TARGET
# ================================
variants[[5]] <- list(
  name="xgb_logtarget",
  feature_func=function(X) X,
  params=variants[[1]]$params,
  use_log_target=TRUE,
  nrounds_max=2000, early_stop_rounds=100
)

