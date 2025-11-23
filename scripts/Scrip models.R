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

# ==================================================================================
# 12. NUEVA VARIANTE (CORREGIDA) — xgb_crime_density2
# ==================================================================================
variants[[6]] <- list(
  name="xgb_crime_density2",
  feature_func=function(X){
    
    df <- as.data.frame(X)
    
    df$idx_global <- seq_len(nrow(df))
    
    if("LocNombre" %in% colnames(df)) df$idx_loc <- as.integer(as.factor(df$LocNombre)) else df$idx_loc <- 0
    if("upz" %in% colnames(df))       df$idx_upz <- as.integer(as.factor(df$upz))       else df$idx_upz <- 0
    
    safe_num <- function(v){
      v <- suppressWarnings(as.numeric(v))
      if(is.null(v)) return(rep(0,nrow(df)))
      v[is.na(v)] <- 0
      return(v)
    }
    
    hom  <- safe_num(df$num_homicidios_anual)
    hur  <- safe_num(df$num_hurto_re_anual) + safe_num(df$num_hurto_pe_anual)
    area <- safe_num(df$area_final); area[area<=0] <- 1
    
    df$density_hom  <- hom/area
    df$density_hurt <- hur/area
    df$log_hom      <- log1p(hom)
    df$log_hurt     <- log1p(hur)
    df$sqrt_hurt    <- sqrt(hur)
    
    df <- df %>% mutate(across(everything(), function(x){
      if(is.factor(x)) return(as.numeric(x))
      if(is.character(x)){
        z <- suppressWarnings(as.numeric(x))
        z[is.na(z)] <- 0; return(z)
      }
      if(is.logical(x)) return(as.numeric(x))
      z <- suppressWarnings(as.numeric(x)); z[is.na(z)] <- 0; return(z)
    }))
    
    df[is.na(df)] <- 0
    as.matrix(df)
  },
  
  params=list(
    booster="gbtree",
    objective="reg:squarederror",
    eval_metric="rmse",
    eta=0.03,
    max_depth=9,
    subsample=0.8,
    colsample_bytree=0.8,
    min_child_weight=2,
    lambda=1.0,
    alpha=0.2
  ),
  
  nrounds_max=2000,
  early_stop_rounds=100
)

# ==================================================================================
# 12 BIS. NUEVA VARIANTE — RED NEURONAL (MLP)
# ==================================================================================

library(nnet)

variants[[7]] <- list(
  name = "neural_network_mlp",
  
  # Usa exactamente las mismas features numéricas que X_full
  feature_func = function(X){
    df <- as.data.frame(X)
    
    # asegurar que TODO sea numérico
    df <- df %>% mutate(across(everything(), function(z){
      z2 <- suppressWarnings(as.numeric(z))
      z2[is.na(z2)] <- 0
      return(z2)
    }))
    
    as.matrix(df)
  },
  
  # hiperparámetros de red neuronal
  params = list(
    size = 20,          # número de neuronas en la capa oculta
    decay = 0.001,      # regularización L2
    maxit = 300         # iteraciones
  ),
  
  is_neural_net = TRUE  # flag especial para detectarlo
)

# ==================================================================================
# 13. ENTRENAMIENTO DE LAS VARIANTES
# ==================================================================================
results_list <- list()
preds_test_list <- list()

for(i in seq_along(variants)){
  
  v <- variants[[i]]
  cat("\n\n==============================")
  cat("\nENTRENANDO:", v$name, " (", i,"/",length(variants),")\n")
  cat("==============================\n")
  
  Xvar     <- v$feature_func(X_full)
  Xtrain_v <- Xvar[full$.is_test==0,,drop=FALSE]
  Xtest_v  <- Xvar[full$.is_test==1,,drop=FALSE]
  
  use_log <- isTRUE(v$use_log_target)
  y_tr_xgb  <- if(use_log) log1p(y_tr)  else y_tr
  y_val_xgb <- if(use_log) log1p(y_val) else y_val
  y_full_xgb<- if(use_log) log1p(y_train) else y_train
  
  dtrain_cv <- to_dmatrix(Xtrain_v, y_full_xgb)
  
  cvres <- tryCatch(
    xgb.cv(
      params=v$params,
      data=dtrain_cv,
      nrounds=v$nrounds_max,
      nfold=5,
      early_stopping_rounds=v$early_stop_rounds,
      verbose=0
    ),
    error=function(e) NULL
  )
  
  best_nrounds <- if(!is.null(cvres)) cvres$best_iteration else floor(v$nrounds_max/2)
  
  cat("Best nrounds:", best_nrounds, "\n")
  
  dtrain_tr <- to_dmatrix(X_tr, y_tr_xgb)
  dval_tr   <- to_dmatrix(X_val, y_val_xgb)
  # ===========================================================
  # SI LA VARIANTE ES RED NEURONAL
  # ===========================================================
  if(!is.null(v$is_neural_net) && v$is_neural_net){
    
    cat("\n---- ENTRENANDO RED NEURONAL MLP ----\n")
    
    Xvar     <- v$feature_func(X_full)
    Xtrain_v <- Xvar[full$.is_test==0,,drop=FALSE]
    Xtest_v  <- Xvar[full$.is_test==1,,drop=FALSE]
    
    # Holdout
    X_train_tr <- Xtrain_v[-idx,,drop=FALSE]
    X_train_val<- Xtrain_v[idx,,drop=FALSE]
    y_train_tr <- y_tr
    y_train_val<- y_val
    
    # Entrenamiento MLP
    set.seed(123)
    nn_model <- nnet(
      x = X_train_tr,
      y = y_train_tr,
      size  = v$params$size,
      decay = v$params$decay,
      maxit = v$params$maxit,
      linout = TRUE,
      trace = FALSE
    )
    
    # Validación
    pred_val <- predict(nn_model, X_train_val)
    mets <- metricas(y_val, pred_val)
    print(mets)
    
    # Reentrenar en TODO el train
    nn_full <- nnet(
      x = Xtrain_v,
      y = y_train,
      size  = v$params$size,
      decay = v$params$decay,
      maxit = v$params$maxit,
      linout = TRUE,
      trace = FALSE
    )
    
    pred_test <- as.numeric(predict(nn_full, Xtest_v))
    
    # Guardar resultados
    results_list[[v$name]] <- list(metrics_holdout=mets, best_nrounds=NA)
    preds_test_list[[v$name]] <- pred_test
    
    fname <- paste0("submission_",v$name,".csv")
    guardar_submission(test2$property_id, pred_test, fname)
    
    next  # IMPORTANTÍSIMO → saltarse la parte XGBoost
  }
  
  model_xgb <- xgb.train(
    params=v$params,
    data=dtrain_tr,
    nrounds=best_nrounds,
    watchlist=list(train=dtrain_tr, eval=dval_tr),
    verbose=0
  )
  
  pred_val_raw <- predict(model_xgb, dval_tr)
  pred_val <- if(use_log) expm1(pred_val_raw) else pred_val_raw
  
  mets <- metricas(y_val, pred_val)
  print(mets)
  
  dtrain_all <- to_dmatrix(Xtrain_v, y_full_xgb)
  model_full <- xgb.train(params=v$params, data=dtrain_all, nrounds=best_nrounds, verbose=0)
  
  pred_test_raw <- predict(model_full, to_dmatrix(Xtest_v))
  pred_test <- if(use_log) expm1(pred_test_raw) else pred_test_raw
  
  results_list[[v$name]] <- list(metrics_holdout=mets, best_nrounds=best_nrounds)
  preds_test_list[[v$name]] <- pred_test
  
  # guardar submission
  fname <- paste0("submission_",v$name,".csv")
  guardar_submission(test2$property_id, pred_test, fname)
}


# ==================================================================================
# 14. TABLA COMPARATIVA
# ==================================================================================
summary_table <- lapply(names(results_list), function(nm){
  res <- results_list[[nm]]
  data.frame(
    model=nm,
    RMSE=res$metrics_holdout$RMSE,
    MAE =res$metrics_holdout$MAE,
    R2  =res$metrics_holdout$R2,
    MAPE=res$metrics_holdout$MAPE,
    nrounds=res$best_nrounds
  )
}) %>% bind_rows()

cat("\n===== COMPARATIVA FINAL (HOLDOUT) =====\n")
print(summary_table)


# segunda parte

# =============================================================
# SCRIPT FINAL COMPLETO — 3 MODELOS — SIN PERDIDA DE FILAS
# Modelo 1 reemplazado: CART (árbol de regresión con rpart)
# =============================================================

library(tidyverse)
library(glmnet)
library(ranger)
library(caret)
library(rpart)      # <--- nuevo
library(rpart.plot) # opcional para visualizar


# =============================================================
# 1. CARGA DE DATOS
# =============================================================
train <- readRDS("C:/Users/user}/Downloads/df_def_entrenamiento.rds")
test  <- readRDS("C:/Users/user}/Downloads/df_def_testeo.rds")

cat("\nFilas train:", nrow(train), " | Filas test:", nrow(test), "\n")

# =============================================================
# 2. VARIABLES ECONÓMICAMENTE RELEVANTES
# =============================================================
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

# =============================================================
# 3. CREAR AREA_FINAL
# =============================================================
train2 <- train2 %>% mutate(area_final = coalesce(as.numeric(area_m2), as.numeric(surface_covered)))
test2  <- test2  %>% mutate(area_final = coalesce(as.numeric(area_m2), as.numeric(surface_covered)))

predictors <- c("area_final", setdiff(predictors, c("area_m2","surface_covered")))

# =============================================================
# 4. IMPUTACIÓN NUMÉRICA (MEDIANA)
# =============================================================
num_vars <- train2 %>% select(-property_id, -price) %>% select(where(is.numeric)) %>% colnames()

for(v in num_vars){
  med <- median(train2[[v]], na.rm = TRUE)
  if(is.na(med)) med <- 0
  train2[[v]][is.na(train2[[v]])] <- med
  if(!(v %in% colnames(test2))) test2[[v]] <- NA_real_
  test2[[v]][is.na(test2[[v]])] <- med
}

# =============================================================
# 5. FACTORES CONSISTENTES ENTRE TRAIN Y TEST
# =============================================================
char_cols <- train2 %>% select(-property_id, -price) %>% select(where(is.character)) %>% colnames()

for(v in char_cols) train2[[v]] <- as.factor(train2[[v]])

for(v in char_cols){
  if(!(v %in% colnames(test2))) test2[[v]] <- NA
  test2[[v]] <- factor(test2[[v]], levels = levels(train2[[v]]))
}

# NA en factores -> OTHER (si aparecen)
factor_vars <- train2 %>% select(-property_id, -price) %>% select(where(is.factor)) %>% colnames()

for(v in factor_vars){
  if(v %in% colnames(test2) && any(is.na(test2[[v]]))){
    levels(train2[[v]]) <- unique(c(levels(train2[[v]]), "OTHER"))
    train2[[v]][is.na(train2[[v]])] <- "OTHER"
    test2[[v]][is.na(test2[[v]])]  <- "OTHER"
  }
}

# =============================================================
# 6. UNIR FULL
# =============================================================
test2$price <- NA_real_
train2$.is_test <- 0
test2$.is_test  <- 1
full <- bind_rows(train2, test2)

cat("FULL filas:", nrow(full), "\n")

# =============================================================
# 7. LISTA FINAL DE PREDICTORAS
# =============================================================
predictors_final <- setdiff(colnames(full), c("property_id","price",".is_test"))

# =============================================================
# 8. FUNCIÓN PARA HACER MATRICES POR COLUMNA (ROBUSTO)
# =============================================================
build_block <- function(varname, df_full) {
  v <- df_full[[varname]]
  n <- nrow(df_full)
  
  if(is.numeric(v) || is.integer(v)) {
    m <- matrix(v, ncol=1)
    colnames(m) <- varname
    return(m)
  }
  
  if(is.factor(v) || is.character(v)) {
    tmp <- data.frame(tmpvar = v)
    mm <- model.matrix(~ tmpvar -1, data=tmp, na.action=na.pass)
    colnames(mm) <- make.names(paste0(varname,"_",colnames(mm)))
    return(mm)
  }
  
  if(is.logical(v)) {
    m <- matrix(as.numeric(v), ncol=1)
    colnames(m) <- varname
    return(m)
  }
  
  tmp <- data.frame(tmpvar = as.character(v))
  mm <- model.matrix(~ tmpvar -1, data=tmp, na.action=na.pass)
  colnames(mm) <- make.names(paste0(varname,"_",colnames(mm)))
  return(mm)
}

# =============================================================
# 9. CONSTRUIR X_full BLOQUE A BLOQUE
# =============================================================
blocks <- list()
errors <- list()

for(var in predictors_final){
  blk <- tryCatch(build_block(var, full), error=function(e) e)
  
  if(inherits(blk,"error") || nrow(blk)!=nrow(full)){
    errors[[var]] <- TRUE
    next
  }
  
  blocks[[var]] <- blk
}

if(length(errors)>0){
  bad <- names(errors)
  predictors_final <- setdiff(predictors_final, bad)
  for(b in bad) blocks[[b]] <- NULL
  message("Variables eliminadas por incompatibilidad: ", paste(bad, collapse=", "))
}

# =============================================================
# 10. UNIR MATRIZ FINAL
# =============================================================
X_full <- do.call(cbind, blocks)
cat("X_full dims:", nrow(X_full), "x", ncol(X_full), "\n")

# Separación train-test
X_train <- X_full[full$.is_test == 0, , drop=FALSE]
X_test  <- X_full[full$.is_test == 1, , drop=FALSE]
y_train <- full$price[full$.is_test == 0]

cat("X_train:", dim(X_train), " | X_test:", dim(X_test), "\n")

# =============================================================
# 11. MODELO 1 — CART (ÁRBOL DE REGRESIÓN)
# =============================================================
df_cart <- data.frame(price = y_train, X_train)

modelo_cart <- rpart(
  price ~ .,
  data = df_cart,
  method = "anova",
  control = rpart.control(cp = 0.001, minsplit = 20)
)

pred_cart <- predict(modelo_cart, newdata = as.data.frame(X_test))
pred_cart[is.na(pred_cart)] <- median(y_train)

# =============================================================
# 12. MODELO 2 — ELASTIC NET
# =============================================================
cv_en <- cv.glmnet(X_train, y_train, alpha=0.5)
modelo_en <- cv_en$glmnet.fit
pred_en <- predict(modelo_en, s=cv_en$lambda.min, newx=X_test)
pred_en <- as.numeric(pred_en)

# =============================================================
# 13. MODELO 3 — RANDOM FOREST
# =============================================================
df_rf <- data.frame(price = y_train, X_train)
rf_model <- ranger(price ~ ., data = df_rf, num.trees=500)
pred_rf <- predict(rf_model, data=X_test)$predictions

# =============================================================
# 14. LISTO — PREDICCIONES
# =============================================================
cat("\nPredicciones generadas:\n")
cat("CART:", length(pred_cart), "\n")
cat("EN:  ", length(pred_en), "\n")
cat("RF:  ", length(pred_rf), "\n")

predicciones_finales <- data.frame(
  property_id = test$property_id,
  pred_cart = pred_cart,
  pred_elasticnet = pred_en,
  pred_rf = pred_rf
)

cat("\nLISTO. Script completo ejecutado sin errores.\n")




# =============================================================
# MÉTRICAS DE REGRESIÓN PARA COMPARAR MODELOS
# =============================================================

library(Metrics)

calc_metrics <- function(real, pred){
  data.frame(
    RMSE = rmse(real, pred),
    MAE  = mae(real, pred),
    R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2),
    MAPE = mape(real, pred)
  )
}

# Usamos VALIDACIÓN INTERNA con un holdout interno:
set.seed(123)
idx <- sample(1:length(y_train), size = floor(0.2 * length(y_train)))
y_val  <- y_train[idx]

X_val  <- X_train[idx, , drop=FALSE]
X_tr   <- X_train[-idx, , drop=FALSE]
y_tr   <- y_train[-idx]

# Reentrenar modelos en X_tr
# 1) CART
df_cart_tr <- data.frame(price=y_tr, X_tr)
cart_tr <- rpart(price ~ ., data=df_cart_tr)
pred_cart_val <- predict(cart_tr, newdata = as.data.frame(X_val))

# 2) Elastic Net
cv_en_tr <- cv.glmnet(X_tr, y_tr, alpha=0.5)
pred_en_val <- predict(cv_en_tr$glmnet.fit, newx=X_val, s=cv_en_tr$lambda.min)

# 3) Random Forest
df_rf_tr <- data.frame(price=y_tr, X_tr)
rf_tr <- ranger(price ~ ., data=df_rf_tr)
pred_rf_val <- predict(rf_tr, data=X_val)$predictions

# MÉTRICAS
metrics_cart <- calc_metrics(y_val, pred_cart_val)
metrics_en   <- calc_metrics(y_val, as.numeric(pred_en_val))
metrics_rf   <- calc_metrics(y_val, pred_rf_val)

cat("\n================== MÉTRICAS CART ==================\n")
print(metrics_cart)

cat("\n================== MÉTRICAS ELASTIC NET ==================\n")
print(metrics_en)

cat("\n================== MÉTRICAS RANDOM FOREST ==================\n")
print(metrics_rf)




# =============================================================
# SCRIPT PARA GENERAR ARCHIVOS DE SUBMISSION PARA KAGGLE
# Compatible con: Elastic Net, Random Forest, CART, XGBoost, etc.
# =============================================================
# =============================================================
# 0. DETECTAR LA CARPETA DOWNLOADS REAL DEL USUARIO
# =============================================================

posibles <- c(
  file.path(Sys.getenv("USERPROFILE"), "Downloads"),
  file.path(Sys.getenv("USERPROFILE"), "Descargas"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Downloads"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Descargas"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documentos", "Downloads"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documentos", "Descargas")
)

downloads_path <- posibles[file.exists(posibles)][1]

if(is.na(downloads_path)){
  stop("No se encontró una carpeta válida de Downloads en este sistema.")
}

cat("\nCarpeta de Downloads detectada:\n", downloads_path, "\n\n")


# =============================================================
# 1. Función general para crear un archivo submission
# =============================================================

crear_submission <- function(property_id, pred, nombre_archivo){
  
  ruta <- file.path(downloads_path, nombre_archivo)
  
  submission <- data.frame(
    property_id = property_id,
    price = pred
  )
  
  write.csv(submission, ruta, row.names = FALSE)
  
  cat("\nArchivo generado en:\n", ruta, "\n")
}


# =============================================================
# 2. Crear SUBMISSION Elastic Net
# =============================================================

if(exists("pred_en")){
  crear_submission(test$property_id, pred_en, "submission_elasticnet.csv")
} else {
  cat("pred_en no existe en memoria — no se generó submission_elasticnet.csv\n")
}

# =============================================================
# 3. Crear SUBMISSION Random Forest
# =============================================================

if(exists("pred_rf")){
  crear_submission(test$property_id, pred_rf, "submission_randomforest.csv")
} else {
  cat("pred_rf no existe en memoria — no se generó submission_randomforest.csv\n")
}

# =============================================================
# 4. Crear SUBMISSION CART
# =============================================================

if(exists("pred_cart")){
  crear_submission(test$property_id, pred_cart, "submission_cart.csv")
} else {
  cat("pred_cart no existe en memoria — no se generó submission_cart.csv\n")
}

# =============================================================
# 5. Crear SUBMISSION XGBoost
# =============================================================

if(exists("pred_xgb")){
  crear_submission(test$property_id, pred_xgb, "submission_xgboost.csv")
} else {
  cat("pred_xgb no existe en memoria — no se generó submission_xgboost.csv\n")
}

# =============================================================
cat("\n===========================================\n")
cat("TODOS LOS ARCHIVOS DE SUBMISSION HAN SIDO GENERADOS\n")
cat("===========================================\n\n")

# =============================================================
# SCRIPT: 5 VARIANTES XGBOOST (entrenamiento, métricas, submissions)
# Requisitos: haber ejecutado antes el pipeline que crea:
# X_train, X_test, y_train, full, predictors_final, train2, test2
# =============================================================

library(xgboost)
library(dplyr)
library(Metrics)
library(Matrix)

# -------------------------
# 0) Detectar carpeta Downloads (misma lógica que usaste)
# -------------------------
posibles <- c(
  file.path(Sys.getenv("USERPROFILE"), "Downloads"),
  file.path(Sys.getenv("USERPROFILE"), "Descargas"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Downloads"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Descargas"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documentos", "Downloads"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documentos", "Descargas")
)
downloads_path <- posibles[file.exists(posibles)][1]
if(is.na(downloads_path)) stop("No se encontró carpeta Downloads válida en este sistema.")
cat("Downloads detectada:", downloads_path, "\n")

# -------------------------
# 1) Funciones auxiliares
# -------------------------
metricas <- function(real, pred){
  rmse_v <- sqrt(mean((real - pred)^2, na.rm=TRUE))
  mae_v  <- mean(abs(real - pred), na.rm=TRUE)
  r2_v   <- 1 - sum((real - pred)^2, na.rm=TRUE)/sum((real - mean(real))^2, na.rm=TRUE)
  mape_v <- mean(abs((real - pred)/real), na.rm=TRUE) * 100
  data.frame(RMSE = rmse_v, MAE = mae_v, R2 = r2_v, MAPE = mape_v)
}

# función segura para guardar submission y chequear filas
guardar_submission <- function(property_id, pred, filename){
  ruta <- file.path(downloads_path, filename)
  if(length(pred) != length(property_id)){
    stop("ERROR: longitud de predicciones (", length(pred), ") no coincide con property_id (", length(property_id), ")")
  }
  write.csv(data.frame(property_id=property_id, price=pred), ruta, row.names = FALSE)
  cat("Guardado:", ruta, "\n")
}

# Freeze seed
set.seed(12345)

# -------------------------
# 2) Preparar holdout interno (20%) para comparar métricas
# -------------------------
n <- length(y_train)
idx_holdout <- sample(1:n, size = floor(0.2 * n))
X_val <- X_train[idx_holdout, , drop = FALSE]
y_val <- y_train[idx_holdout]
X_tr  <- X_train[-idx_holdout, , drop = FALSE]
y_tr  <- y_train[-idx_holdout]

# Convertir a xgb.DMatrix: usamos matrices densas -> convertir a dgCMatrix (sparse) por velocidad
to_dmatrix <- function(X, y = NULL){
  if(inherits(X, "dgCMatrix")){
    if(is.null(y)) return(xgb.DMatrix(data = X))
    return(xgb.DMatrix(data = X, label = y))
  }
  Xs <- Matrix(as.matrix(X), sparse = TRUE)
  if(is.null(y)) return(xgb.DMatrix(data = Xs))
  return(xgb.DMatrix(data = Xs, label = y))
}

dtrain_full <- to_dmatrix(X_train, y_train)
dtrain_hold <- to_dmatrix(X_tr, y_tr)
dval_hold   <- to_dmatrix(X_val, y_val)
dtest       <- to_dmatrix(X_test)

# -------------------------
# 3) Definición de 5 variantes XGBoost
# Cada lista incluye:
# - nombre
# - función para crear features (si aplica)
# - params iniciales (list)
# - nrounds_cv (determinado via xgb.cv con early stopping)
# -------------------------

variants <- list()

# VARIANT 1: xgb_base (todas las variables tal cual)
variants[[1]] <- list(
  name = "xgb_base",
  feature_func = function(X_full) X_full,
  params = list(objective = "reg:squarederror", eta=0.05, max_depth=6, subsample=0.8, colsample_bytree=0.8, min_child_weight=10),
  early_stop_rounds = 30,
  nrounds_max = 1000
)

# VARIANT 2: xgb_interactions (agrega interacciones económicas recomendadas)
variants[[2]] <- list(
  name = "xgb_interactions",
  feature_func = function(X_full){
    Xf <- as.data.frame(X_full)
    # crear razones y log transforms
    if("area_final" %in% colnames(Xf)){
      Xf$log_area <- log1p(Xf$area_final)
      # densidad de habitaciones
      if("bedrooms" %in% colnames(Xf)) Xf$bedroom_density <- Xf$bedrooms / pmax(1, Xf$area_final)
    }
    if("estrato" %in% colnames(Xf)){
      # estrato como factor dummified is already in X_full; keep numeric transformation too
      Xf$estrato_num <- as.numeric(as.character(ifelse(is.na(Xf$estrato), 0, Xf$estrato)))
    }
    # interaction area * estrato (numeric)
    if(all(c("area_final","estrato_num") %in% colnames(Xf))){
      Xf$area_estrato <- Xf$area_final * Xf$estrato_num
    }
    Matrix::Matrix(as.matrix(Xf), sparse = TRUE)
  },
  params = list(objective = "reg:squarederror", eta=0.03, max_depth=7, subsample=0.8, colsample_bytree=0.7, min_child_weight=8),
  early_stop_rounds = 40,
  nrounds_max = 1500
)

# VARIANT 3: xgb_spatial (incluye promedio objetivo por UPZ (train mean) como feature)
variants[[3]] <- list(
  name = "xgb_spatial_mean_upz",
  feature_func = function(X_full){
    # X_full: matrix with full rows order matching 'full'
    Xf <- as.data.frame(X_full)
    # compute train UPZ means from train2 (use y_train and full$.is_test)
    # we assume object 'full' and 'train2' exist in env
    upz_train <- full$upz[full$.is_test == 0]
    train_idx <- which(full$.is_test == 0)
    # Calculate mean price per upz on training data
    upz_means <- tapply(full$price[train_idx], upz_train, mean, na.rm=TRUE)
    # Map means to all rows (for UPZ values not in train, assign global mean)
    upz_all <- as.character(full$upz)
    mean_global <- mean(full$price[train_idx], na.rm=TRUE)
    upz_feature <- sapply(upz_all, function(u) if(!is.na(u) && u %in% names(upz_means)) upz_means[[u]] else mean_global)
    # attach as column (will match full row order)
    Xf$upz_price_mean_train <- upz_feature
    Matrix::Matrix(as.matrix(Xf), sparse = TRUE)
  },
  params = list(objective = "reg:squarederror", eta=0.04, max_depth=6, subsample=0.85, colsample_bytree=0.8, min_child_weight=5),
  early_stop_rounds = 30,
  nrounds_max = 1200
)

# VARIANT 4: xgb_regularized (más regularización: lambda/alpha)
variants[[4]] <- list(
  name = "xgb_regularized",
  feature_func = function(X_full) X_full,
  params = list(objective = "reg:squarederror", eta=0.02, max_depth=8, subsample=0.9, colsample_bytree=0.6, min_child_weight=5, lambda=2, alpha=1),
  early_stop_rounds = 50,
  nrounds_max = 2000
)

# VARIANT 5: xgb_logtarget (predecir log(price) para reducir sesgo)
variants[[5]] <- list(
  name = "xgb_logtarget",
  feature_func = function(X_full) X_full,
  params = list(objective = "reg:squarederror", eta=0.03, max_depth=6, subsample=0.8, colsample_bytree=0.8, min_child_weight=8),
  early_stop_rounds = 30,
  nrounds_max = 1500,
  use_log_target = TRUE
)

# -------------------------
# 4) Loop para entrenar las 5 variantes
# -------------------------
results_list <- list()
preds_test_list <- list()

for(i in seq_along(variants)){
  v <- variants[[i]]
  cat("\n\n==============================\nVariant:", v$name, " (", i, "of", length(variants), ")\n==============================\n")
  
  # 1) crear features para train/val/test (aplicable a full order)
  Xfull_variant <- v$feature_func(X_full)   # debe devolver matrix-like (sparse ok)
  # separar
  Xtrain_var <- Xfull_variant[full$.is_test == 0, , drop = FALSE]
  Xtest_var  <- Xfull_variant[full$.is_test == 1, , drop = FALSE]
  
  # target: si use_log_target, transformar y controlar
  use_log <- ifelse(isTRUE(v$use_log_target), TRUE, FALSE)
  if(use_log){
    y_tr_xgb <- log1p(y_tr)
    y_val_xgb <- log1p(y_val)
    y_full_xgb <- log1p(y_train)
  } else {
    y_tr_xgb <- y_tr
    y_val_xgb <- y_val
    y_full_xgb <- y_train
  }
  
  # 2) crear DMatrix para cv: usamos full training (Xtrain_var)
  dtrain_cv <- to_dmatrix(Xtrain_var, y_full_xgb)  # full train used for cv
  # 3) xgb.cv para elegir nrounds (usamos early stopping)
  cvres <- tryCatch({
    xgb.cv(
      params = v$params,
      data = dtrain_cv,
      nrounds = v$nrounds_max,
      nfold = 5,
      early_stopping_rounds = v$early_stop_rounds,
      verbose = 0,
      maximize = FALSE,
      showsd = TRUE,
      prediction = FALSE
    )
  }, error = function(e){
    cat("xgb.cv fallo para", v$name, "-> usando nrounds = 100\n")
    return(NULL)
  })
  
  if(!is.null(cvres)){
    best_nrounds <- cvres$best_iteration
    cat("Mejor nrounds (cv):", best_nrounds, "\n")
  } else {
    best_nrounds <- min(200, v$nrounds_max)
    cat("Se asignó nrounds fallback:", best_nrounds, "\n")
  }
  
  # 4) entrenar con el holdout TRAIN (X_tr, y_tr) para comparar métricas en holdout
  dtrain_tr <- to_dmatrix(X_tr, if(use_log) log1p(y_tr) else y_tr)
  dval_tr   <- to_dmatrix(X_val, if(use_log) log1p(y_val) else y_val)
  watchlist <- list(train = dtrain_tr, eval = dval_tr)
  
  model_xgb <- xgb.train(
    params = v$params,
    data = dtrain_tr,
    nrounds = best_nrounds,
    watchlist = watchlist,
    verbose = 0
  )
  
  # 5) predecir sobre holdout y calcular métricas (en escala original si log target)
  pred_val_raw <- predict(model_xgb, dval_tr)
  pred_val <- if(use_log) (expm1(pred_val_raw)) else pred_val_raw
  metrics_holdout <- metricas(y_val, pred_val)
  cat("Métricas holdout (", v$name, "):\n"); print(metrics_holdout)
  
  # 6) reentrenar sobre TODO training (full) con best_nrounds
  dtrain_all <- to_dmatrix(Xtrain_var, if(use_log) log1p(y_train) else y_train)
  model_xgb_full <- xgb.train(params = v$params, data = dtrain_all, nrounds = best_nrounds, verbose = 0)
  
  # 7) predecir sobre X_test
  pred_test_raw <- predict(model_xgb_full, to_dmatrix(Xtest_var))
  pred_test <- if(use_log) (expm1(pred_test_raw)) else pred_test_raw
  
  # 8) guardar resultados
  results_list[[v$name]] <- list(metrics_holdout = metrics_holdout, best_nrounds = best_nrounds)
  preds_test_list[[v$name]] <- pred_test
  
  # 9) guardar submission
  filename <- paste0("submission_", v$name, ".csv")
  # verificar filas
  if(length(pred_test) != nrow(test2)){
    warning("Número de predicciones (", length(pred_test), ") NO coincide con nrow(test2) (", nrow(test2), "). No se guardará submission.")
  } else {
    guardar_submission(test2$property_id, pred_test, filename)
  }
}

# -------------------------
# 5) Comparativa rápida de métricas (holdout) entre variantes
# -------------------------
summary_table <- lapply(names(results_list), function(nm){
  res <- results_list[[nm]]
  data.frame(model = nm, RMSE = res$metrics_holdout$RMSE, MAE = res$metrics_holdout$MAE, R2 = res$metrics_holdout$R2, MAPE = res$metrics_holdout$MAPE, nrounds = res$best_nrounds)
}) %>% bind_rows()

cat("\n===== Comparativa Holdout (variantes XGB) =====\n")
print(summary_table)

# -------------------------
# 6) Elegir mejor variante por RMSE (holdout)
# -------------------------
best_model_name <- summary_table$model[which.min(summary_table$RMSE)]
cat("Mejor variante por RMSE (holdout):", best_model_name, "\n")

