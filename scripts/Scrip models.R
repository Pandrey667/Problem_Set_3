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
