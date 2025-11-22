###############################################################
# 1) LIMPIEZA Y LIBRERÍAS
###############################################################

remove(list = ls())

library(dplyr)
library(readr)
library(Metrics)
library(Matrix)
library(sf)
library(forcats)
library(stringi)
library(xgboost)
library(future)
library(future.apply)

set.seed(123)






###############################################################
# 2) IMPORTACIÓN Y PREPARACIÓN
###############################################################

setwd("C:/Users/fevid/Documents/MEcA/Big Data/Taller_3/datos_taller_3")

raw_train <- readRDS("df_def_entrenamiento.rds")
raw_test  <- readRDS("df_def_testeo.rds")

if ("geometry" %in% names(raw_train)) raw_train <- sf::st_drop_geometry(raw_train)
if ("geometry" %in% names(raw_test))  raw_test  <- sf::st_drop_geometry(raw_test)

raw_test <- raw_test %>% distinct(property_id, .keep_all = TRUE)

safe_coalesce_area <- function(df) {
  vars <- intersect(c("area_reportada","surface_covered","area_m2"), names(df))
  stopifnot(length(vars) > 0)
  x <- df[[vars[1]]]
  if (length(vars) > 1)
    for (j in 2:length(vars))
      x <- dplyr::coalesce(x, df[[vars[j]]])
  df$area_m2_final <- x
  df
}

train_imp <- safe_coalesce_area(raw_train) %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year = factor(year)
  ) %>%
  select(-any_of("estrato"))

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





###############################################################
# 4) FEATURE ENGINEERING
###############################################################

fe_engineer <- function(df) {
  out <- df
  
  if ("area_m2_final" %in% names(out))
    out$log_area <- log1p(out$area_m2_final)
  
  if ("area_parque" %in% names(out))
    out$log_area_parque <- log1p(out$area_parque)
  
  dcols <- grep("^distancia_", names(out), value = TRUE)
  if (length(dcols) > 0) {
    out[paste0("log_", dcols)] <- lapply(out[dcols], log1p)
    out[dcols] <- NULL
  }
  
  if (all(c("area_m2_final","estrato_num") %in% names(out)))
    out$area_x_estrato <- out$area_m2_final * out$estrato_num
  
  if (all(c("bedrooms","banos_tot") %in% names(out)))
    out$bed_x_banos <- out$bedrooms * out$banos_tot
  
  if (all(c("lat","lon") %in% names(out)))
    out$lat_x_lon <- out$lat * out$lon
  
  out
}

train_imp <- fe_engineer(train_imp)
test_imp  <- fe_engineer(test_imp)

# Lumping LocNombre
if ("LocNombre" %in% names(train_imp)) {
  train_imp$LocNombre <- fct_lump_n(as.factor(train_imp$LocNombre), n = 20, other_level = "otros")
  test_imp$LocNombre  <- forcats::fct_explicit_na(as.factor(test_imp$LocNombre), na_level = "otros")
  test_imp$LocNombre  <- fct_other(test_imp$LocNombre, keep = levels(train_imp$LocNombre), other_level = "otros")
  test_imp$LocNombre  <- factor(test_imp$LocNombre, levels = levels(train_imp$LocNombre))
}

# Lumping UPZ
if ("upz" %in% names(train_imp)) {
  train_imp$upz <- fct_lump_n(as.factor(train_imp$upz), n = 30, other_level = "otros")
  test_imp$upz  <- forcats::fct_explicit_na(as.factor(test_imp$upz), na_level = "otros")
  test_imp$upz  <- fct_other(test_imp$upz, keep = levels(train_imp$upz), other_level = "otros")
  test_imp$upz  <- factor(test_imp$upz, levels = levels(train_imp$upz))
}






###############################################################
# 5) MATRICES DE MODELO (MODEL MATRIX)
###############################################################

protect_cols <- c(
  "price","property_id","title_norm","description","desc_norm",
  "city","geometry","barrio_oficial","operation_type"
)

pred_cols <- setdiff(names(train_imp), protect_cols)

build_mm <- function(train_df, test_df, pred_cols) {
  
  to_factor_safe <- function(df)
    df %>% mutate(
      across(where(is.character), as.factor),
      across(where(is.logical), as.factor),
      across(where(is.factor), ~ fct_explicit_na(.x, na_level = "NA"))
    )
  
  tr <- to_factor_safe(train_df)
  te <- to_factor_safe(test_df)
  
  for (col in intersect(names(tr), names(te))) {
    if (is.factor(tr[[col]]) && is.factor(te[[col]])) {
      lvls <- union(levels(tr[[col]]), levels(te[[col]]))
      tr[[col]] <- factor(tr[[col]], levels = lvls)
      te[[col]] <- factor(te[[col]], levels = lvls)
    }
  }
  
  is_constant_col <- function(x) {
    if (all(is.na(x))) return(TRUE)
    if (is.logical(x)) return(length(unique(x[!is.na(x)])) <= 1)
    if (is.numeric(x)) {
      nx <- x[is.finite(x)]
      return(length(nx) == 0 || var(nx) == 0)
    }
    if (is.factor(x)) return(nlevels(x) <= 1)
    if (is.character(x)) return(length(unique(x[!is.na(x)])) <= 1)
    FALSE
  }
  
  drop <- pred_cols[
    sapply(tr[pred_cols], function(x)
      (is.factor(x) && nlevels(x) < 2) || is_constant_col(x))
  ]
  
  pred_cols2 <- setdiff(pred_cols, drop)
  pred_cols2 <- intersect(pred_cols2, intersect(names(tr), names(te)))
  
  fmla <- as.formula(paste("~ 0 +", paste(pred_cols2, collapse = " + ")))
  
  Xt <- model.matrix(fmla, data = tr)
  Xv <- model.matrix(fmla, data = te)
  
  trn <- colnames(Xt)
  ten <- colnames(Xv)
  
  add <- setdiff(trn, ten)
  if (length(add) > 0) {
    add_mat <- matrix(0, nrow = nrow(Xv), ncol = length(add))
    colnames(add_mat) <- add
    Xv <- cbind(Xv, add_mat)
  }
  
  Xv <- Xv[, trn, drop = FALSE]
  
  list(
    X_train = Xt,
    X_test = Xv,
    pred_cols = pred_cols2,
    dropped = drop
  )
}

mm <- build_mm(train_imp, test_imp, pred_cols)

X_train_all <- mm$X_train
X_test_all  <- mm$X_test
pred_cols   <- mm$pred_cols

if (length(mm$dropped) > 0)
  message("Se eliminaron columnas problemáticas: ", paste(mm$dropped, collapse = ", "))






###############################################################
# 6) SUBCONJUNTO M2 Y TARGET
###############################################################

idx_m2 <- which(is.finite(train_imp$area_m2_final) & train_imp$area_m2_final > 0)

train_m2 <- train_imp[idx_m2, , drop = FALSE]
test_m2  <- test_imp %>%
  mutate(area_m2_final =
           ifelse(!is.finite(area_m2_final) | area_m2_final <= 0,
                  median(train_imp$area_m2_final, na.rm = TRUE),
                  area_m2_final))

X_train_m2 <- X_train_all[idx_m2, , drop = FALSE]
X_test_m2  <- X_test_all

y_m2 <- log1p(train_m2$price / train_m2$area_m2_final)



###############################################################
# 7) FOLDS ESPACIALES
###############################################################

make_group_folds <- function(df, group_col, v = 8, seed = 123) {
  set.seed(seed)
  g <- as.character(df[[group_col]])
  g[is.na(g) | g == "" ] <- paste0("NA", sample.int(v,
                                                    sum(is.na(df[[group_col]]) | df[[group_col]] == ""), replace = TRUE))
  sizes <- table(g)
  uniq <- names(sort(sizes, decreasing = TRUE))
  fold_assign <- integer(length(uniq))
  load <- rep(0, v)
  for (i in seq_along(uniq)) {
    k <- which.min(load)
    fold_assign[i] <- k
    load[k] <- load[k] + sizes[uniq[i]]
  }
  map <- setNames(fold_assign, uniq)
  as.integer(unname(map[g]))
}

make_spatial_or_random_folds <- function(df, v = 8, seed = 123) {
  set.seed(seed)
  if ("LocNombre" %in% names(df) && any(!is.na(df$LocNombre)))
    return(make_group_folds(df, "LocNombre", v, seed))
  if ("upz" %in% names(df) && any(!is.na(df$upz)))
    return(make_group_folds(df, "upz", v, seed))
  if (all(c("lon","lat") %in% names(df))) {
    coords_ok <- which(complete.cases(df$lon, df$lat))
    km <- stats::kmeans(df[coords_ok, c("lon","lat")], centers = v, nstart = 20)
    foldid <- rep(NA_integer_, nrow(df))
    foldid[coords_ok] <- km$cluster
    foldid[is.na(foldid)] <- sample.int(v, sum(is.na(foldid)), replace = TRUE)
    return(as.integer(foldid))
  }
  sample.int(v, nrow(df), replace = TRUE)
}

folds_id <- make_spatial_or_random_folds(train_m2, v = 8, seed = 42)
folds_list <- lapply(sort(unique(folds_id)), function(k) which(folds_id == k))




###############################################################
# 8) MÉTRICA Y HELPERS
###############################################################

feval_mae_logm2 <- function(preds, dtrain) {
  y <- getinfo(dtrain, "label")
  list(metric = "mae_logm2", value = mean(abs(preds - y)))
}

ratio_scale_price <- function(y_price, yhat_m2_log, area, clip = c(0.01, 0.99)) {
  yhat_price_raw <- area * (exp(yhat_m2_log) - 1)
  ok <- is.finite(y_price) & is.finite(yhat_price_raw) & yhat_price_raw > 0
  if (sum(ok) < 50) return(1)
  r <- y_price[ok] / yhat_price_raw[ok]
  q <- quantile(r, clip, na.rm = TRUE)
  r <- pmin(pmax(r, q[1]), q[2])
  s <- median(r, na.rm = TRUE)
  if (!is.finite(s) || s <= 0 || s > 50) s <- 1
  s
}

clip_price <- function(pred, y_train_price, clip = c(0.01, 0.99)) {
  q <- quantile(y_train_price[is.finite(y_train_price)], clip, na.rm = TRUE)
  pmin(pmax(pred, q[1]), q[2])
}





###############################################################
# 9) TUNING XGBOOST EN 2 ETAPAS
###############################################################

plan(multisession, workers = max(1, parallel::detectCores() - 1))

grid <- expand.grid(
  eta = c(0.03, 0.05, 0.1),
  max_depth = c(4, 6, 8),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.6, 0.8),
  colsample_bytree = c(0.6, 0.8),
  lambda = c(0, 1, 10),
  alpha = c(0, 1),
  KEEP.OUT.ATTRS = FALSE
)

set.seed(123)
grid <- grid[sample(nrow(grid), min(30, nrow(grid))), ]

cv_xgb_coarse <- function(par, nrounds, X, y, y_price, area,
                          nfold = 5, seed = 99) {
  set.seed(seed)
  dtrain_local <- xgb.DMatrix(data = X, label = y)
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = par$eta,
    max_depth = as.integer(round(par$max_depth)),
    min_child_weight = par$min_child_weight,
    subsample = par$subsample,
    colsample_bytree = par$colsample_bytree,
    lambda = par$lambda,
    alpha = par$alpha,
    nthread = 1
  )
  
  cv <- tryCatch({
    xgb.cv(
      params = params,
      data = dtrain_local,
      nrounds = nrounds,
      nfold = nfold,
      feval = feval_mae_logm2,
      maximize = FALSE,
      early_stopping_rounds = 50,
      verbose = 0,
      prediction = TRUE
    )
  }, error = function(e) NULL)
  
  if (is.null(cv)) {
    rm(dtrain_local); gc()
    return(list(
      mae = Inf,
      best_iter = NA,
      scale = 1,
      params = params,
      oof_logm2 = rep(NA_real_, nrow(X))
    ))
  }
  
  best_iter <- cv$best_iteration
  oof_logm2 <- cv$pred
  scale_glob <- ratio_scale_price(y_price, oof_logm2, area)
  oof_price <- clip_price(
    scale_glob * (area * (exp(oof_logm2) - 1)),
    y_price
  )
  mae_price <- Metrics::mae(y_price, oof_price)
  
  rm(dtrain_local); gc()
  
  list(
    mae = mae_price,
    best_iter = best_iter,
    scale = scale_glob,
    params = params,
    oof_logm2 = oof_logm2
  )
}

cv_xgb_refine <- function(par, nrounds, folds_list, X, y, y_price, area) {
  
  dtrain_local <- xgb.DMatrix(data = X, label = y)
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = par$eta,
    max_depth = as.integer(round(par$max_depth)),
    min_child_weight = par$min_child_weight,
    subsample = par$subsample,
    colsample_bytree = par$colsample_bytree,
    lambda = par$lambda,
    alpha = par$alpha,
    nthread = 1
  )
  
  cv <- tryCatch({
    xgb.cv(
      params = params,
      data = dtrain_local,
      nrounds = nrounds,
      folds = folds_list,
      feval = feval_mae_logm2,
      maximize = FALSE,
      early_stopping_rounds = 100,
      verbose = 0,
      prediction = TRUE
    )
  }, error = function(e) NULL)
  
  if (is.null(cv)) {
    rm(dtrain_local); gc()
    return(list(
      mae = Inf,
      best_iter = NA,
      scale = 1,
      params = params,
      oof_logm2 = rep(NA_real_, nrow(X))
    ))
  }
  
  best_iter <- cv$best_iteration
  oof_logm2 <- cv$pred
  
  scale_glob <- ratio_scale_price(y_price, oof_logm2, area)
  
  oof_price <- clip_price(
    scale_glob * (area * (exp(oof_logm2) - 1)),
    y_price
  )
  
  mae_price <- Metrics::mae(y_price, oof_price)
  
  rm(dtrain_local); gc()
  
  list(
    mae = mae_price,
    best_iter = best_iter,
    scale = scale_glob,
    params = params,
    oof_logm2 = oof_logm2
  )
}



###############################################################
# ETAPA 1: COARSE
###############################################################

coarse_nrounds <- 120

coarse_res <- future_lapply(
  seq_len(nrow(grid)),
  function(i)
    cv_xgb_coarse(
      grid[i, ], nrounds = coarse_nrounds,
      X = X_train_m2,
      y = y_m2,
      y_price = train_m2$price,
      area = train_m2$area_m2_final,
      nfold = 5,
      seed = 99 + i
    ),
  future.seed = TRUE,
  future.scheduling = 1
)

coarse_mae <- vapply(coarse_res, function(res) res$mae, numeric(1))
coarse_mae[!is.finite(coarse_mae)] <- Inf

top_k <- min(6, length(coarse_mae))
top_idx <- order(coarse_mae)[seq_len(top_k)]


###############################################################
# ETAPA 2: REFINE
###############################################################

refine_nrounds <- 1500

refine_res <- future_lapply(
  top_idx,
  function(i)
    cv_xgb_refine(
      grid[i, ],
      nrounds = refine_nrounds,
      folds_list = folds_list,
      X = X_train_m2,
      y = y_m2,
      y_price = train_m2$price,
      area = train_m2$area_m2_final
    ),
  future.seed = TRUE,
  future.scheduling = 1
)

refine_mae <- vapply(refine_res, function(res) res$mae, numeric(1))
refine_mae[!is.finite(refine_mae)] <- Inf

stopifnot(!all(is.infinite(refine_mae)))

best_ref <- refine_res[[which.min(refine_mae)]]

cat(
  "Best XGB (2-stage) OOF MAE =",
  round(best_ref$mae, 2),
  "| iter =", best_ref$best_iter, "\n"
)


###############################################################
# 10) ENTRENAMIENTO FINAL Y PREDICCIÓN
###############################################################

final <- xgb.train(
  params = best_ref$params,
  data = xgb.DMatrix(X_train_m2, label = y_m2),
  nrounds = best_ref$best_iter,
  verbose = 0
)

pred_m2_log_test <- predict(final, xgb.DMatrix(X_test_m2))

scale_price_m2 <- best_ref$scale

train_pred_price_oof <- clip_price(
  scale_price_m2 *
    (train_m2$area_m2_final * (exp(best_ref$oof_logm2) - 1)),
  train_m2$price
)

pred_price_test <- clip_price(
  scale_price_m2 *
    (test_m2$area_m2_final * (exp(pred_m2_log_test) - 1)),
  train_m2$price
)

mae_oof  <- Metrics::mae(train_m2$price, train_pred_price_oof)
rmse_oof <- Metrics::rmse(train_m2$price, train_pred_price_oof)

cat(
  "OOF MAE (training):", round(mae_oof, 2),
  " | OOF RMSE:", round(rmse_oof, 2), "\n"
)






###############################################################
# 11) CALIBRACIÓN POR GRUPO
###############################################################

group_vars <- c("estrato_num","property_type2","LocNombre","upz")
group_vars <- intersect(group_vars, names(train_m2))

if (length(group_vars) > 0) {
  
  train_m2 <- train_m2 %>%
    mutate(grp = interaction(across(all_of(group_vars)), drop = TRUE))
  
  test_m2 <- test_m2 %>%
    mutate(grp = interaction(across(all_of(group_vars)), drop = TRUE))
  
  scales_grp <- train_m2 %>%
    mutate(yhat_oof = train_pred_price_oof) %>%
    group_by(grp) %>%
    summarise(
      scale_grp = median(price / yhat_oof, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      scale_grp = ifelse(
        !is.finite(scale_grp) | scale_grp <= 0 | scale_grp > 50,
        1, scale_grp
      )
    )
  
  pred_price_test_grp <- test_m2 %>%
    select(property_id, grp, area_m2_final) %>%
    mutate(yhat_raw = area_m2_final * (exp(pred_m2_log_test) - 1)) %>%
    left_join(scales_grp, by = "grp") %>%
    mutate(scale_grp = coalesce(scale_grp, scale_price_m2),
           price = clip_price(scale_grp * yhat_raw, train_m2$price)) %>%
    pull(price)
  
} else {
  pred_price_test_grp <- pred_price_test
}


###############################################################
# 12) VALIDACIÓN INTERNA (CHAPINERO)
###############################################################

if ("LocNombre" %in% names(train_m2)) {
  is_ch <- tolower(trimws(as.character(train_m2$LocNombre))) == "chapinero"
  if (any(is_ch, na.rm = TRUE)) {
    mae_oof_ch <- Metrics::mae(
      train_m2$price[is_ch],
      train_pred_price_oof[is_ch]
    )
    cat("OOF MAE (Chapinero):", round(mae_oof_ch, 2), "\n")
  }
}









###############################################################
# 13) SUBMISSIONS
###############################################################

write_submission <- function(df_test, pred_vec, filename) {
  tbl <- df_test %>%
    select(property_id) %>%
    mutate(price = as.numeric(pred_vec)) %>%
    mutate(price = ifelse(is.finite(price), price, median(price, na.rm = TRUE)))
  
  subm <- raw_test %>%
    select(property_id) %>%
    left_join(tbl, by = "property_id")
  
  stopifnot(
    nrow(subm) == nrow(raw_test),
    all(!is.na(subm$price))
  )
  
  readr::write_csv(subm, filename)
}

write_submission(
  test_m2,
  pred_price_test,
  "submission_xgb_m2_global_ratio_clip_fast2stage.csv"
)

write_submission(
  test_m2,
  pred_price_test_grp,
  "submission_xgb_m2_groupCal_ratio_clip_fast2stage.csv"
)

cat("\nFilas test:", nrow(raw_test), "\n")
cat("Submissions generadas:\n",
    "- submission_xgb_m2_global_ratio_clip_fast2stage.csv\n",
    "- submission_xgb_m2_groupCal_ratio_clip_fast2stage.csv\n")


sort(mm$pred_cols)


colnames(X_train_m2)











# ===============================================================
# IMPORTANCIA Y GRÁFICAS TOP-10 (XGBOOST + SHAP)
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(xgboost)
  library(parallel)
})

set.seed(123)

# ---------------------------------------------------------------
# Comprobaciones
# ---------------------------------------------------------------
stopifnot(exists("final"), exists("X_train_m2"))
stopifnot(is.matrix(X_train_m2) || inherits(X_train_m2, "dgCMatrix"))

# ===============================================================
# 1) IMPORTANCIA XGBOOST (Gain)
# ===============================================================

imp_xgb <- xgb.importance(
  feature_names = colnames(X_train_m2),
  model = final
)

# Top 10 features por Gain
imp_xgb_top10 <- imp_xgb %>% slice_head(n = min(10, nrow(imp_xgb)))

p_gain_top10 <- ggplot(imp_xgb_top10,
                       aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(title = "Top 10 importancia XGBoost (Gain)",
       x = NULL, y = "Gain") +
  theme_minimal(base_size = 12)

print(p_gain_top10)
ggsave("plot_top10_gain.png", p_gain_top10, width = 7.5, height = 6, dpi = 150)

# ===============================================================
# 2) SHAP RÁPIDO (APROXIMADO, POR LOTES)
# ===============================================================

n_threads <- max(1, detectCores() - 1)
model_for_shap <- final

# Si existe best_ref, reentrenamos para usar multi-thread
if (exists("best_ref")) {
  params_mt <- best_ref$params
  params_mt$nthread <- n_threads
  
  model_for_shap <- xgb.train(
    params = params_mt,
    data = xgb.DMatrix(X_train_m2, label = y_m2),
    nrounds = best_ref$best_iter,
    verbose = 0
  )
}

# Muestreo para reducir tiempo/memoria
use_sample <- TRUE
max_rows <- 30000
use_batches <- TRUE
batch_size <- 10000

if (use_sample && nrow(X_train_m2) > max_rows) {
  set.seed(123)
  idx_rows <- sample.int(nrow(X_train_m2), max_rows)
} else {
  idx_rows <- seq_len(nrow(X_train_m2))
}

X_for_shap <- X_train_m2[idx_rows, , drop = FALSE]

compute_mean_abs_shap <- function(model, X,
                                  approx = TRUE,
                                  use_batches = TRUE,
                                  batch_size = 10000) {
  
  d <- ncol(X)
  feat_names <- colnames(X)
  sum_abs <- numeric(d)
  n_total <- 0L
  
  if (!use_batches) {
    shap_mat <- predict(model, xgb.DMatrix(X),
                        predcontrib = TRUE,
                        approxcontribs = approx)
    if (is.null(colnames(shap_mat))) {
      colnames(shap_mat) <- c(feat_names, "BIAS")
    }
    shap_feat <- shap_mat[, colnames(shap_mat) != "BIAS", drop = FALSE]
    sum_abs <- colSums(abs(shap_feat))
    n_total <- nrow(shap_feat)
    
  } else {
    n <- nrow(X)
    for (start in seq(1, n, by = batch_size)) {
      end <- min(start + batch_size - 1, n)
      Xb <- X[start:end, , drop = FALSE]
      
      shap_b <- predict(model, xgb.DMatrix(Xb),
                        predcontrib = TRUE,
                        approxcontribs = approx)
      
      if (is.null(colnames(shap_b))) {
        colnames(shap_b) <- c(feat_names, "BIAS")
      }
      
      shap_feat_b <- shap_b[, colnames(shap_b) != "BIAS", drop = FALSE]
      sum_abs <- sum_abs + colSums(abs(shap_feat_b))
      n_total <- n_total + nrow(shap_feat_b)
    }
  }
  
  mean_abs <- sum_abs / n_total
  
  data.frame(
    Feature = feat_names,
    MeanAbsSHAP = as.numeric(mean_abs)
  ) %>%
    arrange(desc(MeanAbsSHAP))
}

# Ejecutar SHAP
imp_shap <- compute_mean_abs_shap(
  model = model_for_shap,
  X = X_for_shap,
  approx = TRUE,
  use_batches = use_batches,
  batch_size = batch_size
)

# Top 10 features SHAP
imp_shap_top10 <- imp_shap %>% slice_head(n = min(10, nrow(imp_shap)))

p_shap_top10 <- ggplot(imp_shap_top10,
                       aes(x = reorder(Feature, MeanAbsSHAP),
                           y = MeanAbsSHAP)) +
  geom_col(fill = "#6a3d9a") +
  coord_flip() +
  labs(title = "Top 10 importancia SHAP (mean |contribution|)",
       x = NULL, y = "Mean |SHAP|") +
  theme_minimal(base_size = 12)

print(p_shap_top10)
ggsave("plot_top10_shap.png", p_shap_top10, width = 7.5, height = 6, dpi = 150)

# ===============================================================
# 3) IMPORTANCIA AGRUPADA POR PREFIJOS
# ===============================================================

base_prefixes <- c(
  "upz", "LocNombre", "property_type2", "month", "year",
  "estrato_num", "log_area_parque", "log_area", "area_x_estrato",
  "area_m2_final", "bed_x_banos", "bedrooms", "banos_tot",
  "lat_x_lon", "lat", "lon", "log_distancia_"
)

group_by_prefix <- function(df, name_col, value_col, prefixes) {
  df_local <- df
  out_list <- list()
  
  for (p in prefixes) {
    idx <- str_detect(df_local[[name_col]], paste0("^", p))
    if (any(idx)) {
      val <- sum(df_local[[value_col]][idx], na.rm = TRUE)
      out_list[[p]] <- data.frame(var = p, value = val)
      df_local <- df_local[!idx, , drop = FALSE]
    }
  }
  
  out <- bind_rows(out_list)
  if (nrow(out) == 0)
    return(data.frame(var = character(), value = numeric()))
  
  names(out)[2] <- value_col
  arrange(out, desc(.data[[value_col]]))
}

# ---- Gain agrupado
imp_xgb_gain_grp <- group_by_prefix(imp_xgb, "Feature", "Gain", base_prefixes)
imp_xgb_gain_grp_top10 <- imp_xgb_gain_grp %>% slice_head(n = min(10, nrow(imp_xgb_gain_grp)))

p_gain_grp_top10 <- ggplot(imp_xgb_gain_grp_top10,
                           aes(x = reorder(var, Gain), y = Gain)) +
  geom_col(fill = "royalblue") +
  geom_text(aes(label = round(Gain, 3)),
            color = "white",
            size = 10,
            fontface = "bold",
            hjust = 1.1) +   # empuja el texto hacia dentro de la barra
  coord_flip() +
  labs(title = "Top 10 importancia por grupo (Gain)",
       x = NULL,
       y = "Gain (sumado)") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 25), 
        axis.text.y = element_text(size = 21), 
        axis.text.x = element_text(size = 18),         , 
        axis.title.x = element_text(size = 20))


print(p_gain_grp_top10)
ggsave("plot_top10_group_gain.png", 
       p_gain_grp_top10, width = 15, height = 13, dpi = 150)

# ---- SHAP agrupado
imp_shap_grp <- group_by_prefix(imp_shap, "Feature", "MeanAbsSHAP", base_prefixes)
imp_shap_grp_top10 <- imp_shap_grp %>% slice_head(n = min(10, nrow(imp_shap_grp)))

p_shap_grp_top10 <- ggplot(imp_shap_grp_top10,
                           aes(x = reorder(var, MeanAbsSHAP), y = MeanAbsSHAP)) +
  geom_col(fill = "#6a3d9a") +
  geom_text(aes(label = round(MeanAbsSHAP, 3)),
            color = "white",
            size = 10,
            fontface = "bold",
            hjust = 1.1) +   # mueve el texto dentro de la barra
  coord_flip() +
  labs(title = "Top 10 importancia por grupo\n(Mean |SHAP|)",
       x = NULL,
       y = "Suma Mean |SHAP|") + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 25), 
        axis.text.y = element_text(size = 21), 
        axis.text.x = element_text(size = 18), 
        axis.title.x = element_text(size = 20))

print(p_shap_grp_top10)
ggsave("plot_top10_group_shap.png", 
       p_shap_grp_top10, width = 15, height = 13, dpi = 150)

# ---------------------------------------------------------------
cat("\n✔ Gráficos generados (Gain / SHAP / Agrupados)\n")


feat_imp <- imp_xgb %>%
  select(Feature, Gain, Cover, Frequency) %>%
  full_join(imp_shap %>% select(Feature, MeanAbsSHAP), by = "Feature") %>%
  mutate(
    Gain = coalesce(Gain, 0),
    Cover = coalesce(Cover, 0),
    Frequency = coalesce(Frequency, 0),
    MeanAbsSHAP = coalesce(MeanAbsSHAP, 0)
  )


top10_gain_tbl <- feat_imp %>%
  arrange(desc(Gain)) %>%
  slice_head(n = 10) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Feature, Gain, Cover, Frequency, MeanAbsSHAP) %>%
  mutate(
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4),
    MeanAbsSHAP = round(MeanAbsSHAP, 4)
  )


top10_shap_tbl <- feat_imp %>%
  arrange(desc(MeanAbsSHAP)) %>%
  slice_head(n = 10) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Feature, MeanAbsSHAP, Gain, Cover, Frequency) %>%
  mutate(
    MeanAbsSHAP = round(MeanAbsSHAP, 4),
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )


latex1 <- knitr::kable( top10_gain_tbl, 
                        format = "latex", 
                        booktabs = TRUE, 
                        caption = "Top 10 variables por Gain (XGBoost). Se muestran Gain, Cover, Frequency y Mean |SHAP|.", 
                        align = "l", 
                        escape = TRUE ) |> 
  kableExtra::kable_styling(latex_options = c("striped", 
                                              "hold_position"))

latex2 <- knitr::kable( top10_shap_tbl, format = "latex", booktabs = TRUE, caption = "Top 10 variables por importancia SHAP (media del valor absoluto). Se incluyen métricas de XGBoost para comparación.", align = "l", escape = TRUE ) |> kableExtra::kable_styling(latex_options = c("striped", "hold_position"))
