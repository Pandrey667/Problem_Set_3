###############################################################
#1) LIMPIEZA Y LIBRERÍAS
###############################################################
remove(list = ls())

library(dplyr)
library(readr)
library(Metrics)
library(glmnet)
library(Matrix)
library(sf)
library(forcats)
library(stringi)
set.seed(123)

###############################################################
#2) IMPORTACIÓN Y DROP DE GEOMETRÍAS
###############################################################
setwd("C:/Users/fevid/Documents/MEcA/Big Data/Taller_3/datos_taller_3")

raw_train <- readRDS("df_def_entrenamiento.rds")
raw_test  <- readRDS("df_def_testeo.rds")

if ("geometry" %in% names(raw_train)) raw_train <- sf::st_drop_geometry(raw_train)
if ("geometry" %in% names(raw_test)) raw_test  <- sf::st_drop_geometry(raw_test)

# IDs únicos en test
raw_test <- raw_test %>% distinct(property_id, .keep_all = TRUE)

###############################################################
#3) VARIABLES BASE (área y estrato)
###############################################################
safe_coalesce_area <- function(df) {
  vars <- intersect(c("area_reportada","surface_covered","area_m2"), names(df))
  stopifnot(length(vars) > 0)
  out <- df[[vars[1]]]
  if (length(vars) > 1)
    for (j in 2:length(vars))
      out <- dplyr::coalesce(out, df[[vars[j]]])
  df$area_m2_final <- out
  df
}

train_imp <- safe_coalesce_area(raw_train) %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year  = factor(year)
  )

test_imp <- safe_coalesce_area(raw_test) %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year  = factor(year)
  )

# Quitar 'estrato' crudo
train_imp <- select(train_imp, -any_of("estrato"))
test_imp  <- select(test_imp, -any_of("estrato"))

###############################################################
#4) IMPUTACIÓN (medianas/moda) + ALINEACIÓN
###############################################################
mode_val <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA_character_)
  as.character(ux[which.max(tabulate(match(x, ux)))])
}

impute_fit <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, "price")
  medians <- vapply(df[num_cols], function(x) median(x, na.rm = TRUE), numeric(1))
  medians[!is.finite(medians)] <- 0
  
  cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  modes <- vapply(cat_cols, function(nm) mode_val(df[[nm]]), character(1))
  list(medians = medians, modes = modes)
}

impute_apply <- function(df, imp) {
  out <- df
  cn <- intersect(names(imp$medians), names(out))
  for (nm in cn) {
    x <- out[[nm]]
    x[is.na(x)] <- imp$medians[[nm]]
    out[[nm]] <- x
  }
  cc <- intersect(names(imp$modes), names(out))
  for (nm in cc) {
    x <- as.character(out[[nm]])
    x[is.na(x)] <- imp$modes[[nm]]
    out[[nm]] <- factor(x)
  }
  out
}

align_factor_levels <- function(train_df, test_df, fac_cols = c("month","year")) {
  out <- test_df
  for (fc in fac_cols) if (fc %in% names(train_df) && fc %in% names(test_df)) {
    levs <- levels(train_df[[fc]])
    if (length(levs) == 0) levs <- unique(na.omit(train_df[[fc]]))
    if (length(levs) == 0) levs <- "missing"
    out[[fc]] <- as.character(out[[fc]])
    out[[fc]][!(out[[fc]] %in% levs) | is.na(out[[fc]])] <- levs[1]
    out[[fc]] <- factor(out[[fc]], levels = levs)
  }
  out
}

imp_par   <- impute_fit(train_imp)
train_imp <- impute_apply(train_imp, imp_par)
test_imp  <- impute_apply(test_imp, imp_par)
test_imp  <- align_factor_levels(train_imp, test_imp, c("month","year"))

# Asegurar 'price' numérica
if (!("price" %in% names(train_imp)) && "price" %in% names(raw_train)) {
  train_imp$price <- raw_train$price
}

train_imp$price <- suppressWarnings(readr::parse_number(as.character(train_imp$price)))
stopifnot(length(train_imp$price) == nrow(train_imp))

###############################################################
#5) FEATURE ENGINEERING + dummy Chapinero
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

if ("LocNombre" %in% names(train_imp)) {
  norm <- function(x) tolower(trimws(as.character(x)))
  train_imp$d_chapinero <- as.integer(norm(train_imp$LocNombre) == "chapinero")
  test_imp$d_chapinero  <- as.integer(norm(test_imp$LocNombre)  == "chapinero")
}

###############################################################
#6) ENTRENAMIENTO FOCALIZADO: Chapinero + vecinas
###############################################################
norm_loc <- function(x) stringi::stri_trans_general(tolower(trimws(as.character(x))), "Latin-ASCII")
near_locs <- c("chapinero","usaquen","teusaquillo","barrios unidos","santa fe")

if ("LocNombre" %in% names(train_imp)) {
  train_imp <- train_imp %>%
    mutate(LocNombre_norm = norm_loc(LocNombre)) %>%
    filter(LocNombre_norm %in% near_locs) %>%
    select(-LocNombre_norm)
  message("Training filtrado a: ", paste(near_locs, collapse = ", "), " | filas: ", nrow(train_imp))
} else {
  warning("LocNombre no está en training; no se aplica filtro por vecindad.")
}

if (nrow(train_imp) < 500) {
  warning("Pocas filas tras filtro (", nrow(train_imp), "). Considera ampliar near_locs.")
}

###############################################################
#7) DROP CERO-VARIANZA SOLO EN PREDICTORES
###############################################################
protect_cols <- c("price","LocNombre","upz","property_id",
                  "title_norm","description","desc_norm",
                  "city","geometry","barrio_oficial",
                  "operation_type")

pred_cols <- setdiff(names(train_imp), protect_cols)

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

zero_var_flags <- vapply(pred_cols, function(nm) is_constant_col(train_imp[[nm]]), logical(1))
zero_var <- names(zero_var_flags)[zero_var_flags]

if (length(zero_var) > 0) {
  message("Se eliminan columnas sin variación: ", paste(zero_var, collapse = ", "))
  train_imp <- select(train_imp, -all_of(zero_var))
  test_imp  <- select(test_imp,  -all_of(zero_var))
}

pred_cols <- intersect(names(train_imp), names(test_imp))
pred_cols <- setdiff(pred_cols, protect_cols)

###############################################################
#8) MATRICES DE DISEÑO Y ALINEACIÓN
###############################################################
fmla_rhs_0 <- as.formula(paste("~ 0 +", paste(pred_cols, collapse = " + "))) # glmnet
fmla_rhs_1 <- as.formula(paste("~ 1 +", paste(pred_cols, collapse = " + "))) # OLS

X_train_dense <- model.matrix(fmla_rhs_0, data = train_imp)
X_test_dense  <- model.matrix(fmla_rhs_0, data = test_imp)

align_mm <- function(Xtr, Xte) {
  tr_names <- colnames(Xtr)
  te_names <- colnames(Xte)
  add <- setdiff(tr_names, te_names)
  if (length(add) > 0) {
    add_mat <- matrix(0, nrow = nrow(Xte), ncol = length(add))
    colnames(add_mat) <- add
    Xte <- cbind(Xte, add_mat)
  }
  Xte <- Xte[, tr_names, drop = FALSE]
  list(Xtr = Xtr, Xte = Xte)
}

aligned <- align_mm(X_train_dense, X_test_dense)
x_train <- Matrix::Matrix(aligned$Xtr, sparse = TRUE)
x_test  <- Matrix::Matrix(aligned$Xte, sparse = TRUE)

y_train <- log1p(train_imp$price)

rows_ok <- is.finite(y_train) & Matrix::rowSums(is.na(x_train)) == 0
if (!all(rows_ok)) {
  message("Filas inválidas removidas: ", sum(!rows_ok))
  x_train  <- x_train[rows_ok, , drop = FALSE]
  y_train  <- y_train[rows_ok]
  train_imp <- train_imp[rows_ok, , drop = FALSE]
}

###############################################################
#9) FOLDS ESPACIALES (LocNombre/UPZ; lon/lat; aleatorio)
###############################################################
closest_lambda_idx <- function(lambda_vec, lambda_target)
  which.min(abs(lambda_vec - lambda_target))

make_group_folds <- function(df, group_col, v = 5, seed = 123) {
  set.seed(seed)
  g <- df[[group_col]]
  g <- as.character(g)
  g[is.na(g) | g == ""] <- paste0("NA", sample.int(v, sum(is.na(df[[group_col]]) | df[[group_col]] == ""), replace = TRUE))
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

make_spatial_or_random_folds <- function(df, v = 5, seed = 123) {
  set.seed(seed)
  if ("LocNombre" %in% names(df) && any(!is.na(df$LocNombre)))
    return(make_group_folds(df, "LocNombre", v, seed))
  if ("upz" %in% names(df) && any(!is.na(df$upz)))
    return(make_group_folds(df, "upz", v, seed))
  if (all(c("lon","lat") %in% names(df))) {
    coords_ok <- which(stats::complete.cases(df$lon, df$lat))
    km <- stats::kmeans(df[coords_ok, c("lon","lat")], centers = v, nstart = 20)
    foldid <- rep(NA_integer_, nrow(df))
    foldid[coords_ok] <- km$cluster
    foldid[is.na(foldid)] <- sample.int(v, sum(is.na(foldid)), replace = TRUE)
    return(as.integer(foldid))
  }
  sample.int(v, nrow(df), replace = TRUE)
}

foldid_sp <- make_spatial_or_random_folds(train_imp, v = 5, seed = 42)

# Parche si se removieron filas
foldid_sp <- foldid_sp[seq_len(nrow(x_train))]

###############################################################
#10) CV ESPACIAL POR MAE + RATIO SCALING + CLIPPING
###############################################################
cv_ridge_sp <- cv.glmnet(
  x_train, y_train, alpha = 0, foldid = foldid_sp,
  family = "gaussian", type.measure = "mae",
  standardize = TRUE, keep = TRUE
)

cv_lasso_sp <- cv.glmnet(
  x_train, y_train, alpha = 1, foldid = foldid_sp,
  family = "gaussian", type.measure = "mae",
  standardize = TRUE, keep = TRUE
)

alpha_grid_coarse <- c(0.1, 0.3, 0.5, 0.7, 0.9)

cv_enet_sp_list <- lapply(alpha_grid_coarse, function(a)
  cv.glmnet(x_train, y_train, alpha = a, foldid = foldid_sp,
            family = "gaussian", type.measure = "mae",
            standardize = TRUE, keep = TRUE)
)

cvm_min <- sapply(cv_enet_sp_list, function(obj) min(obj$cvm, na.rm = TRUE))
best_alpha_sp <- alpha_grid_coarse[which.min(cvm_min)]

alpha_fine <- sort(unique(pmax(0, pmin(1, seq(best_alpha_sp - 0.2,
                                              best_alpha_sp + 0.2, by = 0.05)))))

cv_enet_sp_list2 <- lapply(alpha_fine, function(a)
  cv.glmnet(x_train, y_train, alpha = a, foldid = foldid_sp,
            family = "gaussian", type.measure = "mae",
            standardize = TRUE, keep = TRUE)
)

cvm_min2 <- sapply(cv_enet_sp_list2, function(obj) min(obj$cvm, na.rm = TRUE))
best_idx <- which.min(cvm_min2)
best_alpha_sp <- alpha_fine[best_idx]
best_enet_sp <- cv_enet_sp_list2[[best_idx]]

ratio_scale <- function(y, prev_log, clip = c(0.01, 0.99)) {
  yhat_raw <- exp(prev_log) - 1
  ok <- is.finite(y) & is.finite(yhat_raw) & yhat_raw > 0
  if (sum(ok) < 50) return(1)
  r <- y[ok] / yhat_raw[ok]
  q <- quantile(r, clip, na.rm = TRUE)
  r <- pmin(pmax(r, q[1]), q[2])
  s <- median(r, na.rm = TRUE)
  if (!is.finite(s) || s <= 0 || s > 50) s <- 1
  s
}

choose_lambda_mae_ratio <- function(cvobj, y_true) {
  lmin <- cvobj$lambda.min
  l1se <- cvobj$lambda.1se
  idx_min <- which.min(abs(cvobj$lambda - lmin))
  idx_1se <- which.min(abs(cvobj$lambda - l1se))
  prev_min  <- cvobj$fit.preval[, idx_min]
  prev_1se  <- cvobj$fit.preval[, idx_1se]
  scale_min  <- ratio_scale(y_true, prev_min)
  scale_1se  <- ratio_scale(y_true, prev_1se)
  mae_min <- Metrics::mae(y_true, pmax(0, scale_min*(exp(prev_min) - 1)))
  mae_1se <- Metrics::mae(y_true, pmax(0, scale_1se*(exp(prev_1se) - 1)))
  if (is.finite(mae_min) && mae_min <= mae_1se) {
    list(lambda = lmin,  idx = idx_min,  scale = scale_min,  mae = mae_min)
  } else {
    list(lambda = l1se, idx = idx_1se, scale = scale_1se, mae = mae_1se)
  }
}

ch_ridge <- choose_lambda_mae_ratio(cv_ridge_sp, train_imp$price)
ch_lasso <- choose_lambda_mae_ratio(cv_lasso_sp, train_imp$price)
ch_enet  <- choose_lambda_mae_ratio(best_enet_sp, train_imp$price)

ridge_final <- glmnet(x_train, y_train, alpha = 0, lambda = ch_ridge$lambda, standardize = TRUE)
lasso_final <- glmnet(x_train, y_train, alpha = 1, lambda = ch_lasso$lambda, standardize = TRUE)
enet_final  <- glmnet(x_train, y_train, alpha = best_alpha_sp, lambda = ch_enet$lambda, standardize = TRUE)

pred_ridge_log <- drop(predict(ridge_final, newx = x_test, s = ch_ridge$lambda))
pred_lasso_log <- drop(predict(lasso_final, newx = x_test, s = ch_lasso$lambda))
pred_enet_log  <- drop(predict(enet_final, newx = x_test, s = ch_enet$lambda))

ridge_preval <- cv_ridge_sp$fit.preval[, ch_ridge$idx]
lasso_preval <- cv_lasso_sp$fit.preval[, ch_lasso$idx]
enet_preval  <- best_enet_sp$fit.preval[, ch_enet$idx]

scale_ridge <- ch_ridge$scale
scale_lasso <- ch_lasso$scale
scale_enet  <- ch_enet$scale

clip_preds <- function(pred, y_train, clip = c(0.01, 0.99)) {
  q <- quantile(y_train[is.finite(y_train)], clip, na.rm = TRUE)
  pmin(pmax(pred, q[1]), q[2])
}

train_pred_ridge_oof <- clip_preds(pmax(0, scale_ridge*(exp(ridge_preval) - 1)), train_imp$price)
train_pred_lasso_oof <- clip_preds(pmax(0, scale_lasso*(exp(lasso_preval) - 1)), train_imp$price)
train_pred_enet_oof  <- clip_preds(pmax(0, scale_enet *(exp(enet_preval) - 1)), train_imp$price)

pred_ridge <- clip_preds(pmax(0, scale_ridge*(exp(pred_ridge_log) - 1)), train_imp$price)
pred_lasso <- clip_preds(pmax(0, scale_lasso*(exp(pred_lasso_log) - 1)), train_imp$price)
pred_enet  <- clip_preds(pmax(0, scale_enet *(exp(pred_enet_log) - 1)), train_imp$price)

cat("Escalas (Ridge/Lasso/ENet):", scale_ridge, scale_lasso, scale_enet, "\n")

###############################################################
#11) OLS OOF CON MATRICES + RATIO SCALING + CLIPPING
###############################################################
X_train_full <- model.matrix(fmla_rhs_1, data = train_imp)
X_test_full  <- model.matrix(fmla_rhs_1, data = test_imp)

align_full <- (function(Xtr, Xte) {
  trn <- colnames(Xtr)
  ten <- colnames(Xte)
  add <- setdiff(trn, ten)
  if (length(add) > 0) {
    add_mat <- matrix(0, nrow = nrow(Xte), ncol = length(add))
    colnames(add_mat) <- add
    Xte <- cbind(Xte, add_mat)
  }
  Xte <- Xte[, trn, drop = FALSE]
  list(Xtr = Xtr, Xte = Xte)
})(X_train_full, X_test_full)

X_train_full <- align_full$Xtr
X_test_full  <- align_full$Xte

pred_ols_log_oof <- rep(NA_real_, nrow(train_imp))

for (k in sort(unique(foldid_sp))) {
  idx_te <- which(foldid_sp == k)
  idx_tr <- which(foldid_sp != k)
  X_tr_k <- X_train_full[idx_tr, , drop = FALSE]
  X_te_k <- X_train_full[idx_te, , drop = FALSE]
  y_tr_k <- log1p(train_imp$price[idx_tr])
  fit_k <- tryCatch(stats::lm.fit(x = X_tr_k, y = y_tr_k),
                    error = function(e) NULL)
  if (is.null(fit_k)) {
    pred_ols_log_oof[idx_te] <- mean(y_tr_k, na.rm = TRUE)
  } else {
    preds <- drop(X_te_k %*% coef(fit_k))
    preds[!is.finite(preds)] <- mean(y_tr_k, na.rm = TRUE)
    pred_ols_log_oof[idx_te] <- preds
  }
}

scale_ols <- ratio_scale(train_imp$price, pred_ols_log_oof)
fit_ols_full <- stats::lm.fit(x = X_train_full, y = log1p(train_imp$price))
pred_ols_log <- drop(X_test_full %*% coef(fit_ols_full))

train_pred_ols_oof <- clip_preds(pmax(0, scale_ols*(exp(pred_ols_log_oof) - 1)), train_imp$price)
pred_ols           <- clip_preds(pmax(0, scale_ols*(exp(pred_ols_log) - 1)), train_imp$price)

###############################################################
#12) TABLA OOF (MAE y RMSE)
###############################################################
ooftab <- tibble::tibble(
  model = c("Ridge","Lasso","Enet","OLS"),
  MAE   = c(
    Metrics::mae(train_imp$price, train_pred_ridge_oof),
    Metrics::mae(train_imp$price, train_pred_lasso_oof),
    Metrics::mae(train_imp$price, train_pred_enet_oof),
    Metrics::mae(train_imp$price, train_pred_ols_oof)
  ),
  RMSE  = c(
    Metrics::rmse(train_imp$price, train_pred_ridge_oof),
    Metrics::rmse(train_imp$price, train_pred_lasso_oof),
    Metrics::rmse(train_imp$price, train_pred_enet_oof),
    Metrics::rmse(train_imp$price, train_pred_ols_oof)
  )
)

print(ooftab)

###############################################################
#13) SUBMISSIONS ROBUSTAS (garantiza filas y no NAs)
###############################################################
write_submission <- function(pred_vec, filename) {
  tbl <- test_imp %>%
    select(property_id) %>%
    mutate(price = as.numeric(pred_vec)) %>%
    mutate(price = ifelse(is.finite(price), price, median(price, na.rm = TRUE)))
  
  subm <- raw_test %>%
    select(property_id) %>%
    left_join(tbl, by = "property_id")
  
  stopifnot(nrow(subm) == nrow(raw_test), all(!is.na(subm$price)))
  
  readr::write_csv(subm, filename)
}

write_submission(pred_ridge, "submission_ridge_MAE_nearCh_ratio_clip.csv")
write_submission(pred_lasso, "submission_lasso_MAE_nearCh_ratio_clip.csv")
write_submission(pred_enet,  "submission_enet_MAE_nearCh_ratio_clip.csv")
write_submission(pred_ols,   "submission_ols_MAE_nearCh_ratio_clip.csv")

###############################################################
#14) CALIBRACIÓN MAE PARA CHAPINERO (post-proceso)
###############################################################
is_ch <- if ("LocNombre" %in% names(train_imp))
  tolower(trimws(as.character(train_imp$LocNombre))) == "chapinero"
else rep(FALSE, nrow(train_imp))

# Elegir mejor modelo OOF (ajusta según resultados)
oof_best <- train_pred_lasso_oof
pred_best <- pred_lasso

ratio_ch <- if (any(is_ch, na.rm = TRUE)) {
  r <- train_imp$price[is_ch] / oof_best[is_ch]
  r[is.finite(r) & r > 0]
} else numeric(0)

if (length(ratio_ch) >= 30) {
  scale_ch <- median(ratio_ch, na.rm = TRUE)
} else {
  r_all <- train_imp$price / oof_best
  r_all <- r_all[is.finite(r_all) & r_all > 0]
  scale_ch <- median(r_all, na.rm = TRUE)
}

pred_best_cal <- clip_preds(pmax(0, as.numeric(pred_best) * scale_ch), train_imp$price)

write_submission(pred_best_cal, "submission_best_calibrado_MAE_Ch_nearCh_ratio_clip.csv")

###############################################################
#15) ENSEMBLES ORIENTADOS A MAE (mediana y ponderado por 1/MAE)
###############################################################
pred_median <- apply(
  cbind(as.numeric(pred_lasso), as.numeric(pred_ridge), as.numeric(pred_ols)),
  1, median, na.rm = TRUE
)

write_submission(pred_median, "submission_ensemble_median_LRO_nearCh_ratio_clip.csv")

mae_lasso <- Metrics::mae(train_imp$price, train_pred_lasso_oof)
mae_ridge <- Metrics::mae(train_imp$price, train_pred_ridge_oof)
mae_ols   <- Metrics::mae(train_imp$price, train_pred_ols_oof)

Metrics::rmse(train_imp$price, train_pred_ridge_oof)

w <- c(1/mae_lasso, 1/mae_ridge, 1/mae_ols)
w <- w / sum(w)

pred_wavg <- w[1]*as.numeric(pred_lasso) +
  w[2]*as.numeric(pred_ridge) +
  w[3]*as.numeric(pred_ols)

write_submission(pred_wavg, "submission_ensemble_wavg_LRO_nearCh_ratio_clip.csv")

###############################################################
#16) LOG Y VERIFICACIONES
###############################################################
cat("\nFilas test:", nrow(raw_test), "\n")
cat("Submissions generadas:\n",
    "- submission_ridge_MAE_nearCh_ratio_clip.csv\n",
    "- submission_lasso_MAE_nearCh_ratio_clip.csv\n",
    "- submission_enet_MAE_nearCh_ratio_clip.csv\n",
    "- submission_ols_MAE_nearCh_ratio_clip.csv\n",
    "- submission_best_calibrado_MAE_Ch_nearCh_ratio_clip.csv\n",
    "- submission_ensemble_median_LRO_nearCh_ratio_clip.csv\n",
    "- submission_ensemble_wavg_LRO_nearCh_ratio_clip.csv\n")

