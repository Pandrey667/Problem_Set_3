###############################################################
# 1) Limpieza y librerías
###############################################################

remove(list = ls())

library(dplyr)
library(readr)
library(Metrics)
library(sf)
library(forcats)
library(stringi)
library(rpart)

set.seed(123)


###############################################################
# 2) Importación y preparación
###############################################################

# Ajusta la ruta si es necesario
setwd("C:/Users/fevid/Documents/MEcA/Big Data/Taller_3/datos_taller_3")

raw_train <- readRDS("df_def_entrenamiento.rds")
raw_test  <- readRDS("df_def_testeo.rds")

# Eliminar geometría si existe
if ("geometry" %in% names(raw_train)) raw_train <- sf::st_drop_geometry(raw_train)
if ("geometry" %in% names(raw_test))  raw_test  <- sf::st_drop_geometry(raw_test)

# IDs únicos en test
raw_test <- raw_test %>% distinct(property_id, .keep_all = TRUE)

# Unifica área
safe_coalesce_area <- function(df) {
  vars <- intersect(c("area_reportada", "surface_covered", "area_m2"), names(df))
  stopifnot(length(vars) > 0)
  
  x <- df[[vars[1]]]
  if (length(vars) > 1)
    for (j in 2:length(vars))
      x <- dplyr::coalesce(x, df[[vars[j]]])
  
  df$area_m2_final <- x
  df
}

train_imp <- raw_train %>%
  safe_coalesce_area() %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year  = factor(year)
  ) %>%
  select(-any_of("estrato"))

test_imp <- raw_test %>%
  safe_coalesce_area() %>%
  mutate(
    estrato_num = readr::parse_number(estrato),
    month = factor(month),
    year  = factor(year)
  ) %>%
  select(-any_of("estrato"))


###############################################################
# 3) Imputación (mediana/moda) y alineación month/year
###############################################################

mode_val <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA_character_)
  as.character(ux[which.max(tabulate(match(x, ux)))])
}

impute_fit <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, "price")
  
  med <- vapply(df[num_cols], function(z) median(z, na.rm = TRUE), numeric(1))
  med[!is.finite(med)] <- 0
  
  cat_cols <- names(df)[sapply(df, function(z) is.character(z) || is.factor(z))]
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

# Aplicar imputación
imp_par    <- impute_fit(train_imp)
train_imp  <- impute_apply(train_imp, imp_par)
test_imp   <- impute_apply(test_imp, imp_par)
test_imp   <- align_factor_levels(train_imp, test_imp)

# Asegurar price numérico
if (!("price" %in% names(train_imp)) && "price" %in% names(raw_train))
  train_imp$price <- raw_train$price

train_imp$price <- suppressWarnings(readr::parse_number(as.character(train_imp$price)))
stopifnot(length(train_imp$price) == nrow(train_imp))


###############################################################
# 4) Feature engineering
###############################################################

fe_engineer <- function(df) {
  out <- df
  
  # Logs de área
  if ("area_m2_final" %in% names(out))
    out$log_area <- log1p(out$area_m2_final)
  
  if ("area_parque" %in% names(out))
    out$log_area_parque <- log1p(out$area_parque)
  
  # Logs de distancias
  dcols <- grep("^distancia_", names(out), value = TRUE)
  if (length(dcols) > 0) {
    out[paste0("log_", dcols)] <- lapply(out[dcols], log1p)
    out[dcols] <- NULL
  }
  
  # Interacciones
  if (all(c("area_m2_final","estrato_num") %in% names(out)))
    out$area_x_estrato <- out$area_m2_final * out$estrato_num
  
  if (all(c("bedrooms","banos_tot") %in% names(out)))
    out$bed_x_banos <- out$bedrooms * out$banos_tot
  
  if (all(c("lat","lon") %in% names(out))) {
    out$lat_x_lon <- out$lat * out$lon
    out$lat2 <- out$lat^2
    out$lon2 <- out$lon^2
  }
  
  out
}

train_imp <- fe_engineer(train_imp)
test_imp  <- fe_engineer(test_imp)

# Lumping localización
if ("LocNombre" %in% names(train_imp)) {
  train_imp$LocNombre <- forcats::fct_lump_n(as.factor(train_imp$LocNombre), n = 20, other_level = "otros")
  
  test_imp$LocNombre <- forcats::fct_explicit_na(as.factor(test_imp$LocNombre), na_level = "otros")
  test_imp$LocNombre <- forcats::fct_other(test_imp$LocNombre, keep = levels(train_imp$LocNombre), other_level = "otros")
  test_imp$LocNombre <- factor(test_imp$LocNombre, levels = levels(train_imp$LocNombre))
}

if ("upz" %in% names(train_imp)) {
  train_imp$upz <- forcats::fct_lump_n(as.factor(train_imp$upz), n = 30, other_level = "otros")
  
  test_imp$upz <- forcats::fct_explicit_na(as.factor(test_imp$upz), na_level = "otros")
  test_imp$upz <- forcats::fct_other(test_imp$upz, keep = levels(train_imp$upz), other_level = "otros")
  test_imp$upz <- factor(test_imp$upz, levels = levels(train_imp$upz))
}


###############################################################
# 5) Predictores para CART (sin model.matrix)
###############################################################

protect_cols <- c(
  "price", "property_id", "title_norm", "description", "desc_norm",
  "city", "geometry", "barrio_oficial", "operation_type"
)

pred_cols <- setdiff(names(train_imp), protect_cols)

to_factor_cart <- function(df) {
  df %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(is.logical), as.factor)
    )
}

is_constant_col <- function(x) {
  if (all(is.na(x))) return(TRUE)
  if (is.logical(x)) return(length(unique(x[!is.na(x)])) <= 1)
  if (is.numeric(x)) {
    nx <- x[is.finite(x)]
    return(length(nx) == 0 || var(nx, na.rm = TRUE) == 0)
  }
  if (is.factor(x)) return(nlevels(x) <= 1)
  if (is.character(x)) return(length(unique(x[!is.na(x)])) <= 1)
  FALSE
}

prepare_cart_data <- function(train_df, test_df, pred_cols) {
  tr <- to_factor_cart(train_df[, pred_cols, drop = FALSE])
  te <- to_factor_cart(test_df[, pred_cols, drop = FALSE])
  
  # Eliminar columnas constantes
  drop <- names(tr)[sapply(tr, is_constant_col)]
  if (length(drop) > 0)
    message("Eliminando columnas constantes: ", paste(drop, collapse = ", "))
  
  keep <- setdiff(pred_cols, drop)
  tr <- tr[, keep, drop = FALSE]
  te <- te[, keep, drop = FALSE]
  
  # Alinear niveles
  for (col in intersect(names(tr), names(te))) {
    if (is.factor(tr[[col]]) && is.factor(te[[col]])) {
      tr_lvls <- levels(tr[[col]])
      te_chr  <- as.character(te[[col]])
      te_chr[!(te_chr %in% tr_lvls)] <- NA
      te[[col]] <- factor(te_chr, levels = tr_lvls)
    }
  }
  
  list(train_pred = tr, test_pred = te, kept = keep, dropped = drop)
}

mm_cart <- prepare_cart_data(train_imp, test_imp, pred_cols)
X_train_cart <- mm_cart$train_pred
X_test_cart  <- mm_cart$test_pred

if (length(mm_cart$dropped) > 0)
  message("Se eliminaron columnas: ", paste(mm_cart$dropped, collapse = ", "))


###############################################################
# 6) Subconjunto m² y target
###############################################################

idx_m2 <- which(
  is.finite(train_imp$area_m2_final) & train_imp$area_m2_final > 0 &
    is.finite(train_imp$price) & train_imp$price > 0
)

train_m2 <- train_imp[idx_m2, , drop = FALSE]

test_m2 <- test_imp %>%
  mutate(area_m2_final =
           ifelse(!is.finite(area_m2_final) | area_m2_final <= 0,
                  median(train_imp$area_m2_final, na.rm = TRUE),
                  area_m2_final))

X_train_m2_cart <- X_train_cart[idx_m2, , drop = FALSE]
X_test_m2_cart  <- X_test_cart

y_m2 <- log1p(train_m2$price / train_m2$area_m2_final)

stopifnot(nrow(X_train_m2_cart) == length(y_m2))


###############################################################
# 7) Folds espaciales
###############################################################

make_group_folds <- function(df, group_col, v = 8, seed = 123) {
  set.seed(seed)
  g <- as.character(df[[group_col]])
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

make_spatial_or_random_folds <- function(df, v = 8, seed = 123) {
  set.seed(seed)
  
  if ("LocNombre" %in% names(df) && any(!is.na(df$LocNombre)))
    return(make_group_folds(df, "LocNombre", v, seed))
  
  if ("upz" %in% names(df) && any(!is.na(df$upz)))
    return(make_group_folds(df, "upz", v, seed))
  
  if (all(c("lon", "lat") %in% names(df))) {
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
# 8) Retransformación a precio
###############################################################

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
# 9) CART rápido con OOF
###############################################################

cp_val      <- 0.01
minsplit    <- 40
minbucket   <- 15
maxdepth    <- 10
maxcompete  <- 2
maxsurrogate <- 2

oof_logm2 <- rep(NA_real_, nrow(X_train_m2_cart))

for (k in seq_along(folds_list)) {
  
  idx_te <- folds_list[[k]]
  idx_tr <- setdiff(seq_len(nrow(X_train_m2_cart)), idx_te)
  
  df_tr <- data.frame(y = y_m2[idx_tr], X_train_m2_cart[idx_tr, , drop = FALSE])
  df_te <- X_train_m2_cart[idx_te, , drop = FALSE]
  
  fit_tree <- tryCatch(
    rpart::rpart(
      y ~ ., data = df_tr, method = "anova",
      control = rpart::rpart.control(
        cp = cp_val,
        minsplit = minsplit,
        minbucket = minbucket,
        maxdepth = maxdepth,
        xval = 0,
        maxcompete = maxcompete,
        maxsurrogate = maxsurrogate
      ),
      na.action = rpart::na.rpart
    ),
    error = function(e) NULL
  )
  
  if (!is.null(fit_tree))
    oof_logm2[idx_te] <- predict(fit_tree, newdata = df_te)
}

scale_glob <- ratio_scale_price(train_m2$price, oof_logm2, train_m2$area_m2_final)
train_pred_price_oof <- clip_price(
  scale_glob * (train_m2$area_m2_final * (exp(oof_logm2) - 1)),
  train_m2$price
)

ok_oof <- is.finite(train_pred_price_oof) & is.finite(train_m2$price)
cat("Predicciones OOF válidas:", sum(ok_oof), "de", length(ok_oof), "\n")

if (sum(ok_oof) > 0) {
  mae_oof  <- Metrics::mae(train_m2$price[ok_oof], train_pred_price_oof[ok_oof])
  rmse_oof <- Metrics::rmse(train_m2$price[ok_oof], train_pred_price_oof[ok_oof])
  cat(sprintf("CART OOF MAE: %.2f | OOF RMSE: %.2f\n", mae_oof, rmse_oof))
} else {
  cat("No hay predicciones OOF válidas.\n")
}


###############################################################
# 10) Entrenamiento final y predicción
###############################################################

tree_full <- rpart::rpart(
  y ~ ., data = data.frame(y = y_m2, X_train_m2_cart),
  method = "anova",
  control = rpart::rpart.control(
    cp = cp_val,
    minsplit = minsplit,
    minbucket = minbucket,
    maxdepth = maxdepth,
    xval = 0,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate
  ),
  na.action = rpart::na.rpart
)

pred_m2_log_test <- predict(tree_full, newdata = X_test_m2_cart)

pred_price_test <- clip_price(
  scale_glob * (test_m2$area_m2_final * (exp(pred_m2_log_test) - 1)),
  train_m2$price
)

# Diagnóstico en train
pred_m2_log_train_full <- predict(tree_full, newdata = X_train_m2_cart)
pred_price_train_full <- clip_price(
  scale_glob * (train_m2$area_m2_final * (exp(pred_m2_log_train_full) - 1)),
  train_m2$price
)

mae_all  <- Metrics::mae(train_m2$price, pred_price_train_full)
rmse_all <- Metrics::rmse(train_m2$price, pred_price_train_full)

cat(sprintf("CART Train MAE: %.2f | RMSE: %.2f\n", mae_all, rmse_all))


###############################################################
# 11) Calibración por grupo con shrinkage
###############################################################

group_vars <- intersect(
  c("estrato_num", "property_type2", "LocNombre", "upz"),
  names(train_m2)
)

if (length(group_vars) > 0) {
  
  train_m2 <- train_m2 %>% mutate(grp = interaction(across(all_of(group_vars)), drop = TRUE))
  test_m2  <- test_m2  %>% mutate(grp = interaction(across(all_of(group_vars)), drop = TRUE))
  
  scales_grp <- train_m2 %>%
    mutate(yhat_oof = train_pred_price_oof) %>%
    group_by(grp) %>%
    summarise(
      scale_grp = median(price / yhat_oof, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      scale_grp = ifelse(!is.finite(scale_grp) | scale_grp <= 0 | scale_grp > 50, 1, scale_grp),
      scale_grp = (n/(n+100))*scale_grp + (100/(n+100))*scale_glob
    )
  
  pred_price_test_grp <- test_m2 %>%
    select(property_id, grp, area_m2_final) %>%
    mutate(yhat_raw = area_m2_final * (exp(pred_m2_log_test) - 1)) %>%
    left_join(scales_grp, by = "grp") %>%
    mutate(
      scale_grp = coalesce(scale_grp, scale_glob),
      price = clip_price(scale_grp * yhat_raw, train_m2$price)
    ) %>%
    pull(price)
  
} else {
  pred_price_test_grp <- pred_price_test
}


###############################################################
# 12) Submissions
###############################################################

write_submission <- function(df_test, pred_vec, filename) {
  tbl <- df_test %>%
    select(property_id) %>%
    mutate(price = as.numeric(pred_vec)) %>%
    mutate(price = ifelse(is.finite(price), price, median(price, na.rm = TRUE)))
  
  subm <- raw_test %>%
    select(property_id) %>%
    left_join(tbl, by = "property_id")
  
  stopifnot(nrow(subm) == nrow(raw_test), all(!is.na(subm$price)))
  
  readr::write_csv(subm, filename)
}

write_submission(test_m2, pred_price_test,
                 "submission_cart_fast_m2_global_ratio_clip.csv")

write_submission(test_m2, pred_price_test_grp,
                 "submission_cart_fast_m2_groupCal_ratio_clip.csv")
