# =============================================================================
# PREPARACIÓN Y LIMPIEZA DE DATOS 
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

library(tidyverse)      # Manipulación y manejo de datos
library(data.table)     # Manejo de grandes volúmenes de datos
library(rvest)          # Extracción y análisis de texto desde web
library(stringi)        # Manipulación de datos en caracteres
library(readxl)         # Importar bases en formato Excel
library(sf)             # Importación y uso de archivos shapefile
library(osmdata)        # Datos de OpenStreetMap
library(leaflet)        # Visualización de mapas interactivos
library(fuzzyjoin)      # Join aproximado de datos
library(scales)         # Formateo de escalas y porcentajes

# =============================================================================
# 3. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# =============================================================================

setwd('C:/Users/fevid/Documents/MEcA/Big Data/Taller_3/datos_taller_3')
#setwd("C:/Users/investigacion/Desktop/SP3BDML")


# =============================================================================
# 4. CARGA Y PREPROCESAMIENTO DE ARCHIVOS CSV
# =============================================================================

# Guardamos en una lista los nombres de los archivos csv (Train y Test)
lista_t3_txt <- list.files(pattern = "\\.csv$", 
                           full.names = TRUE)

# Usamos el loop map (librería purrr) para leer y guardar en una lista los datos
lista_t3 <- lista_t3_txt %>%
  set_names(basename(.)) %>%
  map(\(f) read.csv(f, stringsAsFactors = FALSE)) %>%
  # Convertimos nombres de columnas a letra minúscula
  map(\(x) { names(x) <- tolower(names(x)); x })

# Extraemos el data frame que contiene el título "train" en el nombre
train <- lista_t3 %>% 
  pluck(which(str_detect(names(.), "train")))

# Convertimos el df de train a un objeto tipo data.table
setDT(train)

# =============================================================================
# 5. NORMALIZACIÓN DE TEXTOS 
# =============================================================================

# Corregimos errores ortográficos específicos en la variable title
train[, title := stringr::str_replace_all(title,
                                          stringr::regex("\\bbrbara\\b", ignore_case = TRUE),
                                          "barbara")]

# Normalizamos la variable title: minúsculas, sin acentos, sin puntuación
train[, title_norm := title %>% 
        stringr::str_to_lower() %>% 
        stringi::stri_trans_general("Latin-ASCII")  %>% 
        stringr::str_replace_all("[[:punct:]]", " ") %>% 
        stringr::str_squish()]

# Creamos vector con palabras a excluir de las descripciones
excluir_desc <- c("balcon", 
                  "patio", 
                  "estacion", 
                  "torre", 
                  "jardin")

# Construimos el patrón regex con las palabras a excluir
pat_excluir <- paste0("\\b(?:", 
                      paste(excluir_desc, 
                            collapse = "|"), 
                      ")\\b")

# Normalizamos la variable description: minúsculas, sin acentos, sin puntuación
train[, desc_norm := description %>% 
        stringr::str_to_lower() %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        stringr::str_replace_all("[[:punct:]]", " ") %>%
        stringr::str_squish() %>%
        stringr::str_remove_all(pat_excluir) %>%
        stringr::str_squish()]

# =============================================================================
# 6. EXTRACCIÓN DE ÁREA DESDE LA DESCRIPCIÓN
# =============================================================================
# Cortamos los números de área muy grandes 
num_pat <- "(?:\\d{1,5}(?:[\\.,]\\d{1,2})?)"

# Definimos patrones para detectar unidades de área (m2, mt2, etc.)
units_pat <- "(?:m\\s*(?:\\^?\\s*2)|mt\\s*(?:\\^?\\s*2)|mts\\s*(?:\\^?\\s*2)|m2|mt2|mts2|m²|mt²|mts²|metros\\s*cuadrados|metro\\s*cuadrado|metros|metro|m|mt|mts)"

# Construimos el patrón completo para detectar área
pattern <- paste0("(?i)\\b(", num_pat, ")\\s*(", units_pat, ")\\b")

# Buscamos coincidencias entre la descripción y los patrones de área
mat <- stringr::str_match(train$desc_norm, pattern)  

# Extraemos el área numérica (segunda columna de la matriz)
numero_raw <- mat[, 2]

# Limpiamos la unidad de medida
unidad_clean <- ifelse(is.na(mat[,3]), 
                       NA_character_, 
                       stringr::str_squish(mat[,3]))

# Homogeneizamos las diferentes representaciones de metros cuadrados a "m2"
unidad_norm <- dplyr::case_when(
  is.na(unidad_clean) ~ NA_character_,
  stringr::str_detect(unidad_clean, 
                      "(\\^?\\s*2|cuadrad|\\bm2\\b|\\bm²\\b|\\bmt2\\b|\\bmts2\\b|\\bmt²\\b|\\bmts²\\b)") ~ "m2",
  stringr::str_detect(unidad_clean, 
                      "^(m|mt|mts|metro|metros)$") ~ "m2",
  TRUE ~ unidad_clean
)

# Creamos la variable area_reportada_desc con el formato estandarizado
train[, area_reportada_desc := ifelse(!is.na(numero_raw),
                                      paste(numero_raw, 
                                            ifelse(unidad_norm == "m2", 
                                                   "m2", 
                                                   unidad_clean)),
                                      NA_character_)]

# Convertimos el número a formato decimal normalizado
num_norm_desc <- ifelse(!is.na(numero_raw), 
                        as.numeric(gsub(",", ".", 
                                        numero_raw)), 
                        NA_real_)

# Asignamos los valores numéricos cuando la unidad es m2
train[, area_m2_desc := ifelse(unidad_norm == "m2", 
                               num_norm_desc, 
                               NA_real_)]




# =============================================================================
# 7. EXTRACCIÓN DE ÁREA DESDE SURFACED_TOTAL
# =============================================================================

# Detectamos la columna surfaced_total de forma flexible
col_surf <- names(train)[stringr::str_detect(stringr::str_to_lower(names(train)),
                                             "(^|\\b)surface(d)?_?\\s*total\\b|^surfaced_total$|^surface_total$")]

if (length(col_surf) > 0) {
  col_surf <- col_surf[1]
  surf_norm <- train[[col_surf]] %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_squish()
  
  surf_match <- stringr::str_match(surf_norm, paste0("\\b(", num_pat, ")\\b"))
  
  surf_num <- ifelse(is.na(surf_match[, 2]),
                     NA_real_,
                     as.numeric(gsub(",", ".", surf_match[, 2])))
  
  surf_text <- ifelse(!is.na(surf_num),
                      paste0(formatC(surf_num, format = "fg", digits = 6), " m2"),
                      NA_character_)
} else {
  surf_num <- rep(NA_real_, nrow(train))
  surf_text <- rep(NA_character_, nrow(train))
  message("No se encontró columna surfaced_total; usaremos solo el área extraída de description.")
}

# Creamos columnas finales priorizando surfaced_total sobre description
train[, area_reportada := data.table::fifelse(!is.na(surf_text), surf_text, area_reportada_desc)]
train[, area_m2        := data.table::fifelse(!is.na(surf_num), surf_num, area_m2_desc)]

# =============================================================================
# 8. CORRECCIÓN DE ÁREAS CON ERRORES DE ESCALA
# =============================================================================

digits_n <- function(x) nchar(sprintf("%.0f", x))
es_entero <- !is.na(train$area_m2) & (train$area_m2 == floor(train$area_m2))
idx_4 <- es_entero & digits_n(train$area_m2) == 4 & train$area_m2 >= 2000
idx_5 <- es_entero & digits_n(train$area_m2) == 5
idx_corr <- idx_4 | idx_5

# Aplicamos corrección dividiendo por 100
train[idx_corr, area_m2 := area_m2 / 100]

# Actualizamos el texto area_reportada en las filas corregidas
train[idx_corr & !is.na(area_m2),
      area_reportada := paste0(formatC(area_m2, format = "fg", digits = 6), " m2")]

# Convertimos a NA las áreas menores a 15 m2 (probablemente errores)
train[!is.na(area_m2) & area_m2 < 15, area_m2 := NA_real_]

# =============================================================================
# 9. SCRAPING DE DATOS DE BARRIOS DESDE WIKIPEDIA
# =============================================================================
# Extraemos tabla de barrios de Bogotá desde Wikipedia

wiki_url <- "https://es.wikipedia.org/wiki/Anexo:Barrios_de_Bogot%C3%A1"
page_html <- read_html(wiki_url)
my_table_node <- html_node(page_html, ".wikitable")
wiki_data <- html_table(my_table_node, fill = TRUE)

# Función para normalizar nombres
norm <- function(x) stringr::str_squish(stringi::stri_trans_general(stringr::str_to_lower(x), "Latin-ASCII"))

# Normalizamos nombres de columnas
nombres_norm <- norm(names(wiki_data))

# Detectamos automáticamente las columnas relevantes
col_barrio <- names(wiki_data)[which(stringr::str_detect(nombres_norm, "^barrio$|^barrios$"))][1]
col_localidad <- names(wiki_data)[which(stringr::str_detect(nombres_norm, "^localidad$"))][1]
col_upz <- names(wiki_data)[which(nombres_norm == "unidad de planeamiento zonal")][1]

# Verificamos que encontramos todas las columnas
if (length(col_barrio) != 1 || length(col_localidad) != 1 || length(col_upz) != 1) {
  stop("No ubicamos correctamente las columnas Barrio, Localidad y Unidad de Planeamiento Zonal en wiki_data.")
}

# Limpiamos y procesamos la tabla de Wikipedia
wiki_tbl <- wiki_data %>%
  transmute(
    barrio_raw    = .data[[col_barrio]],
    localidad_raw = .data[[col_localidad]],
    upz_raw       = .data[[col_upz]]
  ) %>%
  mutate(across(everything(), ~ . %>% 
                  stringr::str_to_lower() %>% 
                  stringi::stri_trans_general("Latin-ASCII") %>% 
                  stringr::str_squish())) %>%
  tidyr::separate_rows(barrio_raw, sep = ",") %>%
  mutate(barrio_raw = stringr::str_squish(barrio_raw)) %>%
  filter(!is.na(barrio_raw), barrio_raw != "") %>%
  mutate(
    barrio_key = barrio_raw %>%
      stringr::str_replace_all("[[:punct:]]", " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace("^(el|la|los|las)\\s+", ""),
    localidad_clean = localidad_raw %>%
      stringr::str_replace_all("\\d+", "") %>%
      stringr::str_squish(),
    upz_codigo = stringr::str_extract(upz_raw, "\\d+"),
    upz_nombre = upz_raw %>%
      stringr::str_replace_all("(^|\\s)upz\\s*\\d*", " ") %>%
      stringr::str_replace_all("\\d+", "") %>%
      stringr::str_squish()
  ) %>%
  distinct(barrio_key, .keep_all = TRUE) %>%
  arrange(desc(nchar(barrio_key)))  # Preferimos coincidencias largas

# Convertimos a data.table y establecemos clave
setDT(wiki_tbl)
setkey(wiki_tbl, barrio_key)



# =============================================================================
# 10. ADICIÓN DE BARRIOS ADICIONALES NO INCLUIDOS EN WIKIPEDIA
# =============================================================================
# Añadimos barrio extra: floralia
barrios_extra <- data.table(
  barrio_raw    = "floralia",
  localidad_raw = NA_character_,
  upz_raw       = NA_character_
)

# Aplicamos la misma limpieza que a wiki_tbl
barrios_extra[, `:=`(
  barrio_raw = barrio_raw |> stringr::str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> stringr::str_squish(),
  barrio_key = barrio_raw |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish() |>
    stringr::str_replace("^(el|la|los|las)\\s+", ""),
  localidad_raw = localidad_raw |> stringr::str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> stringr::str_squish(),
  localidad_clean = localidad_raw |> stringr::str_replace_all("\\d+", "") |> stringr::str_squish(),
  upz_raw = upz_raw |> stringr::str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> stringr::str_squish(),
  upz_codigo = ifelse(is.na(upz_raw), NA_character_, stringr::str_extract(upz_raw, "\\d+")),
  upz_nombre = ifelse(is.na(upz_raw), NA_character_,
                      upz_raw |>
                        stringr::str_replace_all("(^|\\s)upz\\s*\\d*", " ") |>
                        stringr::str_replace_all("\\d+", "") |>
                        stringr::str_squish())
)]

# Unimos las tablas y reordenamos
wiki_tbl <- unique(rbindlist(list(wiki_tbl, barrios_extra), fill = TRUE))
setkey(wiki_tbl, barrio_key)
wiki_tbl <- wiki_tbl[order(-nchar(barrio_key))]

# =============================================================================
# 11. DETECCIÓN DE BARRIOS EN TÍTULOS Y DESCRIPCIONES
# =============================================================================
# Construimos patrón regex con todos los barrios
barrios_pat <- stringr::str_replace_all(wiki_tbl$barrio_key, "\\s+", "\\\\s+")
patron_regex <- paste0("\\b(?:", paste0(barrios_pat, collapse = "|"), ")\\b")

# Detectamos barrio en title normalizado
train[, barrio_detectado_title := stringr::str_extract(title_norm, patron_regex)]

# Detectamos barrio en description normalizada
train[, barrio_detectado_description := stringr::str_extract(desc_norm, patron_regex)]

# Priorizamos detección en title sobre description
train[, barrio_detectado := data.table::fifelse(!is.na(barrio_detectado_title),
                                                barrio_detectado_title,
                                                barrio_detectado_description)]

# =============================================================================
# 12. ASIGNACIÓN DE LOCALIDAD Y UPZ POR BARRIO
# =============================================================================
# Hacemos join con la tabla de Wikipedia para asignar localidad y UPZ
train[wiki_tbl, on = .(barrio_detectado = barrio_key),
      `:=`(localidad_raw  = i.localidad_raw,   # Con número
           localidad      = i.localidad_clean, # Sin número
           upz            = i.upz_raw,         # Texto completo
           upz_nombre     = i.upz_nombre,      # Solo nombre
           upz_codigo     = i.upz_codigo,      # Solo código
           barrio_oficial = i.barrio_raw)]

# Creamos respaldo de UPZ por localidad para casos sin barrio detectado
upz_por_localidad <- wiki_tbl[, .N, by = .(localidad_raw, upz_raw)][
  order(localidad_raw, -N)][
    , .SD[1], by = localidad_raw][
      , .(localidad_raw, upz_raw)]
setkey(upz_por_localidad, localidad_raw)

# Imputamos UPZ usando localidad cuando no tenemos barrio
train[is.na(upz) & !is.na(localidad_raw),
      upz_alt := upz_por_localidad[.SD, on = .(localidad_raw), x.upz_raw]]
train[, upz := data.table::fifelse(!is.na(upz), upz, upz_alt)]
train[, upz_alt := NULL]

# =============================================================================
# 13. CREACIÓN DE DUMMY DE APARTAESTUDIO
# =============================================================================
# Detectamos la columna de property type de forma flexible
col_prop <- names(train)[stringr::str_detect(stringr::str_to_lower(names(train)),
                                             "^property[\\._ ]?type$|\\bproperty[\\._ ]?type\\b")]

if (length(col_prop) > 0) {
  col_prop <- col_prop[1]
  
  # Normalizamos el tipo de propiedad
  train[, prop_norm := train[[col_prop]] |>
          as.character() |>
          stringr::str_to_lower() |>
          stringi::stri_trans_general("Latin-ASCII") |>
          stringr::str_squish()]
  
  # Creamos dummy: apartamento con área <= 40 m2
  train[, dummy_apartaestudio := data.table::fifelse(
    prop_norm == "apartamento" & !is.na(area_m2) & area_m2 <= 40,
    1L, 0L
  )]
  
  train[, prop_norm := NULL]
} else {
  message("No encontramos la columna property type; creamos dummy_apartaestudio como NA.")
  train[, dummy_apartaestudio := NA_integer_]
}

# =============================================================================
# 14. DIAGNÓSTICO DE PROCESAMIENTO
# =============================================================================
cat("Filas con barrio detectado:", sum(!is.na(train$barrio_detectado)), "de", nrow(train), "\n")
cat("Filas con localidad asignada:", sum(!is.na(train$localidad_raw)), "de", nrow(train), "\n")
cat("Filas con UPZ asignada:", sum(!is.na(train$upz)), "de", nrow(train), "\n")
cat("Filas con area_reportada:", sum(!is.na(train$area_reportada)), "de", nrow(train), "\n")
cat("Filas con area_m2:", sum(!is.na(train$area_m2)), "de", nrow(train), "\n")
cat("Filas corregidas por regla 4-5 dígitos:", sum(idx_corr), "\n")
cat("Filas con dummy_apartaestudio = 1:", sum(train$dummy_apartaestudio == 1, na.rm = TRUE), "\n")

# Mostramos muestra de las filas procesadas
print(train[1:10, .(title, barrio_detectado, localidad_raw, localidad, upz, upz_nombre, upz_codigo, area_reportada, area_m2, dummy_apartaestudio)])
print(train[is.na(area_reportada)][1:10, .(title, description, desc_norm)])

# =============================================================================
# 15. CARGA DE DATOS DE CRIMINALIDAD POR LOCALIDAD
# =============================================================================
# Definimos ruta del archivo Excel con datos de criminalidad
#excel_path <- "C:\\Users\\fevid\\Documents\\MEcA\\Big Data\\Taller_3\\datos_taller_3\\crimen_bogota.xlsx"
#excel_path <- "crimen_bogota.xlsx"
excel_path <- "crimen_bogota.xlsx"

# Leemos todas las hojas del archivo Excel
lista_excel <- excel_sheets(excel_path) %>%
  set_names() %>%
  map(~ read_excel(excel_path, sheet = .x))

# Normalizamos la columna localidad en todas las hojas
lista_excel <- lapply(lista_excel, function(df) {
  if ("localidad" %in% names(df)) {
    df %>%
      mutate(
        localidad = str_to_lower(localidad),
        localidad = str_replace_all(localidad, "^[0-9]+-\\s*", ""),
        localidad = str_squish(localidad),
        localidad = stri_trans_general(localidad, "Latin-ASCII")
      )
  } else {
    df
  }
})

# Extraemos hoja de hurto a personas
hom_pe <- lista_excel %>% 
  pluck(which(str_detect(names(.), "_pe")))

head(hom_pe)

# Extraemos hoja de hurto a residencias
hurt_re <- lista_excel %>% 
  pluck(which(str_detect(names(.), "_re")))

# Extraemos hoja de homicidios
homicidios <- lista_excel %>% 
  pluck(which(str_detect(names(.), "homicidios")))

# =============================================================================
# 16. INTEGRACIÓN DE DATOS DE CRIMINALIDAD
# =============================================================================
# Unimos datos de hurto a personas

train <- train %>%
  left_join(
    hom_pe %>%
      select(localidad, year, 
             num_hurto_pe_anual, 
             perc_variacion_hurto_pe),
    by = c("localidad", "year")
  )

# Unimos datos de hurto a residencias
train <- train %>%
  left_join(
    hurt_re %>%
      select(localidad, year, 
             num_hurto_re_anual, 
             perc_variacion_hurto_re),
    by = c("localidad", "year")
  )

# Unimos datos de homicidios
train <- train %>%
  left_join(
    homicidios %>%
      select(localidad, year, 
             num_homicidios_anual, 
             perc_variacion_homicidios),
    by = c("localidad", "year")
  )

# =============================================================================
# 17. CARGA Y PROCESAMIENTO DE DATOS DE ESTRATOS
# =============================================================================
# Cargamos archivo con información de estratos por barrio
# estratos <- read_excel('C:\\Users\\fevid\\Documents\\MEcA\\Big Data\\Taller_3\\datos_taller_3\\manzanas_con_barrio.xlsx')
# estratos <- read_excel("manzanas_con_barrio.xlsx")
estratos <- read_excel("manzanas_con_barrio.xlsx")

head(estratos)

# Limpiamos espacios en blanco
estratos <- estratos %>%
  mutate(across(where(is.character), str_squish))

# Función para normalizar texto
normalize <- function(x) {
  x |> str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

# Función para quitar artículos y palabras neutras
strip_words <- function(x) {
  x |> str_replace_all("\\b(el|la|los|las|de|del|y)\\b", "") |> str_squish()
}

# Preparamos diccionario de estratos
estratos_norm <- estratos %>%
  mutate(barrio_key = strip_words(normalize(barrio))) %>%
  distinct(barrio_key, .keep_all = TRUE) %>%
  select(barrio_key, estrato)

# Preparamos train para join
train_norm <- train %>%
  mutate(barrio_key = strip_words(normalize(barrio_oficial)))

# Hacemos join exacto sobre la clave normalizada
train_match2 <- train_norm %>%
  left_join(estratos_norm, by = "barrio_key") %>%
  select(-barrio_key)

# Verificamos barrios sin estrato asignado
train_match2  %>% 
  select(barrio_oficial, estrato) %>% 
  filter(!is.na(barrio_oficial) & is.na(estrato))

# Calculamos frecuencia de barrios sin estrato
freq_na <- train_match2  %>%
  filter(!is.na(barrio_oficial), is.na(estrato)) %>%
  count(barrio_oficial, name = "n_na") %>% 
  arrange(desc(n_na))

freq_na

# Calculamos porcentaje de NA por barrio (top 35)
top10_pct_global <- train_match2 %>%
  filter(!is.na(barrio_oficial), is.na(estrato)) %>%
  count(barrio_oficial, name = "n_na") %>%
  mutate(pct_na = n_na / sum(n_na),
         pct_na = percent(pct_na, accuracy = 0.1)) %>%
  arrange(desc(n_na)) %>%
  slice_head(n = 35)

top10_pct_global

# Formateamos variable estrato con prefijo
train_match2 <- train_match2 %>%
  mutate(estrato = if_else(!is.na(estrato),
                           paste0('Estrato_', estrato),
                           NA_character_))

# =============================================================================
# 18. CREACIÓN DE DUMMIES DESDE DESCRIPCIONES
# =============================================================================

# Dummy de garaje/parqueadero
pat <- "\\b(parqueadero|parqueaderos|garaje|garajes|estacionamiento|estacionamientos|garage|garages)\\b"

train_match2 <- train_match2 %>%
  mutate(
    dummy_garaje = as.integer(
      coalesce(str_detect(description, regex(pat, ignore_case = TRUE)), FALSE)
    )
  )

# Dummy de terraza
pat_2 <- "\\b(terraza|terrazas)\\b"

train_match2 <- train_match2 %>%
  mutate(
    dummy_terraza = as.integer(
      coalesce(str_detect(description, regex(pat_2, 
                                             ignore_case = TRUE)), FALSE)
    )
  )

# =============================================================================
# 19. EXTRACCIÓN DE NÚMERO DE BAÑOS
# =============================================================================
# Patrón para detectar la palabra baño y variantes
pres_pat <- "\\b(?:ba(?:ñ|n)o(?:s)?)\\b"

# Patrón para extraer número a la izquierda de "baño"
num_pat <- "\\b(\\d{1,3})\\s*(?:[-–—]?\\s*)?(?:ba(?:ñ|n)o(?:s)?)\\b"

# Construimos variable de baños totales
train_match2 <- train_match2 %>%
  mutate(
    # Convertimos bathrooms a entero si es posible
    bathrooms_num = suppressWarnings(as.numeric(bathrooms)),
    bathrooms_int = if_else(
      !is.na(bathrooms_num) & bathrooms_num %% 1 == 0,
      as.integer(bathrooms_num),
      NA_integer_
    ),
    
    # Extraemos número de la descripción
    num_left = suppressWarnings(as.integer(
      str_match(description, regex(num_pat, ignore_case = TRUE))[, 2]
    )),
    
    # Detectamos presencia de la palabra "baño"
    has_bath_word = coalesce(
      str_detect(description, regex(pres_pat, ignore_case = TRUE)),
      FALSE
    ),
    
    # Aplicamos regla de construcción: bathrooms > description > presencia
    banos_tot = case_when(
      !is.na(bathrooms_int) ~ bathrooms_int,
      !is.na(num_left) ~ num_left,
      has_bath_word ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-bathrooms_num, -bathrooms_int, -num_left, -has_bath_word)


