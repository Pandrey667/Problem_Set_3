remove(list = ls())

library(sf)
library(dplyr)


ruta <- "C:\\Users\\fevid\\Downloads\\manzanaestratificacion"

manzanas <- st_read(ruta)

ruta_1 <- "C:\\Users\\fevid\\Downloads\\sector.shp.0925"

barrios <- st_read(ruta_1)

ruta_2 <- "C:\\Users\\fevid\\Downloads\\barriolegalizado"

barrio_leg <- st_read(ruta_2)


# Ver cuántas geometrías inválidas hay
sum(!st_is_valid(barrios))
sum(!st_is_valid(manzanas))

# Repararlas
barrios  <- st_make_valid(barrios)
manzanas <- st_make_valid(manzanas)

st_crs(barrios)
st_crs(manzanas)

barrios   <- st_transform(barrios, 3116)
manzanas  <- st_transform(manzanas, 3116)

st_crs(barrios) == st_crs(manzanas)

sum(!st_is_valid(barrios))
sum(!st_is_valid(manzanas))

sum(st_intersects(barrios, manzanas, sparse = FALSE))

plot(st_geometry(barrios), col = NA, border = 'blue')
plot(st_geometry(manzanas), col = NA, border = 'red', add = TRUE)


manzanas <- manzanas %>% 
  select(ESTRATO, geometry)

manzanas <- st_transform(manzanas, st_crs(barrios))

db_new <- st_join(barrios, 
                  manzanas, 
                  join = st_within, 
                  left = TRUE)


db_new <- st_join(barrios, manzanas, join = st_intersects, left = TRUE)

modal_fun <- function(x) {
  tb <- table(x)
  mx <- max(tb)
  as.integer(names(tb)[tb == mx][1])  # rompe empates tomando el primero
}

db_new_2 <- db_new %>% 
  group_by(SCANOMBRE) %>%
  summarise(ESTRATO = raster::modal(ESTRATO), .groups = "drop")
