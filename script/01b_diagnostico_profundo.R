# =============================================================================
# TP Ciencia de Datos - Instancia 1
# Script 01b: chequeos adicionales que salieron del primer diagnóstico
#
# El script 01 dejó cinco cosas sin resolver:
#   1. La distancia al centro no ordena los precios. ¿El eje es norte-sur?
#   2. Villa Soldati aparece con un precio/m2 altísimo y solo 42 avisos.
#   3. Los avisos repetidos son dos fenómenos distintos, no uno.
#   4. ambientes y dormitorios no son consistentes entre sí.
#   5. El 38% de los avisos es "a estrenar". ¿Es plausible?
# =============================================================================

library(tidyverse)

sink("output/tablas/01b_diagnostico_profundo.txt", split = TRUE)
options(width = 130)

base <- read_csv("raw/base_ventas_caba.csv", show_col_types = FALSE)


cat("\n\n#### 1. ¿EL EJE ES NORTE-SUR Y NO CENTRO-PERIFERIA? ####\n\n")

cat("--- Correlación del precio/m2 con la posición geográfica ---\n")
cat("con latitud (norte = mayor lat) :", round(cor(base$precio_m2_usd, base$lat), 3), "\n")
cat("con longitud (este = mayor long):", round(cor(base$precio_m2_usd, base$long), 3), "\n")
cat("con distancia al centro         :", round(cor(base$precio_m2_usd, base$dist_centro_m), 3), "\n")

cat("\n--- Precio/m2 por decil de latitud ---\n")
print(base |>
        mutate(decil_lat = ntile(lat, 10)) |>
        group_by(decil_lat) |>
        summarise(avisos = n(),
                  lat_mediana = round(median(lat), 4),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop"))

cat("\n--- Precio/m2 mediano por comuna, ordenado ---\n")
print(base |>
        group_by(comuna) |>
        summarise(avisos = n(),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  lat_mediana = round(median(lat), 3),
                  .groups = "drop") |>
        arrange(desc(precio_m2_mediano)), n = Inf)

cat("\n--- Combinación: mitad norte vs mitad sur, por tramo de distancia al centro ---\n")
print(base |>
        mutate(mitad = if_else(lat > median(lat), "Norte de la ciudad", "Sur de la ciudad"),
               tramo_centro = cut(dist_centro_m / 1000, c(0, 3, 6, 9, 15),
                                  labels = c("0-3 km", "3-6 km", "6-9 km", "más de 9 km"))) |>
        group_by(tramo_centro, mitad) |>
        summarise(avisos = n(),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop") |>
        pivot_wider(names_from = mitad, values_from = c(avisos, precio_m2_mediano)))


cat("\n\n#### 2. EL CASO VILLA SOLDATI ####\n\n")

print(base |>
        filter(barrio == "VILLA SOLDATI") |>
        count(zona_aviso, sort = TRUE))
print(base |>
        filter(barrio == "VILLA SOLDATI") |>
        select(id, precio_usd, metros_cubiertos, precio_m2_usd, direccion,
               zona_aviso, geocod_score) |>
        arrange(desc(precio_m2_usd)) |>
        head(15), width = Inf)

cat("\n--- ¿Cuántos barrios tienen menos de 100 avisos? ---\n")
print(base |> count(barrio) |> filter(n < 100) |> arrange(n))

cat("\n--- Discrepancia entre el barrio oficial y la zona declarada en el aviso ---\n")
# Normalizamos ambos textos (mayúsculas, sin tildes) para poder compararlos.
normalizar <- function(x) {
  x |> str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}
comparacion <- base |>
  mutate(barrio_n = normalizar(barrio),
         zona_n   = normalizar(zona_aviso),
         coincide = str_detect(zona_n, fixed(barrio_n)) |
                    str_detect(barrio_n, fixed(zona_n)))
cat("Avisos donde la zona declarada contiene al barrio oficial (o al revés):",
    sum(comparacion$coincide, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(comparacion$coincide, na.rm = TRUE)), "\n")
cat("\nBarrios con mayor tasa de NO coincidencia (mínimo 100 avisos):\n")
print(comparacion |>
        group_by(barrio) |>
        summarise(avisos = n(),
                  pct_no_coincide = round(100 * mean(!coincide, na.rm = TRUE), 1),
                  .groups = "drop") |>
        filter(avisos >= 100) |>
        arrange(desc(pct_no_coincide)) |> head(15))


cat("\n\n#### 3. REPETIDOS: EFECTO SOBRE LOS DESCRIPTIVOS ####\n\n")

# Acá había una primera clasificación de los avisos repetidos que comparaba el
# texto de la dirección hasta la primera coma. Esa comparación incluye el
# identificador de unidad, así que daba vuelta los dos casos: contaba como
# "unidades distintas" a los avisos que solo cambian el sufijo de barrio.
# La clasificación buena, la que usamos, está en script/02_preparacion.R y se
# apoya en el identificador de unidad. Borramos la vieja para que nadie saque
# de acá números que contradicen los de la presentación.
#
# Lo que sigue no dependía de esa clasificación y sigue siendo válido: cuánto
# se mueven los descriptivos si se sacan TODAS las copias repetidas.

cat("--- Efecto de sacar las copias redundantes sobre los descriptivos ---\n")
sin_dup <- base |> distinct(titulo_aviso, precio_usd, metros_cubiertos, .keep_all = TRUE)
comparar <- bind_rows(
  base    |> summarise(version = "Base completa", n = n(),
                       precio_mediano = median(precio_usd),
                       precio_m2_mediano = round(median(precio_m2_usd), 0),
                       m2_mediano = median(metros_cubiertos),
                       pct_palermo = round(100 * mean(barrio == "PALERMO"), 2)),
  sin_dup |> summarise(version = "Sin copias redundantes", n = n(),
                       precio_mediano = median(precio_usd),
                       precio_m2_mediano = round(median(precio_m2_usd), 0),
                       m2_mediano = median(metros_cubiertos),
                       pct_palermo = round(100 * mean(barrio == "PALERMO"), 2))
)
print(comparar, width = Inf)


cat("\n\n#### 4. ambientes CONTRA dormitorios ####\n\n")

cat("La convención habitual es ambientes = dormitorios + 1 (los ambientes incluyen\n")
cat("el living). Chequeamos cuántos avisos la cumplen.\n\n")
print(base |>
        mutate(dif = ambientes - dormitorios) |>
        count(dif) |>
        mutate(pct = round(100 * n / sum(n), 1)))

cat("\n--- Casos imposibles bajo esa convención ---\n")
cat("ambientes <= dormitorios:", sum(base$ambientes <= base$dormitorios), "\n")
cat("  de los cuales monoambientes con 1 dormitorio:",
    sum(base$ambientes == 1 & base$dormitorios == 1), "\n")

cat("\n--- ¿De dónde sale cada variable? tipo_depto contra ambientes ---\n")
print(base |> count(tipo_depto, ambientes) |>
        pivot_wider(names_from = ambientes, values_from = n, values_fill = 0))

cat("\n--- metros_por_ambiente: casos extremos ---\n")
print(base |> arrange(desc(metros_por_ambiente)) |>
        select(id, metros_cubiertos, ambientes, dormitorios, metros_por_ambiente,
               precio_usd, titulo_aviso) |> head(8), width = Inf)


cat("\n\n#### 5. EL 38% 'A ESTRENAR' ####\n\n")

cat("--- ¿Cómo se reparten los a estrenar entre barrios? ---\n")
print(base |>
        group_by(barrio) |>
        summarise(avisos = n(),
                  pct_estrenar = round(100 * mean(a_estrenar), 1),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop") |>
        filter(avisos >= 200) |>
        arrange(desc(pct_estrenar)), n = Inf)

cat("\n--- ¿Quién publica los a estrenar? ---\n")
print(base |> count(publicado_por, a_estrenar) |>
        group_by(publicado_por) |>
        mutate(pct = round(100 * n / sum(n), 1)))

cat("\n--- Precio/m2 por antigüedad y por quién publica ---\n")
print(base |> group_by(antiguedad_grupo, publicado_por) |>
        summarise(avisos = n(),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop") |>
        pivot_wider(names_from = publicado_por,
                    values_from = c(avisos, precio_m2_mediano)))

cat("\n--- ¿Los a estrenar son más chicos? ---\n")
print(base |> group_by(antiguedad_grupo) |>
        summarise(avisos = n(),
                  m2_mediano = median(metros_cubiertos),
                  ambientes_mediano = median(ambientes),
                  precio_mediano = median(precio_usd),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop"))


cat("\n\n#### 6. OTROS CHEQUEOS ####\n\n")

cat("--- Filas idénticas en todas las columnas menos id y url ---\n")
cat(sum(duplicated(base |> select(-id, -url))), "\n")

cat("\n--- Precio/m2 por tipo de departamento ---\n")
print(base |> group_by(tipo_depto) |>
        summarise(avisos = n(),
                  m2_mediano = median(metros_cubiertos),
                  precio_mediano = median(precio_usd),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop") |>
        arrange(m2_mediano))

cat("\n--- Precio/m2 por tramo fino de superficie (la U) ---\n")
print(base |>
        mutate(tramo = cut(metros_cubiertos,
                           breaks = c(14, 25, 35, 45, 55, 70, 90, 120, 160, 400))) |>
        group_by(tramo) |>
        summarise(avisos = n(),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  precio_mediano = median(precio_usd),
                  pct_estrenar = round(100 * mean(a_estrenar), 1),
                  .groups = "drop"))

cat("\n--- Concentración: ¿qué porción de los avisos explican los 5 barrios top? ---\n")
top <- base |> count(barrio, sort = TRUE) |> mutate(pct = 100 * n / sum(n))
cat("Top 5 barrios :", round(sum(head(top$pct, 5)), 1), "% de los avisos\n")
cat("Top 10 barrios:", round(sum(head(top$pct, 10)), 1), "% de los avisos\n")
cat("Comuna 8 (Soldati, Lugano, Riachuelo):",
    round(100 * mean(base$comuna == 8), 2), "% de los avisos\n")

cat("\n--- Distancia al subte: ¿cuántos avisos están lejos? ---\n")
print(base |>
        mutate(tramo_subte = cut(dist_subte_m, c(-1, 300, 600, 1000, 2000, Inf),
                                 labels = c("hasta 300 m", "300-600 m", "600-1000 m",
                                            "1-2 km", "más de 2 km"))) |>
        group_by(tramo_subte) |>
        summarise(avisos = n(),
                  pct = round(100 * n() / nrow(base), 1),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop"))

sink()
cat("Listo. Ver output/tablas/01b_diagnostico_profundo.txt\n")
