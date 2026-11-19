# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 1: Análisis Exploratorio de Datos
# Script 01: diagnóstico de la base cruda
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Qué hace este script: lee la base tal como llegó, revisa su estructura y
# chequea una por una las particularidades que detectamos al mirarla. No
# modifica nada ni escribe datos: solo diagnostica. Las transformaciones van
# en el script 02.
# =============================================================================

library(tidyverse)
library(skimr)

# Todo el output de consola se guarda en un txt para poder revisarlo después.
sink("output/tablas/01_diagnostico.txt", split = TRUE)
options(width = 130)

cat("\n\n############ 1. CARGA Y ESTRUCTURA ############\n\n")

base <- read_csv("raw/base_ventas_caba.csv", show_col_types = FALSE)
dicc <- read_csv("auxiliar/diccionario_variables.csv", show_col_types = FALSE)

cat("Dimensiones de la base:", nrow(base), "filas x", ncol(base), "columnas\n")
cat("Variables en el diccionario:", nrow(dicc), "\n")
cat("¿Coinciden los nombres base/diccionario?:",
    setequal(names(base), dicc$variable), "\n\n")

glimpse(base)

cat("\n--- ¿El id identifica unívocamente al aviso? ---\n")
cat("ids duplicados:", sum(duplicated(base$id)), "\n")
cat("urls duplicadas:", sum(duplicated(base$url)), "\n")

cat("\n--- Chequeo de codificación (acentos) ---\n")
print(head(sort(unique(base$barrio)), 10))
print(head(unique(base$zona_aviso), 5))

cat("\n\n############ 2. VALORES FALTANTES ############\n\n")

faltantes <- base |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "na") |>
  mutate(pct_na = round(100 * na / nrow(base), 2)) |>
  arrange(desc(na))

print(faltantes, n = Inf)

cat("\n--- ¿los NA de inmobiliaria son los particulares? ---\n")
print(table(base$publicado_por, is.na(base$inmobiliaria), useNA = "ifany"))
cat("NA de inmobiliaria:", sum(is.na(base$inmobiliaria)), "\n")
cat("Avisos de particulares:", sum(base$publicado_por == "particular", na.rm = TRUE), "\n")
cat("¿Son exactamente los mismos avisos?:",
    identical(which(is.na(base$inmobiliaria)),
              which(base$publicado_por == "particular")), "\n")

cat("\n--- ¿los NA de antiguedad_max son 'Más de 50 años'? ---\n")
print(table(base$antiguedad_cat, is.na(base$antiguedad_max), useNA = "ifany"))
cat("NA de antiguedad_max:", sum(is.na(base$antiguedad_max)), "\n")
cat("¿Los NA coinciden con una sola categoría?:\n")
print(base |> filter(is.na(antiguedad_max)) |> count(antiguedad_cat))

cat("\n--- ¿Cuántas inmobiliarias distintas publican? ---\n")
cat("inmobiliarias distintas:", n_distinct(base$inmobiliaria, na.rm = TRUE), "\n")
print(base |> filter(!is.na(inmobiliaria)) |> count(inmobiliaria, sort = TRUE) |> head(15))


cat("\n\n############ 3. VARIABLES CATEGÓRICAS ############\n\n")

for (v in c("operacion", "moneda_publicacion", "tipo_depto", "antiguedad_cat",
            "antiguedad_grupo", "a_estrenar", "publicado_por",
            "publicacion_reciente", "linea_subte_cercana", "comuna")) {
  cat("\n--- ", v, " ---\n", sep = "")
  print(base |> count(.data[[v]], sort = TRUE) |>
          mutate(pct = round(100 * n / sum(n), 1)))
}

cat("\n--- ¿las categorías de antigüedad se superponen? ---\n")
print(
  base |>
    group_by(antiguedad_cat) |>
    summarise(
      n = n(),
      min_declarado = min(antiguedad_min, na.rm = TRUE),
      max_declarado = ifelse(all(is.na(antiguedad_max)), NA,
                             max(antiguedad_max, na.rm = TRUE)),
      media = first(antiguedad_media),
      grupo = first(antiguedad_grupo),
      .groups = "drop"
    ) |>
    arrange(min_declarado),
  n = Inf
)

cat("\n--- barrios: cantidad y distribución (distribución de avisos entre barrios) ---\n")
cat("barrios distintos:", n_distinct(base$barrio), "\n")
tabla_barrios <- base |>
  count(barrio, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 2),
         acum = round(cumsum(pct), 1))
print(tabla_barrios, n = Inf)


cat("\n\n############ 4. VARIABLES NUMÉRICAS: DESCRIPTIVAS ############\n\n")

vars_num <- c("precio_usd", "precio_m2_usd", "metros_cubiertos",
              "metros_por_ambiente", "ambientes", "dormitorios", "banos",
              "antiguedad_media", "dist_centro_m", "dist_subte_m",
              "dist_espacio_verde_m", "espacios_verdes_1000m",
              "superficie_verde_1000m_m2", "dist_escuela_m", "escuelas_1000m",
              "dist_comisaria_m", "dist_parada_colectivo_m",
              "paradas_colectivo_1000m", "lineas_colectivo_1000m",
              "geocod_score", "valor_m2_usd_barrio")

descriptivas <- base |>
  select(all_of(vars_num)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  filter(!is.na(valor)) |>
  group_by(variable) |>
  summarise(
    n = n(),
    media = mean(valor),
    mediana = median(valor),
    sd = sd(valor),
    cv = sd(valor) / mean(valor),
    min = min(valor),
    p10 = quantile(valor, 0.10),
    p25 = quantile(valor, 0.25),
    p75 = quantile(valor, 0.75),
    p90 = quantile(valor, 0.90),
    p99 = quantile(valor, 0.99),
    max = max(valor),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(descriptivas, n = Inf, width = Inf)

cat("\n--- Asimetría: comparación media vs mediana ---\n")
print(descriptivas |>
        mutate(ratio_media_mediana = round(media / mediana, 2)) |>
        select(variable, media, mediana, ratio_media_mediana) |>
        arrange(desc(ratio_media_mediana)), n = Inf)

cat("\n--- Ratios entre percentiles (desigualdad de la distribución) ---\n")
print(descriptivas |>
        mutate(p90_p10 = round(p90 / p10, 2),
               p75_p25 = round(p75 / p25, 2)) |>
        select(variable, p10, p50 = mediana, p90, p90_p10, p75_p25) |>
        arrange(desc(p90_p10)), n = Inf)


cat("\n\n############ 5. ¿LA BASE VIENE RECORTADA? ############\n\n")

cat("--- Máximos y mínimos exactos de las variables clave ---\n")
for (v in c("precio_usd", "precio_m2_usd", "metros_cubiertos", "ambientes",
            "dormitorios", "banos", "geocod_score", "antiguedad_media")) {
  cat(sprintf("%-22s  min = %12.2f   max = %12.2f\n", v,
              min(base[[v]], na.rm = TRUE), max(base[[v]], na.rm = TRUE)))
}

cat("\n--- ¿Hay amontonamiento en el techo? (cantidad de avisos en el máximo) ---\n")
for (v in c("precio_usd", "metros_cubiertos", "ambientes", "dormitorios", "banos")) {
  mx <- max(base[[v]], na.rm = TRUE)
  cat(sprintf("%-20s  máximo = %10.1f   avisos en el máximo = %5d (%.2f%%)\n",
              v, mx, sum(base[[v]] == mx, na.rm = TRUE),
              100 * mean(base[[v]] == mx, na.rm = TRUE)))
}

cat("\n--- Valores más altos de metros_cubiertos y precio (los 10 últimos) ---\n")
print(base |> count(metros_cubiertos) |> arrange(desc(metros_cubiertos)) |> head(10))
print(base |> count(precio_usd) |> arrange(desc(precio_usd)) |> head(10))
print(base |> count(precio_usd) |> arrange(precio_usd) |> head(10))
print(base |> count(metros_cubiertos) |> arrange(metros_cubiertos) |> head(10))

cat("\n--- geocod_score: ¿nunca baja de 80? ---\n")
cat("mínimo:", min(base$geocod_score, na.rm = TRUE),
    " | avisos con score = 100:", sum(base$geocod_score == 100, na.rm = TRUE), "\n")

cat("\n--- Outliers por la regla de 1,5 RIC (a pesar del recorte previo) ---\n")
for (v in c("precio_usd", "precio_m2_usd", "metros_cubiertos")) {
  x <- base[[v]]
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  ric <- q[2] - q[1]
  cat(sprintf("%-18s  límite sup = %10.1f   atípicos altos = %5d (%.2f%%)   atípicos bajos = %4d\n",
              v, q[2] + 1.5 * ric, sum(x > q[2] + 1.5 * ric, na.rm = TRUE),
              100 * mean(x > q[2] + 1.5 * ric, na.rm = TRUE),
              sum(x < q[1] - 1.5 * ric, na.rm = TRUE)))
}


cat("\n\n############ 6. AVISOS REPETIDOS ############\n\n")

dups <- base |>
  group_by(titulo_aviso, precio_usd, metros_cubiertos) |>
  mutate(n_copias = n()) |>
  ungroup()

cat("Avisos que comparten título + precio + superficie con al menos otro:",
    sum(dups$n_copias > 1), "\n")
cat("Grupos de repetición distintos:",
    dups |> filter(n_copias > 1) |> distinct(titulo_aviso, precio_usd, metros_cubiertos) |> nrow(), "\n")

cat("\nDistribución del tamaño de los grupos repetidos:\n")
print(dups |> filter(n_copias > 1) |> count(n_copias, name = "avisos"))

cat("\n--- ¿Los repetidos comparten también barrio y dirección? ---\n")
print(
  dups |>
    filter(n_copias > 1) |>
    group_by(titulo_aviso, precio_usd, metros_cubiertos) |>
    summarise(copias = n(),
              barrios_distintos = n_distinct(barrio),
              direcciones_distintas = n_distinct(direccion),
              .groups = "drop") |>
    count(barrios_distintos, direcciones_distintas)
)

cat("\n--- Ejemplos concretos de repetición ---\n")
ejemplos <- dups |>
  filter(n_copias > 1) |>
  arrange(desc(n_copias), titulo_aviso) |>
  select(id, titulo_aviso, precio_usd, metros_cubiertos, ambientes, barrio,
         direccion, publicado_por, inmobiliaria, n_copias)
print(ejemplos |> head(20), width = Inf)

cat("\n--- ¿Cambia algo el promedio si los sacamos? ---\n")
sin_dup <- dups |>
  distinct(titulo_aviso, precio_usd, metros_cubiertos, .keep_all = TRUE)
cat("Con repetidos   : n =", nrow(base),
    " precio medio =", round(mean(base$precio_usd), 0),
    " precio/m2 medio =", round(mean(base$precio_m2_usd), 1), "\n")
cat("Sin repetidos   : n =", nrow(sin_dup),
    " precio medio =", round(mean(sin_dup$precio_usd), 0),
    " precio/m2 medio =", round(mean(sin_dup$precio_m2_usd), 1), "\n")

cat("\n--- ¿Los repetidos se concentran en algún barrio o publicante? ---\n")
print(dups |> mutate(rep = n_copias > 1) |> count(publicado_por, rep) |>
        pivot_wider(names_from = rep, values_from = n))
print(dups |> filter(n_copias > 1) |> count(barrio, sort = TRUE) |> head(10))

cat("\n--- Chequeo alternativo: repetición por dirección + precio + superficie ---\n")
dups2 <- base |>
  group_by(direccion, precio_usd, metros_cubiertos) |>
  mutate(n_copias = n()) |> ungroup()
cat("Avisos repetidos bajo este criterio:", sum(dups2$n_copias > 1), "\n")


cat("\n\n############ 7. COHERENCIA INTERNA ############\n\n")

cat("--- ¿precio_m2_usd = precio_usd / metros_cubiertos? ---\n")
chequeo <- base |>
  mutate(dif = abs(precio_m2_usd - precio_usd / metros_cubiertos))
cat("máxima diferencia absoluta:", max(chequeo$dif, na.rm = TRUE), "\n")

cat("\n--- ¿log_precio_usd = log(precio_usd)? ---\n")
cat("máxima diferencia:", max(abs(base$log_precio_usd - log(base$precio_usd)), na.rm = TRUE), "\n")

cat("\n--- ¿precio_ars = precio_usd * 1132? ---\n")
cat("máxima diferencia:", max(abs(base$precio_ars - base$precio_usd * 1132), na.rm = TRUE), "\n")

cat("\n--- tipo_depto contra dormitorios ---\n")
print(table(base$tipo_depto, base$dormitorios, useNA = "ifany"))

cat("\n--- ambientes contra dormitorios ---\n")
print(table(base$ambientes, base$dormitorios, useNA = "ifany"))

cat("\n--- valor_m2_usd_barrio: ¿un único valor por barrio? ---\n")
print(base |> group_by(barrio) |>
        summarise(valores_distintos = n_distinct(valor_m2_usd_barrio),
                  valor = first(valor_m2_usd_barrio), .groups = "drop") |>
        arrange(desc(valor)), n = Inf)

cat("\n--- barrio contra comuna: ¿cada barrio cae en una sola comuna? ---\n")
print(base |> group_by(barrio) |>
        summarise(comunas = n_distinct(comuna), .groups = "drop") |>
        count(comunas))

cat("\n--- zona_aviso: ¿cuántas zonas declaradas hay y coinciden con el barrio? ---\n")
cat("zonas distintas declaradas:", n_distinct(base$zona_aviso), "\n")
print(base |> count(zona_aviso, sort = TRUE) |> head(15))


cat("\n\n############ 8. PRIMERAS RELACIONES ############\n\n")

cat("--- Precio por m2 por barrio (top 10 y bottom 10) ---\n")
resumen_barrio <- base |>
  group_by(barrio) |>
  summarise(
    avisos = n(),
    precio_mediano = median(precio_usd),
    m2_mediano = median(metros_cubiertos),
    precio_m2_mediano = median(precio_m2_usd),
    precio_m2_medio = mean(precio_m2_usd),
    dist_centro_km = round(median(dist_centro_m) / 1000, 1),
    referencia_externa = first(valor_m2_usd_barrio),
    .groups = "drop"
  ) |>
  arrange(desc(precio_m2_mediano))

print(resumen_barrio, n = Inf, width = Inf)

cat("\n--- Correlación entre precio_m2 mediano por barrio y la referencia externa ---\n")
cat("Pearson :", round(cor(resumen_barrio$precio_m2_mediano,
                           resumen_barrio$referencia_externa), 3), "\n")
cat("Spearman:", round(cor(resumen_barrio$precio_m2_mediano,
                           resumen_barrio$referencia_externa,
                           method = "spearman"), 3), "\n")

cat("\n--- Matriz de correlaciones entre las variables de la pregunta ---\n")
vars_cor <- base |>
  select(precio_usd, log_precio_usd, precio_m2_usd, metros_cubiertos, ambientes,
         dormitorios, banos, antiguedad_media, dist_centro_m, dist_subte_m,
         dist_espacio_verde_m, espacios_verdes_1000m, lineas_colectivo_1000m,
         escuelas_1000m)

cat("\nPearson:\n")
print(round(cor(vars_cor, use = "pairwise.complete.obs"), 2))
cat("\nSpearman:\n")
print(round(cor(vars_cor, use = "pairwise.complete.obs", method = "spearman"), 2))

cat("\n--- ¿Cuánta variación del precio/m2 es entre barrios y cuánta adentro? ---\n")
# Descriptivo: comparación de la dispersión total contra la dispersión de las
# medias de barrio. NO es un modelo, es una descomposición de varianza.
var_total <- var(base$precio_m2_usd)
medias_b <- base |> group_by(barrio) |> summarise(m = mean(precio_m2_usd), n = n())
var_entre <- sum(medias_b$n * (medias_b$m - mean(base$precio_m2_usd))^2) / nrow(base)
cat("Varianza total       :", round(var_total, 0), "\n")
cat("Varianza entre barrios:", round(var_entre, 0),
    sprintf(" (%.1f%% del total)", 100 * var_entre / var_total), "\n")
cat("Varianza dentro       :", round(var_total - var_entre, 0),
    sprintf(" (%.1f%% del total)", 100 * (var_total - var_entre) / var_total), "\n")

cat("\n--- Lo mismo para el precio total ---\n")
var_total_p <- var(base$precio_usd)
medias_p <- base |> group_by(barrio) |> summarise(m = mean(precio_usd), n = n())
var_entre_p <- sum(medias_p$n * (medias_p$m - mean(base$precio_usd))^2) / nrow(base)
cat("% entre barrios:", sprintf("%.1f%%", 100 * var_entre_p / var_total_p), "\n")

cat("\n--- Precio/m2 según tramo de superficie ---\n")
print(base |>
        mutate(tramo_m2 = cut(metros_cubiertos,
                              breaks = c(0, 35, 50, 70, 100, 150, 400),
                              labels = c("hasta 35", "36-50", "51-70",
                                         "71-100", "101-150", "más de 150"))) |>
        group_by(tramo_m2) |>
        summarise(avisos = n(),
                  precio_mediano = median(precio_usd),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop"))

cat("\n--- Precio/m2 según quién publica ---\n")
print(base |> group_by(publicado_por) |>
        summarise(avisos = n(),
                  m2_mediano = median(metros_cubiertos),
                  precio_mediano = median(precio_usd),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop"))

cat("\n--- Precio/m2 según antigüedad ---\n")
print(base |> group_by(antiguedad_grupo) |>
        summarise(avisos = n(),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  m2_mediano = median(metros_cubiertos),
                  .groups = "drop"))

cat("\n--- Precio/m2 según distancia al centro (deciles) ---\n")
print(base |>
        mutate(decil_centro = ntile(dist_centro_m, 10)) |>
        group_by(decil_centro) |>
        summarise(avisos = n(),
                  km_centro = round(median(dist_centro_m) / 1000, 1),
                  precio_m2_mediano = round(median(precio_m2_usd), 0),
                  .groups = "drop"))

cat("\n\n############ 9. SKIM GENERAL ############\n\n")
print(skim(base))

sink()
cat("Diagnóstico terminado. Ver output/tablas/01_diagnostico.txt\n")
