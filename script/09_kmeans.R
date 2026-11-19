# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 2: métodos estadísticos y cuantitativos
# Script 09: kmeans (sección 4 de la consigna)
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: input/base_analisis.csv, output/tablas/08_prima_superficie_barrio.csv
# Salida : output/tablas/09_kmeans.txt, output/tablas/09_*.csv,
#          output/graficos/g16 a g18, output/tablas/t16 a t18
#
# Método: clase 16, script 04 (kmeans, PAM y comparación de particiones).
# Las reglas del script de la clase, que acá se cumplen todas:
#   - scale() siempre antes, porque kmeans usa distancia euclídea;
#   - nstart = 50;
#   - k elegido con codo, silueta y gap, sabiendo que no tienen por qué
#     coincidir;
#   - un segundo algoritmo (PAM) comparado con adjustedRandIndex();
#   - perfiles en unidades originales.
#
# LA DECISIÓN QUE ORDENA TODO EL SCRIPT: la ubicación NO entra al clustering.
# Agrupamos los avisos por lo que son (precio por m², tamaño, antigüedad) y
# después miramos dónde caen los grupos. Si metiéramos la latitud o la
# distancia al centro, que los grupos se parezcan a los barrios sería en parte
# circular: lo habríamos puesto nosotros. Así, el cruce con barrio y comuna que
# pidió el docente es una prueba y no una tautología. Es la misma lógica de las
# variables suplementarias del PCA del script 07.
#
# mclust NO se carga con library(): tapa count() de dplyr y map() de purrr.
# Se usa mclust::adjustedRandIndex() con el nombre completo.
# =============================================================================

library(tidyverse)
library(cluster)    # clara(), silhouette(), clusGap()
source("script/00_tema.R")

base <- read_csv("input/base_analisis.csv", show_col_types = FALSE)
prima_barrio <- read_csv("output/tablas/08_prima_superficie_barrio.csv",
                         show_col_types = FALSE)

# ari() está en utils/ari.R (la carga 00_tema.R).

sink("output/tablas/09_kmeans.txt", split = TRUE)
options(width = 140)


# =============================================================================
# 1. LA MUESTRA Y LAS VARIABLES
# =============================================================================
# La misma muestra que la regresión (38.895 avisos, 42 barrios), para que los
# resultados de los dos métodos se puedan poner uno al lado del otro.
#
# Tres variables, todas sobre el aviso:
#   - log(precio por m²): lo que la regresión explica con la ubicación;
#   - log(superficie): lo que explica el precio total;
#   - tramo de antigüedad, de 0 (a estrenar) a 3 (más de 50 años): la
#     característica que más pesa en el precio por m².
#
# Logaritmos en precio y superficie por lo mismo que en la sección 2: la
# asimetría. Kmeans promedia, y un promedio se deja arrastrar por la cola.
#
# La antigüedad entra como el tramo de antiguedad_grupo (0 a 3), la misma
# variable que usamos en todo el trabajo (decisión 4), y no como
# antiguedad_media: esa sale del punto medio de categorías que se pisan. El
# costo: tratar los tramos como equidistantes. scale() lo deja en la misma
# escala que las otras dos.
#
# Descartamos ambientes y baños: correlacionan 0,88 y 0,74 con la superficie.
# Con las tres adentro, el tamaño contaría tres veces en la distancia.

cat("#### 1. MUESTRA Y VARIABLES ####\n\n")

barrios_chicos <- base |> count(barrio_nom) |> filter(n < 100)
d <- base |>
  filter(!no_es_depto, !barrio_nom %in% barrios_chicos$barrio_nom) |>
  mutate(tramo_antig = match(antiguedad, c("A estrenar", "Hasta 20 años",
                                           "20 a 50 años", "Más de 50 años")) - 1)
stopifnot(nrow(d) == 38895, !anyNA(d$tramo_antig))
n_mod <- nrow(d)

X <- d |> transmute(lpm2 = log(precio_m2_usd), lm2 = log(metros_cubiertos),
                    tramo_antig) |> as.data.frame()
etiq_vars <- c(lpm2 = "Log precio por m²", lm2 = "Log superficie",
               tramo_antig = "Tramo de antigüedad")

cat("Avisos:", num_ar(n_mod), "\n\n--- Correlaciones entre las tres variables ---\n")
print(round(cor(X), 2))

Z <- scale(X)   # kmeans usa distancia euclídea: sin esto manda la de mayor varianza


# =============================================================================
# 2. ELEGIR k
# =============================================================================
# Codo con los 38.895 avisos. Silueta y gap con una muestra aleatoria de 5.000:
# la silueta necesita todas las distancias entre pares, y con 38.895 avisos son
# 756 millones (unos 6 GB). El gap corre kmeans B veces por cada k.
#
# kmeans avisa "Quick-TRANSfer stage steps exceeded" en algunos arranques: la
# antigüedad tiene solo cuatro valores y hay muchos empates. Lo que importa es
# que el arranque que se queda (el mejor de los 50) haya convergido, y eso se
# verifica con ifault.

cat("\n\n#### 2. ELEGIR k ####\n\n")

set.seed(2026)
ks <- 2:8
corre_km <- function(Z, k) suppressWarnings(kmeans(Z, k, nstart = 50, iter.max = 50))

kms <- map(ks, \(k) corre_km(Z, k))
codo <- tibble(k = ks,
               wss = map_dbl(kms, "tot.withinss"),
               entre = map_dbl(kms, \(m) m$betweenss / m$totss),
               convergio = map_lgl(kms, \(m) m$ifault == 0))
print(codo)

# La silueta se calcula sobre las particiones de los 38.895 avisos, mirando
# solo los 5.000 de la muestra. No se vuelve a correr kmeans sobre la muestra:
# así es la silueta de los grupos que presentamos y no la de otros parecidos.
idx_m <- sample(n_mod, 5000)
Zm <- Z[idx_m, ]
dist_m <- dist(Zm)
silueta <- tibble(k = ks, silueta = map_dbl(kms, function(m) {
  mean(silhouette(m$cluster[idx_m], dist_m)[, "sil_width"])
}))
print(silueta)

# B = 100 simulaciones, como en la clase. Tarda unos minutos.
gap <- suppressWarnings(clusGap(Zm, FUN = kmeans, nstart = 25, iter.max = 50,
                                K.max = max(ks), B = 100, verbose = FALSE))
gap_tab <- as_tibble(gap$Tab) |> mutate(k = row_number())
k_gap <- maxSE(gap_tab$gap, gap_tab$SE.sim, method = "firstSEmax")
print(gap, method = "firstSEmax")

criterios <- codo |> left_join(silueta, by = "k") |>
  left_join(select(gap_tab, k, gap, se_gap = SE.sim), by = "k")
write_csv(criterios, "output/tablas/09_criterios_k.csv")

k_sil     <- ks[which.max(silueta$silueta)]
k_gap_max <- gap_tab$k[which.max(gap_tab$gap)]
# Por cuánto gana el k del gap: la regla firstSEmax elige el primer k cuyo gap
# no queda más de un error estándar por debajo del siguiente.
margen_gap <- gap_tab$gap[k_gap] - (gap_tab$gap[k_gap + 1] - gap_tab$SE.sim[k_gap + 1])
cat("\nk según la silueta:", k_sil, "\n")
cat("k según el gap, regla firstSEmax:", k_gap, " (margen:", signif(margen_gap, 2),
    ")   máximo del gap en k =", k_gap_max, "\n")

# El gap entre 2 y 3 está en el filo. Con dos muestras al azar distintas, la
# regla firstSEmax eligió una vez 2 y otra 3: la diferencia entre los dos es
# del tamaño de un error estándar. Por eso no lo leemos como apoyo a ningún k.
gap_dif23 <- gap_tab$gap[3] - gap_tab$gap[2]
gap_se3   <- gap_tab$SE.sim[3]
gap_empate <- abs(gap_dif23) < 2 * gap_se3
cat("Gap(3) − gap(2):", signif(gap_dif23, 2), "  error estándar:", signif(gap_se3, 2),
    if (gap_empate) " -> empate\n" else "\n")

# El codo no se resume en un número. Tomar la caída más grande no sirve: la
# suma de cuadrados dentro cae con rendimientos decrecientes, así que la
# primera caída es siempre la mayor y esa regla devolvería 3 con cualquier base.
# Se mira la curva, y la nuestra baja pareja, sin un quiebre marcado.
cat("Caídas de la suma de cuadrados al sumar un grupo:\n")
print(tibble(de = ks[-length(ks)], a = ks[-1], caida = -diff(codo$wss),
             caida_relativa = round(-diff(codo$wss) / head(codo$wss, -1), 3)))

# --- La elección -------------------------------------------------------------
# Los criterios no coinciden, como avisa la clase. La silueta prefiere 2. El
# gap no distingue entre 2 y 3. El codo no decide. Cuando los criterios no coinciden, elegir k
# es una decisión analítica (clase 16), y la nuestra es de interpretabilidad:
# con 3 los grupos se distinguen por antigüedad y por tamaño a la vez; con 2 la
# partición es prácticamente una sola variable, la antigüedad.
K <- 3
cat("k elegido:", K, "\n")


# =============================================================================
# 3. KMEANS CON EL k ELEGIDO
# =============================================================================
# Los grupos se renumeran de menor a mayor precio por m² mediano: kmeans los
# numera al azar y así el grupo 1 es siempre el más barato.

cat("\n\n#### 3. KMEANS, k =", K, "####\n\n")

km <- kms[[which(ks == K)]]
orden <- d |> mutate(g = km$cluster) |> group_by(g) |>
  summarise(m = median(precio_m2_usd)) |> arrange(m) |> pull(g)
d$grupo <- match(km$cluster, orden)

cat("Convergió:", km$ifault == 0, "\n")
cat("Variabilidad entre grupos / total:", coma_ar(100 * km$betweenss / km$totss), "%\n")
print(table(d$grupo))

# La silueta de los grupos que presentamos, sobre los 5.000 de la muestra.
# Entre 0,25 y 0,5 se considera estructura débil: los grupos son una
# descripción útil de la base, no una partición natural. Va en limitaciones.
sil_k <- silhouette(d$grupo[idx_m], dist_m)
sil_media <- mean(sil_k[, "sil_width"])
cat("Silueta media (muestra de 5.000):", coma_ar(sil_media, 3), "\n")
cat("Avisos de la muestra con silueta negativa:",
    coma_ar(100 * mean(sil_k[, "sil_width"] < 0)), "%\n")
print(summary(sil_k)$clus.avg.widths)

cat("\n--- Centros en unidades estandarizadas ---\n")
print(round(km$centers[orden, ], 2))


# =============================================================================
# 4. PERFILES EN UNIDADES ORIGINALES
# =============================================================================
# Medianas, como en todas las comparaciones entre grupos (decisión 6). Las
# variables de ubicación no entraron al clustering: se describen después.

cat("\n\n#### 4. PERFILES ####\n\n")

perfil <- d |>
  group_by(grupo) |>
  summarise(
    avisos        = n(),
    pct           = 100 * n() / n_mod,
    precio_m2     = median(precio_m2_usd),
    m2            = median(metros_cubiertos),
    precio        = median(precio_usd),
    ambientes     = median(ambientes),
    pct_estrenar  = 100 * mean(antiguedad == "A estrenar"),
    pct_mas50     = 100 * mean(antiguedad == "Más de 50 años"),
    # Lo que no entró al clustering:
    pct_norte     = 100 * mean(mitad_ciudad == "Norte"),
    dist_km       = median(dist_centro_m) / 1000,
    pct_inmob     = 100 * mean(publicado_por == "Inmobiliaria"),
    .groups = "drop"
  )
print(perfil |> mutate(across(where(is.double), ~ round(.x, 1))), width = Inf)
write_csv(perfil, "output/tablas/09_perfiles.csv")

cat("\n--- Antigüedad por grupo (%) ---\n")
print(round(100 * prop.table(table(d$grupo, d$antiguedad), 1)[, c(
  "A estrenar", "Hasta 20 años", "20 a 50 años", "Más de 50 años")], 1))

cat("\n--- Los cinco barrios con más avisos en cada grupo ---\n")
top_barrios <- d |> count(grupo, barrio_nom) |> group_by(grupo) |>
  mutate(pct = 100 * n / sum(n)) |> slice_max(n, n = 5) |> ungroup()
print(top_barrios, n = Inf)


# =============================================================================
# 5. SEGUNDO ALGORITMO: PAM
# =============================================================================
# PAM usa como centro un aviso real (el medoide) y minimiza distancias, no
# distancias al cuadrado: es más robusto a los extremos. pam() necesita la
# matriz de distancias completa, que con 38.895 avisos no entra en memoria.
# clara() es PAM para bases grandes: corre PAM sobre muestras y asigna el resto
# al medoide más cercano. Con samples = 50 y pamLike = TRUE es el mismo
# algoritmo de PAM aplicado por muestras.

cat("\n\n#### 5. PAM (clara) ####\n\n")

set.seed(2026)
pm <- clara(Z, K, samples = 50, sampsize = 1000, pamLike = TRUE, rngR = TRUE)
orden_pm <- d |> mutate(g = pm$clustering) |> group_by(g) |>
  summarise(m = median(precio_m2_usd)) |> arrange(m) |> pull(g)
d$grupo_pam <- match(pm$clustering, orden_pm)

print(table(kmeans = d$grupo, pam = d$grupo_pam))
ari_pam <- ari(d$grupo, d$grupo_pam)
cat("Coincidencia kmeans / PAM (ARI):", coma_ar(ari_pam, 3), "\n")
cat("Avisos en el mismo grupo con los dos:", coma_ar(100 * mean(d$grupo == d$grupo_pam)), "%\n")

cat("\n--- Los medoides: un aviso real que representa a cada grupo ---\n")
print(d[pm$i.med, ] |> mutate(grupo_pam = match(seq_len(K), orden_pm)) |>
        select(grupo_pam, precio_usd, precio_m2_usd, metros_cubiertos, antiguedad, barrio_nom) |>
        arrange(grupo_pam))


# =============================================================================
# 6. SENSIBILIDAD
# =============================================================================
# La consigna pide las limitaciones: escala, outliers, cantidad de grupos.
# Cada una con un número.

cat("\n\n#### 6. SENSIBILIDAD ####\n\n")

# Escala: el mismo kmeans sin scale(). El tramo de antigüedad va de 0 a 3 y su
# desvío es mucho mayor que el de los logaritmos, así que pasa a mandar.
km_sin_escala <- suppressWarnings(kmeans(X, K, nstart = 50, iter.max = 50))
ari_escala <- ari(d$grupo, km_sin_escala$cluster)
cat("Desvío de cada variable sin estandarizar:\n"); print(round(sapply(X, sd), 2))
cat("ARI con / sin estandarizar:", coma_ar(ari_escala, 3), "\n")

# Outliers: el ARI con PAM ya es la prueba (PAM es robusto). Además, kmeans sin
# los avisos más extremos (más de 3 desvíos en alguna variable estandarizada).
extremo <- apply(abs(Z), 1, max) > 3
km_sin_ext <- suppressWarnings(kmeans(Z[!extremo, ], K, nstart = 50, iter.max = 50))
# Para comparar, se asigna cada aviso extremo al centro más cercano.
asignar <- function(Z, centros) apply(Z, 1, \(z) which.min(colSums((t(centros) - z)^2)))
grupo_sin_ext <- asignar(Z, km_sin_ext$centers)
ari_ext <- ari(d$grupo, grupo_sin_ext)
cat("Avisos con más de 3 desvíos en alguna variable:", num_ar(sum(extremo)),
    sprintf("(%s%%)", coma_ar(100 * mean(extremo))), "\n")
cat("ARI con / sin extremos:", coma_ar(ari_ext, 3), "\n")

# Cantidad de grupos: ¿k = 4 parte uno de los tres grupos o reordena todo?
km4 <- kms[[which(ks == 4)]]
cat("\n--- k = 3 contra k = 4 ---\n")
print(table(k3 = d$grupo, k4 = km4$cluster))
ari_k4 <- ari(d$grupo, km4$cluster)
ari_k2 <- ari(d$grupo, kms[[which(ks == 2)]]$cluster)
cat("ARI k = 3 / k = 2:", coma_ar(ari_k2, 3), "  ARI k = 3 / k = 4:", coma_ar(ari_k4, 3), "\n")


# =============================================================================
# 7. ¿LOS GRUPOS SE CORRESPONDEN CON EL BARRIO O LA COMUNA?
# =============================================================================
# Lo pidió el docente: table() más adjustedRandIndex().
#
# Un cuidado al leer el ARI: compara particiones con distinta cantidad de
# grupos (3 contra 42 barrios o 15 comunas). Aunque cada barrio tuviera un solo
# tipo de departamento, el ARI no llegaría a 1, porque 3 grupos no pueden
# reproducir 42. Por eso se acompaña con lo que muestra la tabla: cómo se
# reparte cada barrio entre los grupos.
#
# comuna es un entero del 1 al 15 en la base. Acá entra como factor: nunca
# como número.

cat("\n\n#### 7. GRUPOS CONTRA BARRIO Y COMUNA ####\n\n")

d$comuna_f <- factor(d$comuna)
ari_barrio <- ari(d$grupo, d$barrio_nom)
ari_comuna <- ari(d$grupo, d$comuna_f)
ari_mitad  <- ari(d$grupo, d$mitad_ciudad)
cat("ARI grupos / barrio :", coma_ar(ari_barrio, 3), "\n")
cat("ARI grupos / comuna :", coma_ar(ari_comuna, 3), "\n")
cat("ARI grupos / mitad norte-sur:", coma_ar(ari_mitad, 3), "\n")

cat("\n--- Comuna por grupo (% de cada comuna) ---\n")
print(round(100 * prop.table(table(d$comuna_f, d$grupo), 1), 1))

cat("Comunas que tienen avisos de los", K, "grupos:",
    sum(rowSums(table(d$comuna_f, d$grupo) > 0) == K), "de", nlevels(d$comuna_f), "\n")
cat("Barrios que tienen avisos de los", K, "grupos:",
    sum(rowSums(table(d$barrio_nom, d$grupo) > 0) == K), "de", n_distinct(d$barrio_nom), "\n")

# Cada barrio: qué parte de sus avisos cae en cada grupo, junto a la prima que
# le estimó la regresión.
#
# OJO, esto es casi circular: el kmeans usa log(precio/m²) y la prima del
# barrio sale del precio. Que los barrios de prima alta tengan más avisos en el
# grupo caro es esperable por construcción. El Spearman se reporta como dato
# secundario, con esa aclaración, y NO como evidencia independiente.
barrio_grupo <- d |>
  count(barrio_nom, grupo) |>
  group_by(barrio_nom) |> mutate(pct = 100 * n / sum(n)) |> ungroup() |>
  left_join(select(prima_barrio, barrio_nom, prima_pct), by = "barrio_nom")
write_csv(barrio_grupo, "output/tablas/09_barrio_grupo.csv")

pct_caro <- barrio_grupo |> filter(grupo == K) |>
  right_join(distinct(select(barrio_grupo, barrio_nom, prima_pct)),
             by = c("barrio_nom", "prima_pct")) |>
  mutate(pct = coalesce(pct, 0))
cor_caro_prima <- cor(pct_caro$pct, pct_caro$prima_pct, method = "spearman")
pct_barato <- barrio_grupo |> filter(grupo == 1) |>
  right_join(distinct(select(barrio_grupo, barrio_nom, prima_pct)),
             by = c("barrio_nom", "prima_pct")) |>
  mutate(pct = coalesce(pct, 0))
cor_barato_prima <- cor(pct_barato$pct, pct_barato$prima_pct, method = "spearman")
cat("\nSpearman entre la prima del barrio (regresión) y el % de sus avisos en el grupo",
    K, ":", coma_ar(cor_caro_prima, 2), "\n")
cat("Spearman entre la prima del barrio y el % en el grupo 1:",
    coma_ar(cor_barato_prima, 2), "\n")

# --- Dentro de un mismo tipo, ¿el barrio sigue ordenando el precio? -----------
# Lo que el kmeans agrega a la regresión. Si la prima de un barrio viniera de
# que tiene otro tipo de departamentos, al comparar avisos del MISMO grupo las
# diferencias entre barrios tendrían que achicarse mucho. Comparamos, dentro de
# cada grupo, el precio por m² mediano de cada barrio (solo barrios con al
# menos 30 avisos en ese grupo) contra la prima que le estimó la regresión.
cat("\n--- Dentro de cada grupo: precio por m² por barrio contra la prima ---\n")
dentro <- d |>
  group_by(grupo, barrio_nom) |>
  summarise(avisos = n(), pm2 = median(precio_m2_usd), .groups = "drop") |>
  filter(avisos >= 30) |>
  left_join(select(prima_barrio, barrio_nom, prima_pct), by = "barrio_nom")
dentro_res <- dentro |>
  group_by(grupo) |>
  summarise(barrios = n(),
            spearman = cor(pm2, prima_pct, method = "spearman"),
            max_sobre_min = max(pm2) / min(pm2), .groups = "drop")
print(dentro_res |> mutate(across(where(is.double), ~ round(.x, 2))))
cat("Para comparar, en toda la base el barrio más caro sobre el más barato:",
    coma_ar(with(d |> group_by(barrio_nom) |> summarise(m = median(precio_m2_usd)),
                 max(m) / min(m)), 2), "\n")
write_csv(dentro, "output/tablas/09_dentro_grupo.csv")

# El ejemplo concreto: barrios caros con mucho stock viejo. Recoleta y Retiro
# tienen prima alta y cerca de la mitad de sus avisos en el grupo 1, el de los
# viejos y baratos. Un barrio caro no es un barrio de departamentos caros:
# conviven los dos tipos. Y el mismo tipo vale distinto según el barrio.
cat("\n--- Recoleta y Retiro contra Balvanera, dentro de cada grupo ---\n")
ejemplo <- d |>
  filter(barrio_nom %in% c("Recoleta", "Retiro", "Balvanera")) |>
  group_by(barrio_nom, grupo) |>
  summarise(avisos = n(), pm2 = median(precio_m2_usd), m2 = median(metros_cubiertos),
            .groups = "drop_last") |>
  mutate(pct = 100 * avisos / sum(avisos)) |> ungroup()
print(ejemplo |> mutate(across(where(is.double), ~ round(.x, 1))))
write_csv(ejemplo, "output/tablas/09_ejemplo_recoleta.csv")

# Lo que el chequeo de arriba dice, en una línea: dentro de cada tipo el barrio
# sigue ordenando el precio, pero la brecha entre el más caro y el más barato
# se achica. Una parte de la diferencia entre barrios es qué tipo de
# departamento se publica en cada uno; la mayor parte se sostiene a igual tipo.
rango_total <- with(d |> group_by(barrio_nom) |> summarise(m = median(precio_m2_usd)),
                    max(m) / min(m))

comparacion <- tibble(
  comparacion = c("kmeans contra PAM", "kmeans con y sin estandarizar",
                  "kmeans con y sin extremos", "k = 3 contra k = 2", "k = 3 contra k = 4",
                  "Grupos contra barrio", "Grupos contra comuna",
                  "Grupos contra mitad norte-sur"),
  ari = c(ari_pam, ari_escala, ari_ext, ari_k2, ari_k4, ari_barrio, ari_comuna, ari_mitad)
)
write_csv(comparacion, "output/tablas/09_comparacion_ari.csv")

# Los números que van a los textos de la presentación (script 10).
guardar_numeros(
  "09_numeros.csv",
  K = K, k_sil = k_sil, gap_empate = as.integer(gap_empate),
  gap_dif23 = gap_dif23, gap_se3 = gap_se3,
  silueta = sil_media, silueta_k2 = silueta$silueta[silueta$k == 2],
  sil_neg = 100 * mean(sil_k[, "sil_width"] < 0),
  entre = 100 * km$betweenss / km$totss,
  pct_igual_pam = 100 * mean(d$grupo == d$grupo_pam),
  n_extremos = sum(extremo),
  comunas_todos = sum(rowSums(table(d$comuna_f, d$grupo) > 0) == K),
  n_comunas = nlevels(d$comuna_f),
  barrios_todos = sum(rowSums(table(d$barrio_nom, d$grupo) > 0) == K),
  n_barrios = n_distinct(d$barrio_nom),
  cor_caro_prima = cor_caro_prima, cor_barato_prima = cor_barato_prima,
  rango_total = rango_total
)

sink()
cat("\nDiagnóstico guardado en output/tablas/09_kmeans.txt\n")


# =============================================================================
# 8. GRÁFICOS
# =============================================================================

colores_grupo <- c(`1` = colores$acento2, `2` = colores$apagado, `3` = colores$acento,
                   `4` = "#8C6D4F", `5` = "#5B5F66")[seq_len(K)]
nombre_grupo <- paste("Grupo", seq_len(K))

# --- G16. Los tres criterios para elegir k -----------------------------------
# Cada panel dice en el título qué k prefiere ese criterio, para que no parezca
# que los tres apuntan al k elegido. "k = 3" se escribe con espacios que no
# cortan, para que el envoltorio del título no lo parta entre renglones.
k_txt <- function(k) paste0("k = ", k)
crit_largo <- bind_rows(
  transmute(criterios, k, valor = wss / 1000,
            criterio = "Codo, suma de cuadrados dentro (miles): la curva no tiene un quiebre marcado"),
  transmute(criterios, k, valor = silueta,
            criterio = paste0("Silueta media: prefiere ", k_txt(k_sil))),
  transmute(criterios, k, valor = gap,
            criterio = if (gap_empate)
              paste0("Gap: no distingue entre ", k_txt(2), " y ", k_txt(3), " (a un error estándar)")
              else paste0("Gap: la regla de la clase elige ", k_txt(k_gap)))
) |> mutate(criterio = factor(criterio, levels = unique(criterio)),
            elegido = k == K)

# El panel del gap existe para mostrar cuán cerca están 2 y 3, así que el eje
# tiene que llegar por encima de los dos: un punto invisible lo extiende.
techo_gap <- tibble(criterio = factor(levels(crit_largo$criterio)[3], levels = levels(crit_largo$criterio)),
                    k = 2, valor = max(criterios$gap) + 0.012)
# Sin decimales en el codo (miles de suma de cuadrados); dos en silueta y gap.
etiq_y <- function(x) if (max(x, na.rm = TRUE) > 5) coma_ar(x, 0) else coma_ar(x, 2)
# Marcas cada 0,02 en silueta y gap: las automáticas ponían una en 0,675, que
# con dos decimales se leía "0,68".
marcas_y <- function(lim) {
  if (lim[2] > 5) return(pretty(lim))
  seq(ceiling(lim[1] * 50) / 50, floor(lim[2] * 50) / 50, by = 0.02)
}
# Los dos valores que el panel del gap tiene que dejar leer, escritos al lado
# de sus puntos con tres decimales.
gap_23 <- crit_largo |>
  filter(criterio == levels(criterio)[3], k %in% 2:3) |>
  mutate(texto = coma_ar(valor, 3))

g16 <- ggplot(crit_largo, aes(k, valor)) +
  geom_vline(xintercept = K, color = colores$acento, linewidth = 0.6, linetype = "22") +
  geom_line(color = colores$secundario, linewidth = 0.6) +
  geom_point(aes(color = elegido), size = 2.4) +
  geom_blank(data = techo_gap) +
  geom_text(data = gap_23, aes(label = texto), vjust = -1, size = 3,
            family = fuente_cuerpo, color = colores$secundario) +
  scale_color_manual(values = c(`TRUE` = colores$acento, `FALSE` = colores$secundario),
                     guide = "none") +
  scale_x_continuous(breaks = ks) +
  scale_y_continuous(labels = etiq_y, breaks = marcas_y) +
  facet_wrap(~ criterio, scales = "free_y", nrow = 1,
             labeller = label_wrap_gen(width = 34)) +
  labs(x = "Cantidad de grupos (k)", y = NULL,
       caption = pie(paste0(fuente_pie, ". Codo con los ", num_ar(n_mod),
                            " avisos. Silueta de esas mismas particiones, medida sobre una muestra aleatoria de 5.000; ",
                            "gap sobre la misma muestra. En terracota, el k elegido: ", K,
                            ". Los criterios no coinciden; la elección es por interpretabilidad."))) +
  theme(strip.text = element_text(color = colores$texto, size = 10, hjust = 0))

guardar(g16, "g16_criterios_k.png", ancho = 11, alto = 4.2)

# --- G17. Los grupos en el plano de un PCA sobre las tres variables ----------
# Es el gráfico de fviz_cluster: los dos primeros componentes de un PCA sobre
# las MISMAS variables del clustering. No es el PCA de cercanía del script 07.
pca_k <- prcomp(Z)
var_pc <- 100 * pca_k$sdev^2 / sum(pca_k$sdev^2)
set.seed(2026)
idx_g <- sample(n_mod, 8000)
puntos <- tibble(PC1 = pca_k$x[idx_g, 1], PC2 = pca_k$x[idx_g, 2],
                 grupo = factor(d$grupo[idx_g]))
# Las flechas muestran hacia dónde crece cada variable en el plano.
cargas <- as_tibble(pca_k$rotation[, 1:2] * 2.6, rownames = "var") |>
  mutate(etiqueta = etiq_vars[var],
         lx = PC1 * 1.12, ly = PC2 * 1.12, hj = 0.5)
# La etiqueta de la antigüedad caía encima de la nube del grupo 1. Va afuera de
# la nube, a la izquierda, unida a la punta de la flecha por una línea fina.
fuera_x <- min(puntos$PC1) - 0.15
cargas <- cargas |>
  mutate(lx = if_else(var == "tramo_antig", fuera_x, lx),
         hj = if_else(var == "tramo_antig", 1, hj))

g17 <- ggplot(puntos, aes(PC1, PC2, color = grupo)) +
  geom_jitter(size = 0.6, alpha = 0.35, width = 0.04, height = 0.04) +
  geom_segment(data = cargas, aes(0, 0, xend = PC1, yend = PC2), inherit.aes = FALSE,
               color = colores$texto, linewidth = 0.5,
               arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  geom_segment(data = filter(cargas, var == "tramo_antig"),
               aes(PC1, PC2, xend = lx, yend = ly), inherit.aes = FALSE,
               color = colores$texto, linewidth = 0.3, linetype = "dotted") +
  geom_label(data = cargas, aes(lx, ly, label = etiqueta, hjust = hj),
             inherit.aes = FALSE, size = 3, family = fuente_cuerpo,
             color = colores$texto, fill = colores$fondo, linewidth = 0) +
  # Lugar a la izquierda para la etiqueta que quedó afuera de la nube.
  expand_limits(x = fuera_x - 1.9) +
  scale_color_manual(values = colores_grupo, labels = nombre_grupo) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_fixed() +
  labs(x = paste0("Componente 1: ", coma_ar(var_pc[1]), "% de la varianza"),
       y = paste0("Componente 2: ", coma_ar(var_pc[2]), "% de la varianza"),
       # coord_fixed angosta el gráfico: el pie se corta antes que en los demás.
       # Los grupos son cortes de un continuo, no nubes separadas: es lo que se
       # ve y lo que dice la silueta. Mejor decirlo en la imagen misma.
       caption = pie(paste0(fuente_pie, ". Muestra aleatoria de ", num_ar(length(idx_g)),
                            " avisos. PCA sobre las tres variables del clustering, estandarizadas; ",
                            "no es el PCA de las variables de cercanía. Los grupos son cortes de un ",
                            "continuo, no nubes separadas: la silueta media es ", coma_ar(sil_media, 2), "."),
                     ancho = 95))

guardar(g17, "g17_grupos_pca.png", ancho = 9, alto = 6.4)

# --- G18. Cómo se reparte cada barrio entre los grupos -----------------------
# Barrios ordenados por la prima que les estimó la regresión: arriba los más
# caros. Lo que muestra: los tres tipos de departamento están en casi todos los
# barrios, y barrios caros como Recoleta y Retiro tienen cerca de la mitad de
# sus avisos en el grupo viejo y barato. Que la franja del grupo caro crezca
# hacia arriba es esperable por construcción (el kmeans usa el precio por m²) y
# NO se presenta como evidencia independiente. Recoleta y Retiro se marcan en el
# eje porque son el ejemplo del texto.
orden_barrios <- prima_barrio |> arrange(prima_pct) |> pull(barrio_nom)
marcar <- c("Recoleta", "Retiro")
# Lo que dice el pie: qué marcan las flechas y dónde una franja casi no se ve.
pct_marcados <- barrio_grupo |> filter(barrio_nom %in% marcar, grupo == 1) |>
  arrange(match(barrio_nom, marcar)) |> pull(pct)
stopifnot(all(between(pct_marcados, 40, 60)))   # "casi la mitad"
franja_min <- barrio_grupo |> slice_min(pct, n = 1)
g18_datos <- barrio_grupo |>
  mutate(barrio_nom = factor(barrio_nom, levels = orden_barrios),
         grupo = factor(grupo, levels = rev(seq_len(K))))

g18 <- ggplot(g18_datos, aes(pct, barrio_nom, fill = grupo)) +
  geom_col(width = 0.78) +
  scale_fill_manual(values = colores_grupo, labels = setNames(nombre_grupo, seq_len(K)),
                    breaks = as.character(seq_len(K))) +
  scale_x_continuous(labels = \(x) paste0(num_ar(x), "%"), expand = expansion(0)) +
  # Una flecha de texto y no un triángulo: Windows dibuja el triángulo como emoji.
  scale_y_discrete(labels = \(x) if_else(x %in% marcar, paste0("→ ", x), x)) +
  labs(x = "Avisos del barrio en cada grupo", y = NULL,
       caption = pie(paste0(
         fuente_pie, ". Barrios ordenados por la prima que les estima la regresión (paso 3), ",
         "de mayor a menor. La ubicación no entró al kmeans. ",
         sum(rowSums(table(d$barrio_nom, d$grupo) > 0) == K), " de los ",
         n_distinct(d$barrio_nom), " barrios tienen avisos de los ", K, " grupos, aunque en algunos la ",
         "franja es mínima (", franja_min$barrio_nom, ": ", coma_ar(franja_min$pct, 1), "% en el grupo ",
         franja_min$grupo, "). Las flechas marcan ", paste(marcar, collapse = " y "), ": prima alta y casi ",
         "la mitad de sus avisos en el grupo 1, el viejo (", paste0(coma_ar(pct_marcados, 0), "%", collapse = " y "),
         "). ",
         "Que el grupo ", K, " pese más arriba es esperable: el kmeans usa el precio por m² y la prima sale del precio ",
         "(Spearman ", coma_ar(cor_caro_prima, 2), ")."), ancho = 105)) +
  theme(axis.text.y = element_text(size = 7.6), panel.grid.major.y = element_blank())

guardar(g18, "g18_barrios_grupos.png", ancho = 8, alto = 8)


# =============================================================================
# 9. TABLAS COMO IMAGEN
# =============================================================================

# --- T16. Perfiles en unidades originales ------------------------------------
t16 <- perfil |>
  transmute(
    Grupo = nombre_grupo[grupo],
    Avisos = paste0(num_ar(avisos), " (", coma_ar(pct), "%)"),
    `Precio/m²` = usd_ar(precio_m2),
    `Superficie` = paste(num_ar(m2), "m²"),
    `Precio` = usd_ar(precio),
    `A estrenar` = paste0(coma_ar(pct_estrenar), "%"),
    `Más de 50 años` = paste0(coma_ar(pct_mas50), "%"),
    `En la mitad norte` = paste0(coma_ar(pct_norte), "%"),
    `Dist. al Obelisco` = paste(coma_ar(dist_km), "km")
  )
guardar_tabla(tabla_png(t16, anchos = c(1, 1.6, 1.2, 1.1, 1.3, 1.1, 1.3, 1.4, 1.4),
                        pie_tabla = paste0(
                          "Medianas y porcentajes de cada grupo, en unidades originales. ",
                          "Las dos últimas columnas no entraron al kmeans: se miran después. ",
                          "Grupos numerados de menor a mayor precio por m².")),
              "t16_perfiles_grupos.png", ancho = 12)

# --- T17. Estabilidad y correspondencia --------------------------------------
# Las filas de geografía dicen el hallazgo (los tipos están en todos lados), no
# por qué el ARI no puede ser alto: eso ya lo dice el pie. Van con tres
# decimales, como en el texto: con dos, 0,007 y 0,013 se leían igual. Y el
# resaltado va en la fila del barrio, que es donde pone el peso la diapositiva.
con_todos <- function(geo) sum(rowSums(table(geo, d$grupo) > 0) == K)
fila_geo <- comparacion$comparacion %in% c("Grupos contra barrio", "Grupos contra comuna",
                                            "Grupos contra mitad norte-sur")
t17 <- comparacion |>
  transmute(`Qué se compara` = comparacion,
            ARI = if_else(fila_geo, coma_ar(ari, 3), coma_ar(ari, 2)),
            Lectura = c("Un segundo algoritmo, robusto a extremos",
                        "Sensibilidad a la escala",
                        paste0("Sin los ", num_ar(sum(extremo)), " avisos a más de 3 desvíos"),
                        "Sensibilidad a la cantidad de grupos",
                        "Con 4 aparece una grilla de antigüedad por tamaño",
                        paste0(if (con_todos(d$barrio_nom) == n_distinct(d$barrio_nom)) "Los " else
                                 paste0(con_todos(d$barrio_nom), " de los "),
                               n_distinct(d$barrio_nom), " barrios tienen avisos de los tres grupos"),
                        paste0(if (con_todos(d$comuna_f) == nlevels(d$comuna_f)) "Las " else
                                 paste0(con_todos(d$comuna_f), " de las "),
                               nlevels(d$comuna_f), " comunas, también"),
                        "Los tres grupos están en las dos mitades"))
guardar_tabla(tabla_png(t17, anchos = c(2.6, 0.7, 3.2),
                        alinear = c("izq", "der", "izq"),
                        destacar = which(t17$`Qué se compara` == "Grupos contra barrio"),
                        pie_tabla = paste0(
                          "Índice de Rand ajustado: 1 = particiones idénticas, 0 = coincidencia al azar. ",
                          "Contra barrio y comuna no puede llegar a 1 aunque la correspondencia fuera perfecta, ",
                          "porque ", K, " grupos no reproducen ", n_distinct(d$barrio_nom), " barrios ni ",
                          n_distinct(d$comuna_f), " comunas. Silueta media de los ", K,
                          " grupos (medida sobre 5.000 avisos al azar): ", coma_ar(sil_media, 2),
                          "; entre 0,25 y 0,5 se considera estructura débil.")),
              "t17_estabilidad_grupos.png", ancho = 9)

# --- T18. Comuna por grupo (anexo) -------------------------------------------
t18 <- as.data.frame.matrix(round(100 * prop.table(table(d$comuna_f, d$grupo), 1), 1)) |>
  rownames_to_column("Comuna") |>
  mutate(across(-Comuna, ~ paste0(coma_ar(.x), "%")),
         Avisos = num_ar(as.vector(table(d$comuna_f))))
names(t18)[2:(K + 1)] <- nombre_grupo
guardar_tabla(tabla_png(t18, anchos = c(1, rep(1, K), 1),
                        pie_tabla = "Qué parte de los avisos de cada comuna cae en cada grupo. La comuna entra como categoría, nunca como número."),
              "t18_comuna_grupos.png", ancho = 7)

cat("\nGráficos y tablas del kmeans generados.\n")
