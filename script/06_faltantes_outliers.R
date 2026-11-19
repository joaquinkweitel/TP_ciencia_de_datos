# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 2: métodos estadísticos y cuantitativos
# Script 06: valores faltantes y valores atípicos (sección 2 de la consigna)
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: input/base_analisis.csv, raw/base_ventas_caba.csv
# Salida : output/graficos/*.png, output/tablas/06_*.csv
#
# Método: clase 13 (datos faltantes, outliers y transformaciones).
#   Faltantes: cuántos, dónde se concentran, qué mecanismo, qué estrategia.
#   Outliers : tres reglas (IQR, z clásico, z robusto con MAD) en dos escalas
#              (original y logaritmo), boxplots, y revisión caso por caso.
#
# La idea que ordena todo el script es la de la clase: las reglas marcan
# candidatos, no dictan sentencia. Y lo que encontramos: las seis combinaciones
# de regla y escala cortan la misma cola en lugares distintos.
# =============================================================================

library(tidyverse)
source("script/00_tema.R")

base  <- read_csv("input/base_analisis.csv", show_col_types = FALSE)
crudo <- read_csv("raw/base_ventas_caba.csv", show_col_types = FALSE)

n_base <- nrow(base)

sink("output/tablas/06_faltantes_outliers.txt", split = TRUE)
options(width = 130)
cat("Avisos en la base de análisis:", num_ar(n_base), "\n")


# =============================================================================
# 1. FALTANTES: CUÁNTOS Y EN QUÉ VARIABLES
# =============================================================================

cat("\n\n#### 1. FALTANTES POR VARIABLE ####\n\n")

# Las 18 variables que usamos. El resto de la base no entra en el análisis.
vars_usadas <- c(
  "precio_usd", "precio_m2_usd", "metros_cubiertos", "ambientes",
  "dormitorios", "banos", "tipo_depto", "antiguedad_grupo", "a_estrenar",
  "barrio", "comuna", "lat", "long", "dist_centro_m", "geocod_score",
  "publicado_por", "inmobiliaria", "valor_m2_usd_barrio"
)

faltantes <- base |>
  select(all_of(vars_usadas)) |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") |>
  mutate(pct_na = 100 * n_na / n_base) |>
  arrange(desc(n_na))

print(faltantes, n = Inf)

cat("\nVariables con al menos un NA:", sum(faltantes$n_na > 0), "de", length(vars_usadas), "\n")
cat("Filas sin ningún NA en las 18 variables:",
    num_ar(sum(complete.cases(select(base, all_of(vars_usadas))))), "\n")


# =============================================================================
# 2. LOS DOS FALTANTES SON ESTRUCTURALES
# =============================================================================
# La clase 13 clasifica los faltantes en MCAR, MAR y MNAR, que describen un
# mecanismo probabilístico. Los nuestros no son probabilísticos: son
# determinísticos. Un particular no tiene inmobiliaria porque no existe, no
# porque no la haya informado. Si hubiera que ubicarlos en la taxonomía serían
# el caso extremo de MAR: la probabilidad de faltar es exactamente 1
# condicional a una variable que observamos.

cat("\n\n#### 2. LOS FALTANTES SON ESTRUCTURALES ####\n\n")

# La clase 13 recomienda crear una variable indicadora del faltante. Acá la
# construimos, pero sobre todo para MOSTRAR que ya existe en la base: la
# indicadora es idéntica a una variable que ya teníamos.
base <- base |>
  mutate(
    publica_inmobiliaria = !is.na(inmobiliaria),
    antiguedad_abierta   = antiguedad_grupo == "d. Más de 50 años"
  )

cat("--- inmobiliaria: la indicadora es publicado_por ---\n")
print(table(publicado_por = base$publicado_por,
            tiene_inmobiliaria = base$publica_inmobiliaria))
cat("¿Son exactamente las mismas filas?:",
    identical(which(is.na(base$inmobiliaria)),
              which(base$publicado_por == "Particular")), "\n")

cat("\n--- antiguedad_max: la indicadora es antiguedad_grupo ---\n")
print(table(categoria_abierta = base$antiguedad_abierta,
            antiguedad_max_NA = is.na(base$antiguedad_max)))
cat("¿Son exactamente las mismas filas?:",
    identical(which(is.na(base$antiguedad_max)),
              which(base$antiguedad_abierta)), "\n")

# Los faltantes de las dos variables, sobre la base de análisis. Se calculan
# acá y se reutilizan en la tabla de la presentación: escribirlos a mano fue
# lo que nos hizo mezclar la base cruda con la de análisis.
na_inmo <- sum(is.na(base$inmobiliaria))
na_antig <- sum(is.na(base$antiguedad_max))
na_pubrec <- sum(crudo$publicacion_reciente == "sin dato")

cat("\nFaltantes sobre la base de análisis (", num_ar(n_base), " avisos):\n", sep = "")
cat("  inmobiliaria  :", num_ar(na_inmo),
    sprintf("(%s%%)", coma_ar(100 * na_inmo / n_base)), "\n")
cat("  antiguedad_max:", num_ar(na_antig),
    sprintf("(%s%%)", coma_ar(100 * na_antig / n_base)), "\n")


# =============================================================================
# 3. DÓNDE SE CONCENTRAN
# =============================================================================
# La consigna pide explícitamente dónde se concentran los faltantes. Como el
# faltante de inmobiliaria es "ser un aviso de particular", la pregunta real
# es: ¿los particulares se reparten parejo o se concentran en algún lado?
# Importa porque si se concentran en los barrios baratos, cualquier lectura de
# publicado_por queda confundida con la ubicación.

cat("\n\n#### 3. DÓNDE SE CONCENTRAN LOS FALTANTES ####\n\n")

part_barrio <- base |>
  group_by(barrio_nom) |>
  summarise(avisos = n(),
            pct_particular = 100 * mean(publicado_por == "Particular"),
            precio_m2 = median(precio_m2_usd),
            .groups = "drop") |>
  filter(avisos >= 100) |>
  arrange(desc(pct_particular))

print(part_barrio, n = Inf)

cat("\nCorrelación entre el % de particulares del barrio y su precio por m²:\n")
cat("  Pearson :", coma_ar(cor(part_barrio$pct_particular, part_barrio$precio_m2), 3), "\n")
cat("  Spearman:", coma_ar(cor(part_barrio$pct_particular, part_barrio$precio_m2,
                               method = "spearman"), 3), "\n")

cat("\n--- Por decil de precio ---\n")
part_decil <- base |>
  mutate(decil = ntile(precio_usd, 10)) |>
  group_by(decil) |>
  summarise(avisos = n(),
            precio_mediano = median(precio_usd),
            pct_particular = 100 * mean(publicado_por == "Particular"),
            .groups = "drop")
print(part_decil)


# =============================================================================
# 4. UN FALTANTE DISFRAZADO: publicacion_reciente
# =============================================================================
# La clase 13 insiste con los faltantes disfrazados: códigos como -9, 99 o
# "sin dato" que R no lee como NA. Revisando la base cruda encontramos uno.
# Esta variable NO estaba en nuestro análisis: la miramos ahora, encontramos
# que casi toda es un NA con otro nombre, y por eso queda afuera.

cat("\n\n#### 4. UN FALTANTE DISFRAZADO ####\n\n")

pub_rec <- crudo |>
  count(publicacion_reciente, sort = TRUE) |>
  mutate(pct = 100 * n / nrow(crudo))
print(pub_rec)

cat("\nNA formales que detecta R en esa variable:",
    sum(is.na(crudo$publicacion_reciente)), "\n")
cat("Faltantes reales, contando la categoría 'sin dato':",
    num_ar(sum(crudo$publicacion_reciente == "sin dato")),
    sprintf("(%s%%)", coma_ar(100 * mean(crudo$publicacion_reciente == "sin dato"))), "\n")

# Chequeo de otros faltantes disfrazados habituales en el resto de la base.
cat("\n--- Búsqueda de otros códigos sospechosos ---\n")
sospechosos <- c(-9, -99, 99, 999, 9999)
num_crudo <- crudo |> select(where(is.numeric))
for (v in names(num_crudo)) {
  hits <- sum(num_crudo[[v]] %in% sospechosos, na.rm = TRUE)
  if (hits > 0) cat(sprintf("  %-26s %5d valores en %s\n", v, hits,
                            paste(intersect(unique(num_crudo[[v]]), sospechosos),
                                  collapse = ", ")))
}
cat("Ceros en variables donde un cero sería imposible:\n")
for (v in c("precio_usd", "metros_cubiertos", "ambientes", "banos")) {
  cat(sprintf("  %-20s %d\n", v, sum(crudo[[v]] == 0, na.rm = TRUE)))
}


# =============================================================================
# 5. OUTLIERS: TRES REGLAS EN DOS ESCALAS
# =============================================================================
# Clase 13, lámina 23. Las tres reglas sobre las mismas variables, en escala
# original y en logaritmo. Lo que importa no es cuál tiene razón sino que NO
# COINCIDEN: son un punto de partida, no un veredicto.

cat("\n\n#### 5. OUTLIERS: TRES REGLAS, DOS ESCALAS ####\n\n")

vars_outlier <- c(precio_usd       = "Precio publicado (US$)",
                  precio_m2_usd    = "Precio por m² (US$)",
                  metros_cubiertos = "Superficie cubierta (m²)",
                  dist_centro_m    = "Distancia al Obelisco (m)")

# Las tres reglas, devolviendo un vector lógico.
regla_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75)); r <- q[2] - q[1]
  x < q[1] - 1.5 * r | x > q[2] + 1.5 * r
}
regla_z <- function(x) abs((x - mean(x)) / sd(x)) > 3
regla_z_rob <- function(x) {
  m <- mad(x)
  if (m == 0) return(rep(FALSE, length(x)))
  abs((x - median(x)) / m) > 3
}

conteo_reglas <- map_dfr(names(vars_outlier), function(v) {
  x  <- base[[v]]
  lx <- log(x)
  tibble(
    variable      = vars_outlier[[v]],
    iqr_orig      = sum(regla_iqr(x)),
    iqr_log       = sum(regla_iqr(lx)),
    z_orig        = sum(regla_z(x)),
    z_log         = sum(regla_z(lx)),
    z_rob_orig    = sum(regla_z_rob(x)),
    z_rob_log     = sum(regla_z_rob(lx))
  )
})

print(conteo_reglas, width = Inf)

cat("\nEn porcentaje de los", num_ar(n_base), "avisos:\n")
print(conteo_reglas |>
        mutate(across(-variable, ~ round(100 * .x / n_base, 2))), width = Inf)

# ¿Cuánto se superponen las reglas? Sobre el precio, que es la variable central.
# Las seis combinaciones de regla y escala. Una versión anterior usaba solo
# cuatro, elegidas sin criterio, y decía "de 3.292, solo 482 los marcan las
# cuatro". El número era correcto pero engañoso: los 482 eran exactamente los
# de la regla más estricta, contenidos en las otras tres.
cat("\n--- ¿Marcan a los mismos avisos? (precio_usd) ---\n")
marcas <- tibble(
  iqr_orig   = regla_iqr(base$precio_usd),
  z_orig     = regla_z(base$precio_usd),
  z_rob_orig = regla_z_rob(base$precio_usd),
  iqr_log    = regla_iqr(log(base$precio_usd)),
  z_log      = regla_z(log(base$precio_usd)),
  z_rob_log  = regla_z_rob(log(base$precio_usd))
)
cat("Marcados por al menos una combinación:", num_ar(sum(rowSums(marcas) > 0)), "\n")
cat("Marcados por las seis               :", num_ar(sum(rowSums(marcas) == 6)), "\n")
# Lo que muestra la cuenta: los conjuntos están anidados. Los de una regla más
# estricta quedan casi siempre dentro de los de una más laxa. Las reglas no
# señalan avisos distintos: cortan la misma cola en lugares distintos.
cat("La combinación más laxa sola cubre:", num_ar(max(colSums(marcas))), "de esos",
    num_ar(sum(rowSums(marcas) > 0)), "\n")
anidado <- function(a, b) all(marcas[[b]][marcas[[a]]])   # ¿todo a está en b?
cat("¿Cada combinación está contenida en la de más marcados?:\n")
mas_laxa <- names(which.max(colSums(marcas)))
print(sapply(names(marcas), \(v) anidado(v, mas_laxa)))
print(marcas |> summarise(across(everything(), sum)))
cat("\nCoincidencia entre pares de reglas (proporción de acuerdo):\n")
print(round(cor(marcas * 1), 2))


# =============================================================================
# 6. LOS LÍMITES: CUÁNDO LA AUSENCIA DE ATÍPICOS INFORMA Y CUÁNDO NO
# =============================================================================

cat("\n\n#### 6. LÍMITES DE LA REGLA DE 1,5 RIC ####\n\n")

limites <- map_dfr(names(vars_outlier), function(v) {
  x <- base[[v]]
  q <- quantile(x, c(0.25, 0.75)); r <- q[2] - q[1]
  tibble(variable = vars_outlier[[v]],
         q1 = q[[1]], q3 = q[[2]], ric = r,
         lim_inf = q[[1]] - 1.5 * r,
         lim_sup = q[[2]] + 1.5 * r,
         minimo = min(x),
         alcanzable = q[[1]] - 1.5 * r > 0,
         bajos = sum(x < q[[1]] - 1.5 * r),
         altos = sum(x > q[[2]] + 1.5 * r))
})
print(limites, width = Inf)

cat("\nLectura: cuando el límite inferior es negativo, que no haya atípicos\n")
cat("bajos no informa nada, porque ningún valor puede caer ahí. Solo informa\n")
cat("cuando el límite es positivo y alcanzable.\n")


# =============================================================================
# 7. ¿ERRORES O VALORES GENUINOS?
# =============================================================================
# La consigna pide decir cuántos y cuáles son, y si son errores o genuinos.
# Acá la respuesta es las dos cosas, y hay un hallazgo concreto: parte de los
# avisos más extremos NO son departamentos.

cat("\n\n#### 7. ¿ERRORES O GENUINOS? ####\n\n")

# 7.1 Avisos que no son departamentos, detectados por el título.
#
# Primer intento, descartado: buscar las palabras en cualquier parte del
# título. Marcaba 3.950 avisos, el 10%, y estaba mal: 3.835 eran por "cochera",
# que en casi todos los casos es un departamento CON cochera, no una cochera en
# venta. Lo mismo con "galpón" o "terreno" dentro de la descripción de un patio.
#
# Criterio que usamos: el título EMPIEZA con el tipo de unidad, eventualmente
# después de "venta" o "venta de". Ahí la palabra nombra lo que se vende, no
# una de sus comodidades. Es conservador: deja afuera avisos mixtos del tipo
# "departamento/oficina", que son departamentos vendidos como apto profesional.

patron_no_depto <- regex(
  "^\\s*(venta\\s+(de\\s+)?)?(oficina|local|dep[oó]sito|galp[oó]n|cochera|terreno|lote|fondo de comercio)\\b",
  ignore_case = TRUE)

base <- base |> mutate(no_es_depto = str_detect(titulo_aviso, patron_no_depto))

cat("Avisos cuyo título los identifica como otra cosa que un departamento:",
    num_ar(sum(base$no_es_depto)),
    sprintf("(%s%% de la base)", coma_ar(100 * mean(base$no_es_depto), 2)), "\n\n")

cat("--- Qué tipo de unidad son ---\n")
print(base |>
        filter(no_es_depto) |>
        mutate(tipo = str_to_lower(str_extract(
          titulo_aviso,
          regex("(oficina|local|dep[oó]sito|galp[oó]n|cochera|terreno|lote)",
                ignore_case = TRUE)))) |>
        count(tipo, sort = TRUE))

cat("\n--- Los 40 avisos, uno por uno ---\n")
print(base |> filter(no_es_depto) |>
        arrange(desc(metros_cubiertos)) |>
        select(id, metros_cubiertos, ambientes, precio_usd, precio_m2_usd,
               barrio_nom, titulo_aviso),
      width = Inf, n = Inf)

cat("\n--- ¿Son extremos? Comparación con el resto ---\n")
print(base |> group_by(no_es_depto) |>
        summarise(avisos = n(),
                  m2_mediano = median(metros_cubiertos),
                  precio_mediano = median(precio_usd),
                  precio_m2_mediano = median(precio_m2_usd),
                  .groups = "drop"))

# 7.2 Los extremos que SÍ son genuinos.
cat("\n--- Los avisos más caros: ¿errores o Puerto Madero? ---\n")
print(base |> slice_max(precio_usd, n = 10) |>
        select(id, precio_usd, metros_cubiertos, precio_m2_usd, barrio_nom, titulo_aviso),
      width = Inf)

cat("\n--- Los de mayor precio por m² ---\n")
print(base |> slice_max(precio_m2_usd, n = 10) |>
        select(id, precio_m2_usd, precio_usd, metros_cubiertos, barrio_nom),
      width = Inf)

cat("\nAvisos de Puerto Madero entre los 100 de mayor precio por m²:",
    sum(slice_max(base, precio_m2_usd, n = 100)$barrio_nom == "Puerto Madero"), "\n")


# =============================================================================
# 8. EL IMPACTO DE CADA ESTRATEGIA (clase 13, lámina 53)
# =============================================================================
# Toda decisión de limpieza cambia los datos. Hay que medir cuánto.

cat("\n\n#### 8. IMPACTO DE CADA ESTRATEGIA ####\n\n")

winsorizar <- function(x, p_inf = 0.01, p_sup = 0.99) {
  l <- quantile(x, c(p_inf, p_sup))
  pmin(pmax(x, l[1]), l[2])
}

q <- quantile(base$precio_usd, c(0.25, 0.75))
lim_sup_precio <- q[[2]] + 1.5 * (q[[2]] - q[[1]])

resumen_version <- function(datos, etiqueta) {
  datos |>
    summarise(
      version        = etiqueta,
      n              = n(),
      media          = mean(precio_usd),
      mediana        = median(precio_usd),
      desvio         = sd(precio_usd),
      p5             = quantile(precio_usd, 0.05),
      p95            = quantile(precio_usd, 0.95),
      precio_m2_med  = median(precio_m2_usd)
    )
}

impacto <- bind_rows(
  resumen_version(base, "Base de análisis"),
  resumen_version(filter(base, !no_es_depto), "Sin los que no son departamentos"),
  resumen_version(mutate(base, precio_usd = winsorizar(precio_usd)), "Winsorizado p1-p99"),
  resumen_version(filter(base, precio_usd <= lim_sup_precio), "Sin atípicos por 1,5 RIC")
) |>
  mutate(across(where(is.numeric), ~ round(.x)))

print(impacto, width = Inf)

cat("\nLa señal de alerta de la clase 13: si la media se mueve mucho y la\n")
cat("mediana casi no, los extremos estaban tirando de la media. Hay que\n")
cat("confirmar que son errores antes de sacarlos.\n")

# Los números que van a los textos de la presentación (script 10).
precio_reglas <- filter(conteo_reglas, variable == vars_outlier[["precio_usd"]])
dist_reglas   <- filter(conteo_reglas, variable == vars_outlier[["dist_centro_m"]])

# La distancia al Obelisco va al revés que el precio: el logaritmo no reduce
# sus atípicos, los crea. Es casi simétrica en escala original y el logaritmo
# le estira la cola izquierda: los marcados son todos avisos muy cerca del
# Obelisco. Y en el precio, el logaritmo hace aparecer unos pocos atípicos por
# abajo, que en escala original no pueden existir porque el límite es negativo.
lim_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75)); r <- q[[2]] - q[[1]]
  c(inf = q[[1]] - 1.5 * r, sup = q[[2]] + 1.5 * r)
}
asim <- function(x) mean(((x - mean(x)) / sd(x))^3)
l_dist   <- lim_iqr(log(base$dist_centro_m))
l_precio <- lim_iqr(log(base$precio_usd))
stopifnot(sum(log(base$dist_centro_m) > l_dist[["sup"]]) == 0)   # todos por abajo
cat("\nDistancia: atípicos en log", sum(log(base$dist_centro_m) < l_dist[["inf"]]),
    "todos a menos de", round(exp(l_dist[["inf"]])), "m. Asimetría:",
    round(asim(base$dist_centro_m), 2), "original,", round(asim(log(base$dist_centro_m)), 2), "log\n")
cat("Precio en log: atípicos por abajo", sum(log(base$precio_usd) < l_precio[["inf"]]),
    "(menos de US$", round(exp(l_precio[["inf"]])), ")\n")
lim_m2 <- filter(limites, variable == vars_outlier[["precio_m2_usd"]])
tipos_no_depto <- base |>
  filter(no_es_depto) |>
  mutate(tipo = str_to_lower(str_extract(titulo_aviso, regex(
    "(oficina|local|dep[oó]sito|galp[oó]n|cochera|terreno|lote)", ignore_case = TRUE))),
    # Los títulos a veces vienen sin tilde.
    tipo = recode(tipo, deposito = "depósito", galpon = "galpón")) |>
  count(tipo, sort = TRUE)
plural <- c(oficina = "oficinas", local = "locales", terreno = "terrenos",
            cochera = "cocheras", "depósito" = "depósito", deposito = "depósito",
            "galpón" = "galpones", galpon = "galpones", lote = "lotes")
guardar_numeros(
  "06_numeros.csv",
  n_base = n_base, n_crudo = nrow(crudo),
  na_inmo = na_inmo, pct_na_inmo = 100 * na_inmo / n_base,
  na_antig = na_antig, pct_na_antig = 100 * na_antig / n_base,
  pct_pubrec = 100 * mean(crudo$publicacion_reciente == "sin dato"),
  filas_completas = sum(complete.cases(select(base, all_of(vars_usadas)))),
  part_max_barrio = part_barrio$barrio_nom[1], part_max = part_barrio$pct_particular[1],
  part_min_barrio = last(part_barrio$barrio_nom), part_min = last(part_barrio$pct_particular),
  part_spearman = cor(part_barrio$pct_particular, part_barrio$precio_m2, method = "spearman"),
  part_pearson = cor(part_barrio$pct_particular, part_barrio$precio_m2),
  part_n_barrios = nrow(part_barrio),
  iqr_orig = precio_reglas$iqr_orig, iqr_log = precio_reglas$iqr_log,
  z_orig = precio_reglas$z_orig, z_log = precio_reglas$z_log,
  zrob_orig = precio_reglas$z_rob_orig, zrob_log = precio_reglas$z_rob_log,
  marcados_alguna = sum(rowSums(marcas) > 0), marcados_todas = sum(rowSums(marcas) == 6),
  marcados_max = max(colSums(marcas)),
  dist_iqr_orig = dist_reglas$iqr_orig, dist_iqr_log = dist_reglas$iqr_log,
  dist_log_lim_m = exp(l_dist[["inf"]]),
  dist_asim_orig = asim(base$dist_centro_m), dist_asim_log = asim(log(base$dist_centro_m)),
  precio_log_bajos = sum(log(base$precio_usd) < l_precio[["inf"]]),
  precio_log_lim = exp(l_precio[["inf"]]),
  lim_inf_m2 = lim_m2$lim_inf, min_m2 = lim_m2$minimo,
  no_depto = sum(base$no_es_depto),
  no_depto_tipos = paste(paste(tipos_no_depto$n,
                               ifelse(tipos_no_depto$n == 1, tipos_no_depto$tipo,
                                      plural[tipos_no_depto$tipo])), collapse = ", "),
  pm_top100 = sum(slice_max(base, precio_m2_usd, n = 100)$barrio_nom == "Puerto Madero"),
  media_base = impacto$media[1], media_sin_ric = impacto$media[4],
  p95_base = impacto$p95[1], p95_sin_ric = impacto$p95[4],
  sacados_ric = n_base - impacto$n[4],
  desvio_base = impacto$desvio[1], desvio_sin_ric = impacto$desvio[4]
)

# Guardamos la base con las dos marcas nuevas, para los scripts que siguen.
write_csv(base, "input/base_analisis.csv")
cat("\nBase actualizada con las indicadoras: publica_inmobiliaria,",
    "antiguedad_abierta, no_es_depto\n")

sink()
cat("\nDiagnóstico guardado en output/tablas/06_faltantes_outliers.txt\n")


# =============================================================================
# 9. GRÁFICOS
# =============================================================================
# Sin título ni bajada: los pone la diapositiva, como en la instancia 1.

# --- G09. El mismo precio, dos escalas (clase 13, lámina 19) -----------------
# Es el gráfico que muestra por qué la escala importa. En escala original el
# boxplot marca como atípicos a 3.287 avisos; en logaritmo, a 723.

# Van como dos archivos separados y no como dos facetas de un mismo gráfico:
# ggplot no admite una transformación de escala distinta en cada faceta, y la
# gracia de esta lámina es justamente que el eje de abajo sea logarítmico.
# En la diapositiva se apilan uno sobre el otro.

n_orig <- sum(regla_iqr(base$precio_usd))
n_log  <- sum(regla_iqr(log(base$precio_usd)))

g09a <- ggplot(base, aes(precio_usd, y = 1)) +
  geom_boxplot(fill = colores$apagado, color = colores$secundario,
               outlier.color = colores$acento, outlier.alpha = 0.2,
               outlier.size = 0.8, width = 0.5) +
  annotate("text", x = Inf, y = 1.45, hjust = 1.03, size = 3.2,
           family = fuente_cuerpo, color = colores$acento,
           label = paste0("Escala original: ", num_ar(n_orig), " avisos fuera de los bigotes")) +
  # La última etiqueta del eje, US$ 3.000.000, cae sobre el borde y se cortaba:
  # más margen a la derecha del panel y del gráfico.
  scale_x_continuous(labels = usd_ar, expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(limits = c(0.55, 1.55)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        plot.margin = margin(14, 40, 12, 14))

g09b <- ggplot(base, aes(precio_usd, y = 1)) +
  geom_boxplot(fill = colores$apagado, color = colores$secundario,
               outlier.color = colores$acento, outlier.alpha = 0.2,
               outlier.size = 0.8, width = 0.5) +
  annotate("text", x = Inf, y = 1.45, hjust = 1.03, size = 3.2,
           family = fuente_cuerpo, color = colores$acento,
           label = paste0("Escala logarítmica: ", num_ar(n_log), " avisos fuera de los bigotes")) +
  scale_x_log10(labels = usd_ar,
                breaks = c(25e3, 50e3, 1e5, 2e5, 5e5, 1e6, 3e6)) +
  scale_y_continuous(limits = c(0.55, 1.55)) +
  labs(x = "Precio publicado (US$)", y = NULL,
       caption = pie(paste0(fuente_pie,
         ". La regla es la misma, 1,5 veces el rango intercuartílico; lo único que cambia es la escala."))) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

guardar(g09a, "g09a_boxplot_precio_original.png", ancho = 10, alto = 2.3)
guardar(g09b, "g09b_boxplot_precio_log.png", ancho = 10, alto = 2.6)


# --- G10. Las tres reglas no coinciden (clase 13, lámina 24) -----------------
# Una fila por regla, un punto por aviso, el eje en log para que se vean todos.

set.seed(2026)
muestra <- base |> slice_sample(n = 6000)

reglas_largo <- bind_rows(
  tibble(regla = "IQR, escala original", precio = muestra$precio_usd,
         marcado = regla_iqr(muestra$precio_usd)),
  tibble(regla = "z clásico, escala original", precio = muestra$precio_usd,
         marcado = regla_z(muestra$precio_usd)),
  tibble(regla = "z robusto (MAD), escala original", precio = muestra$precio_usd,
         marcado = regla_z_rob(muestra$precio_usd)),
  tibble(regla = "IQR, logaritmo", precio = muestra$precio_usd,
         marcado = regla_iqr(log(muestra$precio_usd))),
  tibble(regla = "z clásico, logaritmo", precio = muestra$precio_usd,
         marcado = regla_z(log(muestra$precio_usd))),
  tibble(regla = "z robusto (MAD), logaritmo", precio = muestra$precio_usd,
         marcado = regla_z_rob(log(muestra$precio_usd)))
) |>
  mutate(regla = factor(regla, levels = rev(c(
    "IQR, escala original", "z clásico, escala original",
    "z robusto (MAD), escala original", "IQR, logaritmo",
    "z clásico, logaritmo", "z robusto (MAD), logaritmo"))),
    escala = if_else(str_detect(regla, "logaritmo"), "Logaritmo", "Escala original"))

g10 <- ggplot(reglas_largo, aes(precio, regla, color = marcado)) +
  geom_jitter(height = 0.22, size = 0.5, alpha = 0.45) +
  scale_color_manual(values = c(`TRUE` = colores$acento, `FALSE` = colores$apagado),
                     labels = c(`TRUE` = "Marcado como atípico", `FALSE` = "No marcado")) +
  scale_x_log10(labels = usd_ar, breaks = c(25e3, 50e3, 1e5, 2e5, 5e5, 1e6, 3e6)) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  labs(x = "Precio publicado (US$), eje en escala logarítmica", y = NULL,
       caption = pie(paste0(fuente_pie, ". Muestra aleatoria de ",
                            num_ar(nrow(muestra)),
                            " avisos para que se vean los puntos; los conteos del texto usan los ",
                            num_ar(n_base), " avisos."))) +
  theme(panel.grid.major.y = element_blank())

guardar(g10, "g10_tres_reglas.png", ancho = 10, alto = 4.6)


# --- G11. Dónde se concentran los faltantes ---------------------------------
# El faltante de inmobiliaria es "ser un aviso de particular". Si los
# particulares se concentraran en los barrios baratos, cualquier lectura de
# publicado_por estaría confundida con la ubicación.

g11 <- ggplot(part_barrio, aes(precio_m2, pct_particular)) +
  geom_point(color = colores$apagado, size = 2.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = colores$acento, linewidth = 0.9) +
  geom_text(data = filter(part_barrio, barrio_nom %in%
                            c("Puerto Madero", "Mataderos", "Belgrano", "Palermo", "Boca")),
            aes(label = barrio_nom), hjust = -0.12, size = 3,
            family = fuente_cuerpo, color = colores$texto) +
  scale_x_continuous(labels = num_ar, expand = expansion(mult = c(0.05, 0.16))) +
  scale_y_continuous(labels = function(x) paste0(num_ar(x), "%")) +
  labs(x = "Precio por m² mediano del barrio (US$)",
       y = "Avisos publicados por particulares",
       # La recta es un ajuste lineal, así que el número es Pearson: los dos miden
       # lo mismo. Spearman, por rangos, da parecido (en el txt).
       caption = pie(paste0(fuente_pie,
         ". Un punto por barrio, solo los ", nrow(part_barrio), " con al menos 100 avisos. ",
         "La recta es un ajuste lineal; su correlación (Pearson): ",
         coma_ar(cor(part_barrio$pct_particular, part_barrio$precio_m2), 2), ".")))

guardar(g11, "g11_particulares_por_barrio.png", ancho = 10, alto = 4.7)


# =============================================================================
# 10. TABLAS COMO IMAGEN
# =============================================================================

# --- T01. Faltantes: las dos variables y su indicadora ----------------------
# Los números salen de los objetos calculados arriba, no escritos a mano.
t01 <- tibble(
  Variable = c("inmobiliaria", "antiguedad_max", "publicacion_reciente"),
  `Faltantes` = num_ar(c(na_inmo, na_antig, na_pubrec)),
  `% de su base` = paste0(coma_ar(c(100 * na_inmo / n_base,
                                    100 * na_antig / n_base,
                                    100 * na_pubrec / nrow(crudo))), "%"),
  `Por qué falta` = c("El aviso lo publica un particular",
                      "La categoría «Más de 50 años» es abierta",
                      "La categoría «sin dato» es un NA con otro nombre"),
  `Indicadora equivalente` = c("publicado_por", "antiguedad_grupo", "no aplica"),
  `Decisión` = c("Mantener", "Mantener", "Descartar la variable")
)
guardar_tabla(tabla_png(t01, anchos = c(1.6, 0.9, 1, 3.2, 1.6, 1.4),
                        alinear = c("izq", "der", "der", "izq", "izq", "izq"),
                        pie_tabla = paste0(
                          "Las dos primeras se miden sobre la base de análisis (", num_ar(n_base),
                          " avisos). publicacion_reciente nunca entró al análisis, así que se mide sobre la base cruda (",
                          num_ar(nrow(crudo)), " avisos).")),
              "t01_faltantes.png", ancho = 11)

# --- T02. Las tres reglas en dos escalas ------------------------------------
t02 <- conteo_reglas |>
  transmute(
    Variable = variable,
    `IQR` = num_ar(iqr_orig),
    `z clásico` = num_ar(z_orig),
    `z robusto` = num_ar(z_rob_orig),
    `IQR (log)` = num_ar(iqr_log),
    `z clásico (log)` = num_ar(z_log),
    `z robusto (log)` = num_ar(z_rob_log)
  )
# El gráfico de las reglas (g10) va al anexo, así que el punto de la clase 13,
# que las reglas no marcan a los mismos avisos, tiene que quedar en un número
# en esta tabla, que es la que va al cuerpo.
guardar_tabla(tabla_png(t02, anchos = c(2.4, 1, 1.1, 1.1, 1.1, 1.3, 1.3),
                        pie_tabla = paste0(
                          "Avisos marcados como atípicos sobre ", num_ar(n_base),
                          ". Las tres primeras columnas aplican la regla sobre la variable; ",
                          "las tres últimas, sobre su logaritmo. Sobre el precio, las seis combinaciones marcan entre ",
                          num_ar(min(colSums(marcas))), " y ", num_ar(max(colSums(marcas))),
                          " avisos; la más laxa sola cubre ", num_ar(max(colSums(marcas))), " de los ",
                          num_ar(sum(rowSums(marcas) > 0)),
                          " que marca alguna. No señalan avisos distintos: cortan la misma cola en lugares distintos.")),
              "t02_tres_reglas.png", ancho = 10.5)

# --- T03. Impacto de cada estrategia ----------------------------------------
t03 <- impacto |>
  transmute(
    `Versión de la base` = version,
    Avisos = num_ar(n),
    `Media` = usd_ar(media),
    `Mediana` = usd_ar(mediana),
    `Desvío` = usd_ar(desvio),
    `P5` = usd_ar(p5),
    `P95` = usd_ar(p95)
  )
guardar_tabla(tabla_png(t03, anchos = c(2.6, 1, 1.2, 1.2, 1.2, 1.1, 1.2),
                        pie_tabla = paste0(
                          "Precio publicado. Sacar todos los atípicos por 1,5 RIC elimina ",
                          num_ar(n_base - impacto$n[4]),
                          " avisos y baja el desvío a menos de la mitad: esos extremos son Puerto Madero, no errores.")),
              "t03_impacto_estrategias.png", ancho = 10.5)

# --- T04. Los avisos que no son departamentos -------------------------------
t04 <- base |>
  filter(no_es_depto) |>
  mutate(tipo = str_to_sentence(str_extract(
    titulo_aviso, regex("(oficina|local|dep[oó]sito|galp[oó]n|cochera|terreno|lote)",
                        ignore_case = TRUE))),
    # Los títulos a veces vienen sin tilde.
    tipo = recode(tipo, Deposito = "Depósito", Galpon = "Galpón")) |>
  count(tipo, sort = TRUE) |>
  mutate(`% de la base` = paste0(coma_ar(100 * n / n_base, 2), "%")) |>
  transmute(`Tipo de unidad` = tipo, Avisos = num_ar(n), `% de la base`)
t04 <- bind_rows(t04, tibble(`Tipo de unidad` = "Total",
                             Avisos = num_ar(sum(base$no_es_depto)),
                             `% de la base` = paste0(coma_ar(100 * mean(base$no_es_depto), 2), "%")))
guardar_tabla(tabla_png(t04, anchos = c(2, 1, 1.2), destacar = nrow(t04),
                        pie_tabla = "Avisos cuyo título empieza nombrando una unidad que no es un departamento."),
              "t04_no_departamentos.png", ancho = 6)

cat("\nGráficos y tablas de la sección 2 generados.\n")
