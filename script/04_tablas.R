# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 1: Análisis Exploratorio de Datos
# Script 04: tablas de la presentación
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: input/base_analisis.csv, raw/base_ventas_caba.csv
# Salida : output/tablas/04_*.csv
#
# Los conteos de nulos, los mínimos y los máximos NO se escriben a mano: se
# calculan sobre la base y se pegan a la descripción. Así la tabla de la
# presentación no puede quedar desactualizada respecto de los datos.
# =============================================================================

library(tidyverse)
source("script/00_tema.R")

base  <- read_csv("input/base_analisis.csv", show_col_types = FALSE)
crudo <- read_csv("raw/base_ventas_caba.csv", show_col_types = FALSE)

n_avisos <- nrow(base)


# =============================================================================
# 1. FICHA DE LA BASE
# =============================================================================

ficha <- tribble(
  ~campo, ~valor,
  "Nombre",              "Avisos de venta de departamentos en la Ciudad de Buenos Aires",
  "Fuente",              "Publicaciones de MercadoLibre Argentina, relevadas por el docente",
  "Qué representa",      "La oferta de departamentos en venta publicada en el portal, no el stock de viviendas ni las operaciones cerradas",
  "Unidad de observación", "El aviso publicado. Una misma vivienda puede aparecer en más de una fila",
  "Período",             "Corte transversal de noviembre de 2024. La base no tiene variable de fecha",
  "Observaciones",       paste0(num_ar(nrow(crudo)), " avisos en la base cruda; ",
                                num_ar(n_avisos), " después de quitar republicaciones"),
  "Variables",           paste0(ncol(crudo), " en la base original; 18 las que usamos en este análisis"),
  "Cobertura",           paste0(n_distinct(base$barrio), " barrios y ",
                                n_distinct(base$comuna), " comunas de CABA"),
  "Moneda y operación",  "Todos los avisos son de venta y todos los precios están publicados en dólares"
)

write_csv(ficha, "output/tablas/04_ficha_base.csv")
print(ficha, n = Inf, width = Inf)


# =============================================================================
# 2. DESCRIPCIÓN DE LAS VARIABLES
# =============================================================================

# Descripción de cada variable, escrita por nosotros. El tipo, la cantidad de
# nulos y los valores que toma se calculan abajo sobre la base.
descripciones <- tribble(
  ~variable, ~bloque, ~descripcion, ~tipo,

  # --- Precio y características de la vivienda ---
  "precio_usd", "vivienda",
  "Precio publicado del departamento, en dólares", "Numérica (US$)",

  "precio_m2_usd", "vivienda",
  "Precio publicado dividido por la superficie cubierta", "Numérica (US$/m²)",

  "metros_cubiertos", "vivienda",
  "Superficie cubierta declarada en el aviso", "Numérica (m²)",

  "ambientes", "vivienda",
  "Cantidad de ambientes declarada en el aviso", "Numérica (cantidad)",

  "dormitorios", "vivienda",
  "Dormitorios según la categoría de búsqueda del portal, no según el aviso", "Numérica (cantidad)",

  "banos", "vivienda",
  "Cantidad de baños declarada en el aviso", "Numérica (cantidad)",

  "tipo_depto", "vivienda",
  "Categoría del portal, de monoambiente a cuatro dormitorios o más", "Categórica",

  "antiguedad_grupo", "vivienda",
  "Antigüedad agrupada en cuatro tramos, sin solapamiento", "Categórica",

  "a_estrenar", "vivienda",
  "Vale 1 si el aviso declara la propiedad a estrenar", "Binaria",

  # --- Ubicación y publicación ---
  "barrio", "ubicacion",
  "Barrio oficial de CABA, asignado geocodificando la dirección del aviso", "Categórica",

  "comuna", "ubicacion",
  "Comuna de CABA a la que pertenece el barrio", "Categórica",

  "lat", "ubicacion",
  "Latitud de la dirección geocodificada. La usamos para el eje norte-sur", "Numérica (grados)",

  "long", "ubicacion",
  "Longitud de la dirección geocodificada", "Numérica (grados)",

  "dist_centro_m", "ubicacion",
  "Distancia en línea recta al Obelisco", "Numérica (m)",

  "geocod_score", "ubicacion",
  "Calidad con que ArcGIS ubicó la dirección. Un puntaje alto no garantiza el barrio correcto", "Numérica (0 a 100)",

  "publicado_por", "ubicacion",
  "Si el aviso lo publica una inmobiliaria o un particular", "Categórica",

  "inmobiliaria", "ubicacion",
  "Nombre de la inmobiliaria que publica el aviso", "Texto",

  "valor_m2_usd_barrio", "ubicacion",
  "Valor de referencia del m² del barrio, de fuente externa al portal", "Numérica (US$/m²)"
)

# Qué valores toma cada variable. Va como función y no como case_when porque
# case_when evalúa todas las ramas: pedirle el mínimo a una variable de texto
# rompe el cálculo aunque esa rama no sea la que corresponde.
resumir_valores <- function(nombre, x) {
  if (nombre == "inmobiliaria")
    return(paste0(num_ar(n_distinct(x, na.rm = TRUE)), " inmobiliarias distintas"))
  if (nombre == "a_estrenar")
    return(paste0("vale 1 en el ", coma_ar(100 * mean(x)), "% de los avisos"))
  if (nombre == "comuna")
    return(paste0(n_distinct(x), " comunas, de la 1 a la 15"))
  # El 4 de dormitorios no es "cuatro": es el techo de la categoría de búsqueda
  # del portal, que se llama "4 dormitorios o más". Lo chequeamos mirando los
  # ambientes de esos avisos: el 30,3% declara más de 5, o sea más de cuatro
  # dormitorios bajo la convención ambientes = dormitorios + 1.
  if (nombre == "dormitorios")
    return(paste0("de ", num_ar(min(x)), " a ", num_ar(max(x)),
                  ", donde ", num_ar(max(x)), " es cuatro o más"))
  if (nombre %in% c("lat", "long"))
    return("Coordenadas dentro de la jurisdicción de la Ciudad")
  if (nombre == "geocod_score")
    return(paste0("de ", coma_ar(min(x), 2), " a ", num_ar(max(x)),
                  ", nunca por debajo de 80"))
  if (nombre == "valor_m2_usd_barrio")
    return(paste0("de ", num_ar(min(x)), " a ", num_ar(max(x)),
                  ", un valor por barrio"))
  if (is.character(x) || is.factor(x))
    return(paste0(n_distinct(x), " categorías"))
  paste0("de ", num_ar(min(x)), " a ", num_ar(max(x)))
}

# Nulos y valores que toma cada variable, calculados sobre la base.
resumen_variables <- descripciones |>
  rowwise() |>
  mutate(
    n_nulos = sum(is.na(base[[variable]])),
    valores = resumir_valores(variable, base[[variable]])
  ) |>
  ungroup() |>
  mutate(
    # La celda lleva solo el número. Que esos 28.745 nulos sean exactamente los
    # avisos de particulares se explica en el párrafo de la diapositiva 4.
    # Para la instancia 2 conviene reconsiderarlo: la consigna evalúa la
    # descripción de los valores nulos en esta tabla, y ahí la explicación
    # estaba a la vista.
    nulos = num_ar(n_nulos)
  ) |>
  select(variable, descripcion, tipo, valores, nulos, bloque)

write_csv(resumen_variables |> filter(bloque == "vivienda") |> select(-bloque),
          "output/tablas/04_variables_vivienda.csv")
write_csv(resumen_variables |> filter(bloque == "ubicacion") |> select(-bloque),
          "output/tablas/04_variables_ubicacion.csv")

print(resumen_variables, n = Inf, width = Inf)

cat("\nVariables descriptas:", nrow(resumen_variables),
    "de", ncol(crudo), "que trae la base\n")
cat("Variables con nulos:",
    sum(resumen_variables$nulos != "0"), "\n")


# =============================================================================
# 3. DESCRIPTIVAS DE LAS VARIABLES NUMÉRICAS
# =============================================================================

vars_num <- c("precio_usd", "precio_m2_usd", "metros_cubiertos",
              "ambientes", "dormitorios", "banos", "dist_centro_m")

etiquetas_num <- c(
  precio_usd       = "Precio publicado (US$)",
  precio_m2_usd    = "Precio por m² (US$)",
  metros_cubiertos = "Superficie cubierta (m²)",
  ambientes        = "Ambientes",
  dormitorios      = "Dormitorios",
  banos            = "Baños",
  dist_centro_m    = "Distancia al Obelisco (m)"
)

descriptivas <- base |>
  select(all_of(vars_num)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  group_by(variable) |>
  summarise(
    media   = mean(valor),
    mediana = median(valor),
    desvio  = sd(valor),
    cv      = sd(valor) / mean(valor),
    minimo  = min(valor),
    p25     = quantile(valor, 0.25),
    p75     = quantile(valor, 0.75),
    maximo  = max(valor),
    .groups = "drop"
  ) |>
  mutate(variable = factor(variable, levels = vars_num)) |>
  arrange(variable) |>
  # Ambientes, dormitorios y baños se mueven entre 0 y 8: redondearlos a
  # enteros borra la diferencia entre la media y la mediana, que es justamente
  # lo que muestra la asimetría. A esas variables les dejamos un decimal.
  rowwise() |>
  mutate(
    Variable  = etiquetas_num[as.character(variable)],
    decimales = if (maximo < 100) 1 else 0,
    across(c(media, mediana, desvio, minimo, p25, p75, maximo),
           ~ coma_ar(.x, decimales)),
    cv = coma_ar(cv, 2)
  ) |>
  ungroup() |>
  select(Variable, Media = media, Mediana = mediana, `Desvío` = desvio,
         `CV` = cv, `Mínimo` = minimo, P25 = p25, P75 = p75, `Máximo` = maximo)

write_csv(descriptivas, "output/tablas/04_descriptivas.csv")
print(descriptivas, width = Inf)


# =============================================================================
# 4. EVIDENCIA DE QUE LA BASE LLEGA RECORTADA
# =============================================================================

# Los extremos de varias variables caen en valores exactos y redondos, y por
# debajo o por encima no hay nada. Eso no se explica por el mercado: la base
# fue podada antes de llegarnos. La consecuencia es que no podemos decir nada
# sobre lo que quedó afuera.

recorte <- tribble(
  ~variable, ~lectura,
  "metros_cubiertos", "Ningún aviso baja de 15 m² ni supera los 400 m²",
  "ambientes",        "El máximo es 8 y no hay nada por encima",
  "dormitorios",      "El 0 son monoambientes. El 4 no es cuatro: agrupa todo lo que tenga cuatro o más",
  "geocod_score",     "La calidad de la geocodificación nunca baja de 80 sobre 100",
  "precio_usd",       "El máximo cae en un valor redondo y hay un solo aviso ahí"
) |>
  rowwise() |>
  mutate(
    # geocod_score corta en 80,03: redondearlo a 80 haría parecer el corte más
    # prolijo de lo que es. Dejamos decimales cuando el valor los tiene.
    minimo = coma_ar(min(crudo[[variable]], na.rm = TRUE),
                     if (min(crudo[[variable]]) %% 1 == 0) 0 else 2),
    maximo = coma_ar(max(crudo[[variable]], na.rm = TRUE),
                     if (max(crudo[[variable]]) %% 1 == 0) 0 else 2),
    en_el_maximo = num_ar(sum(crudo[[variable]] ==
                                max(crudo[[variable]], na.rm = TRUE), na.rm = TRUE))
  ) |>
  ungroup() |>
  select(Variable = variable, `Mínimo` = minimo, `Máximo` = maximo,
         `Avisos en el máximo` = en_el_maximo, `Qué muestra` = lectura)

write_csv(recorte, "output/tablas/04_evidencia_recorte.csv")
print(recorte, width = Inf)

# Atípicos por la regla de 1,5 RIC, que sobreviven al recorte previo.
cat("\nAtípicos por la regla de 1,5 RIC (sobre la base de análisis):\n")
atipicos <- tibble(variable = c("precio_usd", "precio_m2_usd", "metros_cubiertos")) |>
  rowwise() |>
  mutate(
    q1 = quantile(base[[variable]], 0.25),
    q3 = quantile(base[[variable]], 0.75),
    limite_sup = q3 + 1.5 * (q3 - q1),
    limite_inf = q1 - 1.5 * (q3 - q1),
    altos = sum(base[[variable]] > limite_sup),
    bajos = sum(base[[variable]] < limite_inf),
    pct_altos = round(100 * altos / n_avisos, 1)
  ) |>
  ungroup() |>
  # El límite inferior va en la tabla a propósito. En precio y en superficie da
  # negativo, así que ningún aviso puede quedar por debajo y el "0 atípicos
  # bajos" no es un hallazgo. En precio por m² da positivo, y ahí sí lo es.
  select(variable, limite_inf, limite_sup, altos, pct_altos, bajos)
print(atipicos, width = Inf)
write_csv(atipicos, "output/tablas/04_atipicos.csv")


# =============================================================================
# 5. CUÁNTO DE LA VARIACIÓN DEL PRECIO ESTÁ ENTRE BARRIOS Y CUÁNTO ADENTRO
# =============================================================================

# Descomposición descriptiva de la varianza. NO es un modelo: separa la
# variación total en la parte que se explica por las diferencias entre las
# medias de cada barrio y la que queda dentro de los barrios.

descomponer <- function(y, grupo) {
  medias <- tibble(y = y, g = grupo) |>
    group_by(g) |> summarise(m = mean(y), n = n(), .groups = "drop")
  var_total <- var(y)
  var_entre <- sum(medias$n * (medias$m - mean(y))^2) / length(y)
  tibble(entre = 100 * var_entre / var_total,
         dentro = 100 * (var_total - var_entre) / var_total)
}

varianza <- bind_rows(
  descomponer(base$precio_m2_usd, base$barrio) |> mutate(variable = "Precio por m²"),
  descomponer(base$precio_usd,    base$barrio) |> mutate(variable = "Precio total")
) |>
  mutate(across(c(entre, dentro), ~ coma_ar(.x))) |>
  select(Variable = variable,
         `Entre barrios (%)` = entre,
         `Dentro de cada barrio (%)` = dentro)

write_csv(varianza, "output/tablas/04_varianza_barrios.csv")
print(varianza, width = Inf)

# Correlaciones de las dos variables de precio con superficie y ubicación.
cat("\nCorrelaciones (Spearman, que no se deja arrastrar por la cola):\n")
correlaciones <- tibble(
  contra = c("Superficie cubierta", "Latitud (eje norte-sur)",
             "Distancia al Obelisco"),
  precio_total = c(
    cor(base$precio_usd, base$metros_cubiertos, method = "spearman"),
    cor(base$precio_usd, base$lat, method = "spearman"),
    cor(base$precio_usd, base$dist_centro_m, method = "spearman")),
  precio_m2 = c(
    cor(base$precio_m2_usd, base$metros_cubiertos, method = "spearman"),
    cor(base$precio_m2_usd, base$lat, method = "spearman"),
    cor(base$precio_m2_usd, base$dist_centro_m, method = "spearman"))
) |>
  # Una correlación de -0,001 se imprime como "-0,00" y se lee raro. La
  # llevamos a cero, que es lo que dice.
  mutate(across(where(is.numeric),
                ~ coma_ar(if_else(abs(.x) < 0.005, 0, .x), 2))) |>
  select(`Correlación con` = contra,
         `Precio total` = precio_total,
         `Precio por m²` = precio_m2)

write_csv(correlaciones, "output/tablas/04_correlaciones.csv")
print(correlaciones, width = Inf)


# =============================================================================
# 9. LA FICHA Y LAS VARIABLES COMO IMAGEN (presentación final)
# =============================================================================
# En la instancia 1 estas dos tablas se armaban en la presentación a partir de
# los CSV. Para la final salen como imagen, con tabla_png() y guardar_tabla(),
# igual que las otras 18 (t01 a t18): misma paleta, misma tipografía y su
# nombre adentro. Siguen la numeración: t19 la ficha, t20 las variables.

t19 <- ficha |> rename(Campo = campo, Descripción = valor)
guardar_tabla(tabla_png(t19, anchos = c(1.5, 6), alinear = c("izq", "izq")),
              "t19_ficha_base.png", ancho = 10.5)

# Las dos tablas de variables van en una sola imagen, como en la diapositiva:
# una fila de encabezado por bloque, en terracota, y debajo sus variables.
bloque_t20 <- function(bloque_id, titulo) {
  bind_rows(
    tibble(Variable = titulo, Descripción = "", Tipo = "", `Valores que toma` = "", Nulos = ""),
    resumen_variables |>
      filter(bloque == bloque_id) |>
      transmute(Variable = variable, Descripción = descripcion, Tipo = tipo,
                `Valores que toma` = valores, Nulos = nulos)
  )
}
t20 <- bind_rows(bloque_t20("vivienda", "Describen el departamento"),
                 bloque_t20("ubicacion", "Ubican el aviso en la ciudad"))
filas_bloque <- which(t20$Descripción == "")
stopifnot(length(filas_bloque) == 2, nrow(t20) == nrow(resumen_variables) + 2)
guardar_tabla(tabla_png(t20, anchos = c(1.7, 5.2, 1.6, 3.4, 0.7),
                        alinear = c("izq", "izq", "izq", "izq", "der"),
                        destacar = filas_bloque, size = 3, size_enc = 3.2),
              "t20_variables.png", ancho = 14)

cat("\nListo. Tablas en output/tablas/\n")
