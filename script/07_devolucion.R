# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 2: métodos estadísticos y cuantitativos
# Script 07: las cuatro correcciones que pidió la devolución
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# La devolución de la instancia 1 calificó el análisis exploratorio como
# "Regular" y pidió cuatro cosas. Este script produce las tres que son de
# cálculo (la cuarta, los outliers, está en el script 06):
#
#   1. Estadísticas descriptivas de TODAS las variables relevantes.
#      En la instancia 1 calculamos 21 y a la presentación llegaron 7.
#   2. Cantidad de observaciones por barrio, con su porcentaje.
#   3. Justificar por qué no usamos las variables de cercanía, en lugar de
#      descartarlas sin argumento. Matriz de correlaciones y PCA.
#
# Entrada: input/base_analisis.csv
# Salida : output/tablas/07_*.csv, output/graficos/*.png, output/tablas/t05 a t09
# =============================================================================

library(tidyverse)
library(FactoMineR)
source("script/00_tema.R")

base <- read_csv("input/base_analisis.csv", show_col_types = FALSE)
n_base <- nrow(base)

sink("output/tablas/07_devolucion.txt", split = TRUE)
options(width = 130)


# =============================================================================
# 1. DESCRIPTIVAS DE TODAS LAS VARIABLES RELEVANTES
# =============================================================================
# La devolución pide ver "la forma" de las variables, no solo su centro. Por
# eso además de media, mediana y desvío va el coeficiente de variación, los
# cuartiles y una medida de asimetría.

cat("\n#### 1. DESCRIPTIVAS DE LAS VARIABLES NUMÉRICAS ####\n\n")

asimetria <- function(x) {
  n <- length(x)
  m <- mean(x); s <- sd(x)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

etiquetas <- c(
  precio_usd                = "Precio publicado (US$)",
  precio_m2_usd             = "Precio por m² (US$)",
  metros_cubiertos          = "Superficie cubierta (m²)",
  ambientes                 = "Ambientes",
  dormitorios               = "Dormitorios",
  banos                     = "Baños",
  antiguedad_media          = "Antigüedad, punto medio (años)",
  dist_centro_m             = "Distancia al Obelisco (m)",
  dist_subte_m              = "Distancia al subte (m)",
  dist_comisaria_m          = "Distancia a comisaría (m)",
  dist_escuela_m            = "Distancia a escuela (m)",
  escuelas_1000m            = "Escuelas a menos de 1 km",
  dist_espacio_verde_m      = "Distancia a espacio verde (m)",
  espacios_verdes_1000m     = "Espacios verdes a menos de 1 km",
  superficie_verde_1000m_m2 = "Superficie verde a menos de 1 km (m²)",
  dist_parada_colectivo_m   = "Distancia a parada de colectivo (m)",
  paradas_colectivo_1000m   = "Paradas de colectivo a menos de 1 km",
  lineas_colectivo_1000m    = "Líneas de colectivo a menos de 1 km",
  geocod_score              = "Calidad de la geocodificación (0 a 100)",
  valor_m2_usd_barrio       = "Valor de referencia del m² del barrio (US$)"
)

descriptivas <- map_dfr(names(etiquetas), function(v) {
  x <- base[[v]]
  tibble(variable = v, etiqueta = etiquetas[[v]],
         n = length(x), media = mean(x), mediana = median(x),
         desvio = sd(x), cv = sd(x) / mean(x),
         minimo = min(x), p25 = quantile(x, .25), p75 = quantile(x, .75),
         maximo = max(x), asim = asimetria(x))
})

print(descriptivas |> mutate(across(where(is.numeric), ~ round(.x, 2))),
      n = Inf, width = Inf)

write_csv(descriptivas, "output/tablas/07_descriptivas_completas.csv")

cat("\n--- Las más asimétricas ---\n")
print(descriptivas |> arrange(desc(asim)) |>
        select(etiqueta, media, mediana, asim) |>
        mutate(across(where(is.numeric), ~ round(.x, 2))) |> head(8))

cat("\n--- Variables categóricas: tablas de frecuencias ---\n")
for (v in c("tipo_depto", "antiguedad_grupo", "publicado_por", "a_estrenar")) {
  cat("\n", v, "\n", sep = "")
  print(base |> count(.data[[v]], sort = TRUE) |>
          mutate(pct = round(100 * n / n_base, 1)))
}


# =============================================================================
# 2. OBSERVACIONES POR BARRIO
# =============================================================================
# La devolución lo pide textualmente. Es la tabla más sencilla del trabajo y
# faltaba.

cat("\n\n#### 2. OBSERVACIONES POR BARRIO ####\n\n")

frec_barrio <- base |>
  count(barrio_nom, name = "avisos", sort = TRUE) |>
  mutate(pct = 100 * avisos / n_base,
         acumulado = cumsum(pct),
         comuna = base$comuna[match(barrio_nom, base$barrio_nom)])

print(frec_barrio, n = Inf)
write_csv(frec_barrio, "output/tablas/07_frecuencias_barrio.csv")

cat("\nBarrios:", nrow(frec_barrio), "\n")
cat("Top 5 :", coma_ar(sum(head(frec_barrio$pct, 5))), "% de los avisos\n")
cat("Top 10:", coma_ar(sum(head(frec_barrio$pct, 10))), "% de los avisos\n")
cat("Barrios con menos de 100 avisos:", sum(frec_barrio$avisos < 100), "\n")
cat("Barrios con al menos 100 avisos:", sum(frec_barrio$avisos >= 100),
    "que reúnen", coma_ar(sum(frec_barrio$pct[frec_barrio$avisos >= 100])), "% de los avisos\n")


# =============================================================================
# 3. LAS VARIABLES DE CERCANÍA
# =============================================================================
# El docente escribió: "No descartaría las variables de cercanía porque sí,
# hay que justificarlo". El argumento lo teníamos pero nunca lo mostramos:
# estas variables miden centralidad, no cercanía a un servicio en particular.
# Acá lo probamos de dos formas, con correlaciones y con un PCA.

cat("\n\n#### 3. LAS VARIABLES DE CERCANÍA ####\n\n")

vars_cercania <- c(
  dist_centro_m             = "Dist. al Obelisco",
  dist_subte_m              = "Dist. al subte",
  dist_comisaria_m          = "Dist. a comisaría",
  dist_escuela_m            = "Dist. a escuela",
  escuelas_1000m            = "Escuelas a 1 km",
  dist_espacio_verde_m      = "Dist. a espacio verde",
  espacios_verdes_1000m     = "Espacios verdes a 1 km",
  superficie_verde_1000m_m2 = "Sup. verde a 1 km",
  dist_parada_colectivo_m   = "Dist. a parada de colectivo",
  paradas_colectivo_1000m   = "Paradas a 1 km",
  lineas_colectivo_1000m    = "Líneas a 1 km"
)

X <- base |> select(all_of(names(vars_cercania))) |> as.data.frame()
colnames(X) <- unname(vars_cercania)

cat("--- Matriz de correlaciones entre las 11 variables de cercanía ---\n")
mat_cor <- cor(X)
print(round(mat_cor, 2))

cat("\n--- Las correlaciones más fuertes entre pares ---\n")
pares <- as.data.frame(as.table(mat_cor)) |>
  as_tibble() |>
  rename(v1 = Var1, v2 = Var2, r = Freq) |>
  filter(as.character(v1) < as.character(v2)) |>
  arrange(desc(abs(r)))
print(pares |> mutate(r = round(r, 2)) |> head(12))

cat("\nCorrelación media absoluta entre pares:",
    coma_ar(mean(abs(pares$r)), 3), "\n")

# --- PCA ---------------------------------------------------------------------
# PCA resume variables correlacionadas. Si estas once midieran cosas distintas,
# harían falta muchos componentes. Si miden lo mismo, uno solo alcanza.
#
# precio_m2_usd y lat entran como variables SUPLEMENTARIAS: no construyen los
# componentes, se proyectan encima para interpretar. Es la técnica del script
# 01 de la clase 16, que proyecta el IDH sobre un PCA que no lo usó.

cat("\n\n--- PCA sobre las 11 variables de cercanía ---\n")

X_sup <- base |>
  select(all_of(names(vars_cercania)), precio_m2_usd, lat) |>
  as.data.frame()
colnames(X_sup) <- c(unname(vars_cercania), "Precio por m²", "Latitud")

res <- PCA(X_sup, scale.unit = TRUE, quanti.sup = 12:13, graph = FALSE)

cat("\nVarianza explicada por componente:\n")
print(round(res$eig, 2))

cat("\nComponentes con autovalor > 1 (criterio de Kaiser):",
    sum(res$eig[, 1] > 1), "\n")

cat("\n--- Correlación de cada variable con los dos primeros componentes ---\n")
coords <- as_tibble(res$var$coord[, 1:2], rownames = "variable") |>
  rename(PC1 = Dim.1, PC2 = Dim.2) |>
  mutate(contrib_PC1 = res$var$contrib[, 1],
         cos2_PC1 = res$var$cos2[, 1]) |>
  arrange(desc(abs(PC1)))
print(coords |> mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf)

cat("\n--- Las suplementarias: dónde caen el precio por m² y la latitud ---\n")
sup <- as_tibble(res$quanti.sup$coord[, 1:2], rownames = "variable") |>
  rename(PC1 = Dim.1, PC2 = Dim.2)
print(sup |> mutate(across(where(is.numeric), ~ round(.x, 3))))

cat("\nLectura: PC1 es un eje de centralidad. Si el precio por m² queda cerca\n")
cat("del centro del círculo, las once variables de cercanía no lo ordenan.\n")

# Guardamos los scores por si hacen falta después.
scores_cercania <- as_tibble(res$ind$coord[, 1:2]) |>
  rename(centralidad_PC1 = Dim.1, cercania_PC2 = Dim.2)
write_csv(bind_cols(select(base, id), scores_cercania),
          "output/tablas/07_scores_cercania.csv")

cat("\n--- Correlación directa del precio por m² con cada variable ---\n")
cor_precio <- map_dfr(names(vars_cercania), function(v) {
  tibble(variable = vars_cercania[[v]],
         r_pearson  = cor(base$precio_m2_usd, base[[v]]),
         r_spearman = cor(base$precio_m2_usd, base[[v]], method = "spearman"),
         r_con_dist_centro = cor(base$dist_centro_m, base[[v]]))
}) |> arrange(desc(abs(r_spearman)))
print(cor_precio |> mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf)

# Los números que van a los textos de la presentación (script 10).
coord_var <- function(v, eje) coords[[eje]][coords$variable == v]
coord_sup <- function(v, eje) sup[[eje]][sup$variable == v]
guardar_numeros(
  "07_numeros.csv",
  # Las que se muestran en t05 y t06 (geocod_score quedó afuera de la tabla).
  n_descriptivas = nrow(descriptivas) - 1,
  n_barrios = nrow(frec_barrio),
  pct_top5 = sum(head(frec_barrio$pct, 5)),
  pc1 = res$eig[1, 2], pc2 = res$eig[2, 2],
  pm2_pc1 = coord_sup("Precio por m²", "PC1"), pm2_pc2 = coord_sup("Precio por m²", "PC2"),
  lat_pc1 = coord_sup("Latitud", "PC1"), lat_pc2 = coord_sup("Latitud", "PC2"),
  obelisco_pc1 = coord_var("Dist. al Obelisco", "PC1")
)

sink()
cat("\nDiagnóstico guardado en output/tablas/07_devolucion.txt\n")


# =============================================================================
# 4. GRÁFICOS
# =============================================================================

# --- G12. Matriz de correlaciones de las variables de cercanía --------------

orden_cor <- coords$variable[order(coords$PC1)]

# Solo un triángulo: la mitad de arriba repite la de abajo, y la diagonal es
# siempre 1. Quedan 55 celdas en lugar de 121.
mat_largo <- as.data.frame(as.table(mat_cor)) |>
  as_tibble() |>
  rename(v1 = Var1, v2 = Var2, r = Freq) |>
  filter(match(v1, orden_cor) < match(v2, orden_cor)) |>
  mutate(v1 = factor(v1, levels = orden_cor[-length(orden_cor)]),
         v2 = factor(v2, levels = rev(orden_cor[-1])))

g12 <- ggplot(mat_largo, aes(v1, v2, fill = r)) +
  geom_tile(color = colores$fondo, linewidth = 0.8) +
  geom_text(aes(label = coma_ar(r, 2)), size = 2.5,
            family = fuente_cuerpo,
            color = if_else(abs(mat_largo$r) > 0.6, "white", colores$texto)) +
  scale_fill_gradient2(low = colores$acento2, mid = "#EDE7E0",
                       high = colores$acento, midpoint = 0, limits = c(-1, 1),
                       labels = function(x) coma_ar(x, 1)) +
  coord_fixed() +
  labs(x = NULL, y = NULL,
       caption = pie(paste0(fuente_pie,
         ". Variables ordenadas por su carga en el primer componente principal."))) +
  # panel.grid.major hay que anularla explícitamente: el tema del trabajo la
  # define, y anular solo panel.grid no la hereda. Con medio triángulo vacío,
  # las líneas se veían.
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank(), panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.key.height = unit(1.6, "cm"), legend.key.width = unit(0.35, "cm"))

guardar(g12, "g12_correlaciones_cercania.png", ancho = 8.6, alto = 6.4)


# --- G13. El círculo de correlaciones del PCA -------------------------------
# Las once variables activas en terracota, las dos suplementarias en verde
# azulado y con línea punteada: no construyen los componentes, se proyectan.

circulo <- tibble(t = seq(0, 2 * pi, length.out = 200)) |>
  mutate(x = cos(t), y = sin(t))

flechas_act <- coords |> select(variable, PC1, PC2) |> mutate(tipo = "activa")
flechas_sup <- sup   |> select(variable, PC1, PC2) |> mutate(tipo = "suplementaria")

pc1_pct <- res$eig[1, 2]; pc2_pct <- res$eig[2, 2]

# La distancia de un punto al origen es su correlación con el plano de los dos
# componentes. El radio de la zona sin señal se fija en 0,25 y se verifica que
# las dos suplementarias caigan adentro: si no, el gráfico diría algo falso.
radio_sin_senal <- 0.25
stopifnot(all(sqrt(flechas_sup$PC1^2 + flechas_sup$PC2^2) < radio_sin_senal))

# Dónde va cada etiqueta, a mano. Precio por m² a la izquierda del eje
# vertical, en el hueco entre las flechas de espacios verdes y de líneas de
# colectivo: a la derecha quedaba pegada a la de distancia a escuela. Latitud
# abajo a la derecha, entre la flecha al Obelisco y la de distancia a espacio
# verde.
etiq_sup <- flechas_sup |>
  mutate(lx = if_else(variable == "Latitud", PC1 + 0.06, -0.15),
         ly = if_else(variable == "Latitud", PC2 - 0.13, 0.24),
         hj = if_else(variable == "Latitud", 0, 1))
# La leyenda del radio, abajo a la izquierda del círculo chico, entre las
# flechas de paradas y de escuelas (en otro lugar se cruzaba con la de escuelas).
pos_radio <- c(x = -0.27, y = -0.075)

g13 <- ggplot() +
  geom_path(data = circulo, aes(x, y), color = colores$grilla, linewidth = 0.6) +
  geom_hline(yintercept = 0, color = colores$grilla, linewidth = 0.4) +
  geom_vline(xintercept = 0, color = colores$grilla, linewidth = 0.4) +
  geom_segment(data = flechas_act, aes(0, 0, xend = PC1, yend = PC2),
               color = colores$acento, linewidth = 0.6,
               arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  geom_text(data = flechas_act,
            aes(PC1 * 1.08, PC2 * 1.08, label = variable),
            size = 2.8, family = fuente_cuerpo, color = colores$texto,
            hjust = ifelse(flechas_act$PC1 > 0, 0, 1)) +
  # Las suplementarias. Lo que tiene que verse es que caen pegadas al origen:
  # un círculo de radio 0,25 marca la zona sin señal, los dos puntos van
  # grandes y las etiquetas lo más cerca posible. El centro está amontonado de
  # flechas, así que cada etiqueta va en el hueco libre más próximo a su punto,
  # con fondo para que se lea sobre las líneas.
  geom_polygon(data = mutate(circulo, x = radio_sin_senal * x, y = radio_sin_senal * y),
               aes(x, y), fill = colores$acento2, alpha = 0.08,
               color = colores$acento2, linewidth = 0.4, linetype = "22") +
  geom_segment(data = flechas_sup, aes(0, 0, xend = PC1, yend = PC2),
               color = colores$acento2, linewidth = 0.8) +
  geom_point(data = flechas_sup, aes(PC1, PC2), color = colores$acento2,
             fill = colores$fondo, shape = 21, size = 3.6, stroke = 1.4) +
  geom_segment(data = etiq_sup, aes(PC1, PC2, xend = lx, yend = ly),
               color = colores$acento2, linewidth = 0.3) +
  geom_label(data = etiq_sup, aes(lx, ly, label = variable, hjust = hj),
             size = 3.2, family = fuente_cuerpo, color = colores$acento2,
             fontface = "bold", fill = colores$fondo, linewidth = 0,
             label.padding = unit(0.08, "lines")) +
  annotate("text", x = pos_radio[["x"]], y = pos_radio[["y"]],
           label = paste0("radio ", coma_ar(radio_sin_senal, 2)), size = 2.5, hjust = 1,
           family = fuente_cuerpo, color = colores$acento2) +
  coord_fixed(xlim = c(-1.45, 1.45), ylim = c(-1.15, 1.15)) +
  labs(x = paste0("Componente 1: ", coma_ar(pc1_pct), "% de la varianza"),
       y = paste0("Componente 2: ", coma_ar(pc2_pct), "% de la varianza"),
       caption = pie(paste0(fuente_pie,
         ". En terracota, las once variables de cercanía. En verde azulado, el precio por m² y la latitud: ",
         "no construyen los componentes, se proyectan encima. Dentro del círculo sombreado, de radio ",
         coma_ar(radio_sin_senal, 2), ", una variable tiene menos de ", coma_ar(radio_sin_senal, 2),
         " de correlación con el plano de los dos componentes: no hay señal."))) +
  theme(panel.grid.major = element_blank())

guardar(g13, "g13_pca_cercania.png", ancho = 9, alto = 6.2)


# --- G14. Avisos por barrio -------------------------------------------------
# La devolución la pidió explícitamente. Va en el anexo como tabla, pero el
# gráfico sirve para mostrar la concentración de un vistazo.

frec_g <- frec_barrio |>
  mutate(etiqueta = fct_reorder(barrio_nom, avisos),
         destacado = rank(-avisos) <= 5)

g14 <- ggplot(frec_g, aes(avisos, etiqueta, fill = destacado)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = paste0(num_ar(avisos), "  (", coma_ar(pct), "%)")),
            hjust = -0.1, size = 2.5, family = fuente_cuerpo,
            color = colores$secundario) +
  scale_fill_manual(values = c(`TRUE` = colores$acento, `FALSE` = colores$apagado),
                    guide = "none") +
  scale_x_continuous(labels = num_ar, expand = expansion(mult = c(0, 0.22))) +
  labs(x = "Cantidad de avisos", y = NULL,
       caption = pie(paste0(fuente_pie, ". Los cinco primeros barrios reúnen el ",
                            coma_ar(sum(head(frec_barrio$pct, 5))),
                            "% de los avisos; la comuna 8 entera, el ",
                            coma_ar(100 * mean(base$comuna == 8), 2), "%."))) +
  theme(axis.text.y = element_text(size = 7.6),
        panel.grid.major.y = element_blank())

guardar(g14, "g14_avisos_por_barrio.png", ancho = 7.2, alto = 7)


# =============================================================================
# 5. TABLAS COMO IMAGEN
# =============================================================================

fmt_desc <- function(d) {
  d |> rowwise() |>
    mutate(dec = if (maximo < 100) 1 else 0) |>
    mutate(across(c(media, mediana, desvio, minimo, p25, p75, maximo),
                  ~ coma_ar(.x, dec))) |>
    ungroup() |>
    transmute(Variable = etiqueta, Media = media, Mediana = mediana,
              `Desvío` = desvio, CV = coma_ar(cv, 2), `Mínimo` = minimo,
              P25 = p25, P75 = p75, `Máximo` = maximo,
              `Asimetría` = coma_ar(asim, 1))
}

vars_vivienda <- c("precio_usd", "precio_m2_usd", "metros_cubiertos",
                   "ambientes", "dormitorios", "banos", "antiguedad_media")

# Las tablas describen lo que está en la base, también lo que no usamos. Cada
# variable que no entra a los modelos lleva en el pie el porqué.
pct_amb_dorm <- 100 * mean(base$ambientes == base$dormitorios + 1)
t05 <- fmt_desc(filter(descriptivas, variable %in% vars_vivienda))
guardar_tabla(tabla_png(t05, anchos = c(2.6, 1.1, 1.1, 1.1, 0.8, 1, 1, 1, 1.2, 1),
                        pie_tabla = paste0(
                          "Base de análisis, ", num_ar(n_base),
                          " avisos. CV es el coeficiente de variación. Una asimetría positiva indica cola a la derecha. ",
                          "Dos filas se describen porque están en la base, pero no entran a los modelos. La antigüedad ",
                          "como punto medio sale de categorías que se pisan; en los modelos usamos el tramo de ",
                          "antiguedad_grupo, que no se pisa. Los dormitorios repiten a los ambientes: en el ",
                          coma_ar(pct_amb_dorm), "% de los avisos, ambientes = dormitorios + 1.")),
              "t05_descriptivas_vivienda.png", ancho = 11)

# geocod_score no se analiza (de acuerdo con la devolución): sale de la tabla
# y queda solo el dato que sí usamos, su mínimo. El valor de referencia del m²
# del barrio se describe y se explica por qué no entra (decisión 7).
mediana_barrio <- base |> group_by(barrio_nom) |>
  summarise(m = median(precio_m2_usd), ref = first(valor_m2_usd_barrio))
cor_ref <- cor(mediana_barrio$m, mediana_barrio$ref)
t06 <- fmt_desc(filter(descriptivas, !variable %in% c(vars_vivienda, "geocod_score")))
guardar_tabla(tabla_png(t06, anchos = c(3.2, 1.1, 1.1, 1.1, 0.8, 1, 1, 1, 1.2, 1),
                        size = 2.9,
                        pie_tabla = paste0(
                          "Base de análisis, ", num_ar(n_base),
                          " avisos. De las once variables de cercanía solo entra a los modelos la distancia al Obelisco, ",
                          "que representa al bloque; el motivo está en el componente principal. El valor de referencia ",
                          "del m² del barrio es un dato externo, un promedio de precios: explicar el precio con él sería ",
                          "circular, así que no entra a los modelos. Lo usamos solo para validar: la mediana de nuestros ",
                          "avisos por barrio correlaciona ", coma_ar(cor_ref, 2), " con él. La calidad de la ",
                          "geocodificación no se analiza; solo notamos que nunca baja de ",
                          coma_ar(min(base$geocod_score), 0), ", señal de que la base llega recortada.")),
              "t06_descriptivas_entorno.png", ancho = 11.5)

# --- Frecuencias de las variables categóricas (anexo) -----------------------
etiq_var <- c(tipo_depto = "Tipo de departamento", antiguedad_grupo = "Antigüedad",
              publicado_por = "Quién publica", a_estrenar = "A estrenar")

# Las dos variables ordinales van en su orden propio, como en la tabla de la
# regresión; las otras dos, que no tienen orden, por frecuencia.
orden_propio <- list(
  tipo_depto = c("Monoambiente", "1 dormitorio", "2 dormitorios", "3 dormitorios",
                 "4 dormitorios o más"),
  antiguedad_grupo = sort(unique(base$antiguedad_grupo))   # "a. ", "b. "... ya ordenan
)
cat_frec <- map_dfr(names(etiq_var), function(v) {
  x <- base |>
    count(.data[[v]]) |>
    rename(categoria = 1) |>
    mutate(categoria = as.character(categoria))
  x <- if (v %in% names(orden_propio)) {
    stopifnot(setequal(x$categoria, orden_propio[[v]]))
    arrange(x, match(categoria, orden_propio[[v]]))
  } else arrange(x, desc(n))
  mutate(x, variable = v)
})

# a_estrenar es exactamente la categoría "A estrenar" de antiguedad_grupo: los
# mismos avisos, fila por fila. Por eso no entra a los modelos por separado.
stopifnot(identical(base$a_estrenar == 1, base$antiguedad_grupo == "a. A estrenar"))

t09 <- cat_frec |>
  mutate(categoria = if_else(variable == "a_estrenar",
                             if_else(categoria == "1", "Sí", "No"), categoria),
         categoria = str_remove(categoria, "^[a-d]\\. "),
         Variable = etiq_var[variable]) |>
  transmute(Variable, `Categoría` = categoria, Avisos = num_ar(n),
            `%` = paste0(coma_ar(100 * n / n_base), "%")) |>
  # El nombre de la variable se escribe solo en su primera fila.
  group_by(Variable) |>
  mutate(Variable = if_else(row_number() == 1, Variable, "")) |>
  ungroup()

guardar_tabla(tabla_png(t09, anchos = c(2.2, 2.4, 1, 1),
                        alinear = c("izq", "izq", "der", "der"),
                        pie_tabla = paste0(
                          "Base de análisis, ", num_ar(n_base),
                          " avisos. Tipo de departamento y antigüedad van en su orden propio; las otras dos, por frecuencia. ",
                          "A estrenar se muestra porque está en la base, pero son exactamente los mismos ",
                          num_ar(sum(base$a_estrenar == 1)), " avisos de la categoría «A estrenar» de la antigüedad: ",
                          "por eso en los modelos entra solo la antigüedad.")),
              "t09_frecuencias_categoricas.png", ancho = 7.5)

# Barrios: la tabla completa va al anexo, partida en dos columnas para que entre.
mitad <- ceiling(nrow(frec_barrio) / 2)
t07 <- bind_cols(
  frec_barrio |> slice(1:mitad) |>
    # Un decimal en las dos columnas de porcentaje, para que se comparen igual.
    transmute(Barrio = barrio_nom, Avisos = num_ar(avisos),
              `%` = coma_ar(pct, 1), `% acum.` = coma_ar(acumulado, 1)),
  frec_barrio |> slice((mitad + 1):n()) |>
    bind_rows(tibble(barrio_nom = NA, avisos = NA, pct = NA, acumulado = NA)) |>
    slice(1:mitad) |>
    transmute(`Barrio ` = coalesce(barrio_nom, ""),
              `Avisos ` = coalesce(num_ar(avisos), ""),
              `% ` = coalesce(coma_ar(pct, 1), ""),
              `% acum. ` = coalesce(coma_ar(acumulado, 1), ""))
)
guardar_tabla(tabla_png(t07, anchos = c(2, 1, 0.9, 1.1, 2, 1, 0.9, 1.1),
                        alinear = c("izq", "der", "der", "der", "izq", "der", "der", "der"),
                        size = 2.7, alto_fila = 0.26,
                        pie_tabla = paste0(
                          "Los ", nrow(frec_barrio), " barrios de CABA, ordenados por cantidad de avisos. ",
                          "Porcentajes sobre los ", num_ar(n_base), " avisos de la base de análisis: la base cruda sin ",
                          "las republicaciones del mismo inmueble, antes de cualquier recorte de los modelos. ",
                          "La columna acumulada sigue el orden de la primera columna y continúa en la segunda.")),
              "t07_avisos_por_barrio.png", ancho = 10)

# PCA: varianza y cargas.
t08 <- bind_rows(
  coords |> transmute(Variable = variable,
                      `Carga en PC1` = coma_ar(PC1, 2),
                      `Carga en PC2` = coma_ar(PC2, 2),
                      `Contribución a PC1` = paste0(coma_ar(contrib_PC1), "%"),
                      Tipo = "Activa"),
  sup |> transmute(Variable = variable,
                   `Carga en PC1` = coma_ar(PC1, 2),
                   `Carga en PC2` = coma_ar(PC2, 2),
                   `Contribución a PC1` = "—",
                   Tipo = "Suplementaria")
)
guardar_tabla(tabla_png(t08, anchos = c(2.4, 1.2, 1.2, 1.5, 1.4),
                        alinear = c("izq", "der", "der", "der", "izq"),
                        destacar = c(nrow(coords) + 1, nrow(coords) + 2),
                        pie_tabla = paste0(
                          "PC1 explica el ", coma_ar(pc1_pct), "% de la varianza de las once variables y PC2 el ",
                          coma_ar(pc2_pct), "%. Las dos suplementarias no participan del cálculo.")),
              "t08_pca_cargas.png", ancho = 9)

cat("\nGráficos y tablas de la devolución generados.\n")
