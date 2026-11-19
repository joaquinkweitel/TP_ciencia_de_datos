# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 1: Análisis Exploratorio de Datos
# Script 03: gráficos de la presentación
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: input/base_analisis.csv
# Salida : output/graficos/*.png
#          output/tablas/03_titulos_graficos.csv
#
# Criterios que seguimos en todos los gráficos:
#   - el título dice el hallazgo, no el nombre de la variable;
#   - el acento terracota se reserva para lo que queremos que se mire, el
#     resto va en gris;
#   - las categorías se ordenan por valor, nunca alfabéticamente;
#   - los números van con punto de miles y coma decimal, y la unidad se
#     aclara en el nombre del eje, no repetida en cada etiqueta;
#   - todos llevan el mismo pie de fuente.
#
# Sobre los títulos. Los gráficos se exportan sin el título afirmativo ni la
# bajada: el título lo pone la diapositiva, en posición y tamaño fijos, y así
# no queda duplicado. Lo que sí llevan adentro es su nombre descriptivo (qué
# es la figura, no el hallazgo), que pone guardar() con MOSTRAR_NOMBRES
# (script/00_tema.R) desde auxiliar/nombres_figuras.csv. Los títulos y bajadas
# se definen igual acá, en la tabla `textos`, porque el script 05 los lee de
# ahí; poniendo MOSTRAR_TITULOS en TRUE (y MOSTRAR_NOMBRES en FALSE) se
# regeneran los gráficos con el título afirmativo adentro, para usarlos
# sueltos fuera de la presentación.
#
# El tema y la paleta están en script/00_tema.R.
# =============================================================================

library(tidyverse)
source("script/00_tema.R")

MOSTRAR_TITULOS <- FALSE

base <- read_csv("input/base_analisis.csv", show_col_types = FALSE) |>
  mutate(
    # Norte primero, para que el orden de las barras coincida con el de la
    # referencia de color.
    mitad_ciudad = factor(mitad_ciudad, levels = c("Norte", "Sur")),
    antiguedad = factor(antiguedad,
                        levels = c("A estrenar", "Hasta 20 años",
                                   "20 a 50 años", "Más de 50 años"))
  )

cat("Avisos en la base de análisis:", num_ar(nrow(base)), "\n\n")

# Escala de relleno norte-sur, igual en los dos gráficos que la usan.
relleno_mitad <- scale_fill_manual(
  values = c(Norte = colores$acento, Sur = colores$acento2)
)


# =============================================================================
# 1. LOS DATOS QUE ALIMENTAN CADA GRÁFICO
# =============================================================================
# Los calculamos todos juntos y antes de dibujar, porque varios títulos usan
# estos números y así no quedan escritos a mano en dos lugares.

# --- Mapa: celdas de unos 200 metros -----------------------------------------
# Con 39.305 avisos, un punto por aviso es una mancha. Agrupamos en celdas y
# coloreamos cada una con la MEDIANA del precio por m² de los avisos que caen
# adentro, para que cada celda pese igual y los edificios con muchas unidades
# publicadas no tapen al resto.

paso_lat  <- 0.0018   # ~200 m
paso_long <- 0.0022   # ~200 m a la latitud de Buenos Aires

celdas <- base |>
  mutate(
    celda_lat  = floor(lat  / paso_lat)  * paso_lat  + paso_lat  / 2,
    celda_long = floor(long / paso_long) * paso_long + paso_long / 2
  ) |>
  group_by(celda_lat, celda_long) |>
  summarise(avisos = n(), precio_m2 = median(precio_m2_usd), .groups = "drop")

# La escala de color se recorta en los percentiles 5 y 95 para que el gradiente
# no quede dominado por Puerto Madero. Los valores fuera de ese rango se
# dibujan con el color del extremo (oob = squish), no se descartan.
limites_color <- quantile(celdas$precio_m2, c(0.05, 0.95))

cat("Celdas del mapa:", num_ar(nrow(celdas)),
    "| mediana de avisos por celda:", median(celdas$avisos), "\n")

# --- Ranking de barrios ------------------------------------------------------
# Mediana y no promedio: la distribución del precio por m² es asimétrica y el
# promedio se corre hacia arriba por unos pocos avisos caros.

ranking <- base |>
  group_by(barrio_nom) |>
  summarise(avisos = n(), precio_m2 = median(precio_m2_usd), .groups = "drop") |>
  arrange(precio_m2) |>
  mutate(
    etiqueta = fct_inorder(paste0(barrio_nom, "  (", num_ar(avisos), ")")),
    destacado = rank(-precio_m2) <= 3 | rank(precio_m2) <= 3,
    pocos_avisos = avisos < 100,
    # Los 48 barrios en una sola columna dejan cada barra en 7 puntos de alto y
    # los nombres se tocan. Los partimos en dos columnas de 24: la primera
    # lleva los más caros, la segunda los más baratos.
    columna = if_else(rank(-precio_m2) <= 24, "caros", "baratos")
  )

brecha_barrios <- max(ranking$precio_m2) / min(ranking$precio_m2)

# --- Norte y sur por tramo de distancia al centro ----------------------------

por_distancia <- base |>
  mutate(tramo_centro = cut(
    dist_centro_km, breaks = c(0, 3, 6, 9, 15),
    labels = c("Hasta 3 km", "3 a 6 km", "6 a 9 km", "Más de 9 km"))) |>
  group_by(tramo_centro, mitad_ciudad) |>
  summarise(avisos = n(), precio_m2 = median(precio_m2_usd), .groups = "drop")

brechas_dist <- por_distancia |>
  pivot_wider(id_cols = tramo_centro, names_from = mitad_ciudad,
              values_from = precio_m2) |>
  mutate(brecha = 100 * (Norte / Sur - 1))

cat("\nBrecha norte-sur por tramo de distancia (%):\n")
print(brechas_dist |> mutate(brecha = round(brecha, 1)))

# --- Norte y sur por tramo de antigüedad -------------------------------------

antig_mitad <- base |>
  group_by(antiguedad, mitad_ciudad) |>
  summarise(avisos = n(), precio_m2 = median(precio_m2_usd), .groups = "drop")

brechas_antig <- antig_mitad |>
  pivot_wider(id_cols = antiguedad, names_from = mitad_ciudad,
              values_from = precio_m2) |>
  mutate(brecha = 100 * (Norte / Sur - 1))

cat("\nBrecha norte-sur por tramo de antigüedad (%):\n")
print(brechas_antig |> mutate(brecha = round(brecha, 1)))

# --- Categorías de antigüedad, con sus rangos --------------------------------
# antiguedad_cat no clasifica: registra qué filtro de búsqueda usó quien
# publicó. Dibujamos cada categoría como un segmento entre su extremo inferior
# y su extremo superior para que el solapamiento se vea.

rangos <- base |>
  group_by(antiguedad_cat) |>
  summarise(avisos = n(),
            desde = first(antiguedad_min),
            hasta = first(antiguedad_max),
            .groups = "drop") |>
  mutate(
    abierta = is.na(hasta),
    hasta_dibujo = if_else(abierta, 66, hasta),
    # "estrenar" es un punto, no un rango: le damos un ancho mínimo visible.
    hasta_dibujo = if_else(desde == hasta_dibujo, desde + 0.8, hasta_dibujo),
    etiqueta = if_else(antiguedad_cat == "estrenar", "A estrenar", antiguedad_cat),
    empieza_en_1 = desde == 1
  ) |>
  arrange(desde, hasta_dibujo) |>
  mutate(etiqueta = fct_inorder(etiqueta))

# --- Precio y superficie -----------------------------------------------------
# Recortamos la VISTA en 250 m² y US$ 1.000.000 para que se lea la nube, donde
# está el 98% de los avisos. El recorte es del gráfico, no de los datos: la
# línea suavizada y las estadísticas se calculan sobre la base completa.

fuera_vista    <- sum(base$precio_usd > 1e6 | base$metros_cubiertos > 250)
media_precio   <- mean(base$precio_usd)
mediana_precio <- median(base$precio_usd)


# =============================================================================
# 2. LOS TÍTULOS
# =============================================================================
# Un solo lugar donde se escribe el hallazgo de cada gráfico. El script 05 lee
# este CSV para armar las diapositivas.

coma <- function(x, dec = 1) format(round(x, dec), decimal.mark = ",", trim = TRUE)

textos <- tribble(
  ~id,   ~titulo, ~bajada,

  "g1",
  "El precio por m² dibuja un gradiente que va del sur al norte de la ciudad",
  "Mediana del precio por m² en celdas de unos 200 metros de lado. Cada celda agrupa los avisos que caen adentro.",

  "g2",
  paste0("Entre el barrio más caro y el más barato el precio por m² se multiplica por ",
         coma(brecha_barrios)),
  "Mediana del precio por m² de los avisos publicados en cada barrio. Entre paréntesis, la cantidad de avisos.",

  "g3",
  paste0("A igual distancia del centro, el norte es entre un ",
         round(min(brechas_dist$brecha)), "% y un ",
         round(max(brechas_dist$brecha)), "% más caro"),
  "Mediana del precio por m² según la distancia al Obelisco, con la ciudad partida por la latitud mediana.",

  "g4",
  "El precio total sigue de cerca a la superficie, y la dispersión crece con el tamaño",
  paste0("Un punto por aviso (n = ", num_ar(nrow(base)),
         "). La línea resume la tendencia de la nube; no es una regresión."),

  "g5",
  "Las categorías de antigüedad se superponen: cuatro arrancan en el mismo año",
  "Rango que cubre cada categoría tal como la informa el portal. La última es abierta y no tiene extremo superior.",

  "g6",
  "La brecha norte-sur se sostiene dentro de cada tramo de antigüedad",
  "Mediana del precio por m² según la antigüedad declarada y la mitad de la ciudad.",

  "g7",
  paste0("La mitad de los avisos pide menos de ", usd_ar(mediana_precio),
         " y la cola derecha estira el promedio"),
  paste0("Distribución del precio publicado, en intervalos de US$ 25.000. Quedan fuera del gráfico ",
         num_ar(sum(base$precio_usd > 1e6)), " avisos por encima de US$ 1.000.000."),

  "g8",
  paste0("Casi todos los avisos son de departamentos chicos: la mediana es de ",
         median(base$metros_cubiertos), " m²"),
  "Distribución de la superficie cubierta, en intervalos de 5 m². Los dos extremos de la variable son valores exactos."
)

write_csv(textos, "output/tablas/03_titulos_graficos.csv")

# Aplica el título y la bajada al gráfico. Por defecto no los pone, porque en
# la presentación los escribe la diapositiva. Con con_titulo = TRUE se obtiene
# el gráfico autocontenido, para usarlo suelto.
rotular <- function(p, id, con_titulo = MOSTRAR_TITULOS,
                    ancho_titulo = 62, ancho_bajada = 105) {
  if (!con_titulo) return(p)
  fila <- textos[textos$id == id, ]
  p + labs(title    = envolver(fila$titulo, ancho_titulo),
           subtitle = envolver(fila$bajada, ancho_bajada))
}


# =============================================================================
# 3. LOS GRÁFICOS
# =============================================================================

# --- G1. El mapa -------------------------------------------------------------
# coord_fixed mantiene la proporción real de la ciudad: sin eso, el mapa sale
# estirado. El factor corrige que un grado de longitud mide menos que uno de
# latitud a la altura de Buenos Aires.

g1 <- ggplot(celdas, aes(celda_long, celda_lat, color = precio_m2)) +
  geom_point(size = 1.3, shape = 15) +
  scale_color_gradientn(
    colours = escala_continua,
    limits  = limites_color,
    oob     = scales::squish,
    labels  = num_ar,
    # Pocos cortes: con la barra angosta, más etiquetas se pisan entre sí.
    breaks  = c(1500, 2500, 3500),
    name    = "Precio por m² (US$)",
    guide   = guide_colorbar(barwidth = 9, barheight = 0.4,
                             ticks.colour = NA, title.position = "top")
  ) +
  coord_fixed(ratio = 1 / cos(34.6 * pi / 180)) +
  labs(caption = pie(paste0(fuente_pie,
                            ". Escala recortada en los percentiles 5 y 95 (US$ ",
                            num_ar(limites_color[1]), " a US$ ",
                            num_ar(limites_color[2]), ")."), 62)) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(color = colores$secundario, size = 10)
  )

guardar(rotular(g1, "g1", ancho_titulo = 42, ancho_bajada = 58),
        "g1_mapa_precio_m2.png", ancho = 4.4, alto = 5.7)


# --- G2. Ranking de los 48 barrios -------------------------------------------
# El nombre de los barrios con menos de 100 avisos va en un gris más claro:
# su mediana se apoya en pocos casos. No usamos un tercer color de barra.

# Los nombres de los barrios van dibujados adentro del panel y no como
# etiquetas del eje. El motivo es concreto: cuando hay facetas, ggplot recicla
# el vector de colores de axis.text.y en cada panel, así que el segundo panel
# heredaría los colores del primero. Como geom_text, el color se mapea a la
# variable y cada barrio queda con el suyo.

g2 <- ggplot(ranking, aes(precio_m2, etiqueta)) +
  geom_col(aes(fill = destacado), width = 0.72) +
  # Nombre del barrio, a la izquierda del cero.
  geom_text(aes(x = 0, label = paste0(barrio_nom, "  (", num_ar(avisos), ")"),
                color = pocos_avisos),
            hjust = 1, nudge_x = -130, size = 2.85, family = fuente_cuerpo) +
  # Valor de la mediana, a la derecha de cada barra.
  geom_text(aes(label = num_ar(precio_m2)),
            hjust = -0.2, size = 2.7, family = fuente_cuerpo,
            color = colores$secundario) +
  facet_wrap(~ factor(columna, levels = c("caros", "baratos")),
             scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(`TRUE` = colores$acento,
                               `FALSE` = colores$apagado), guide = "none") +
  scale_color_manual(values = c(`TRUE` = colores$apagado,
                                `FALSE` = colores$secundario), guide = "none") +
  scale_x_continuous(labels = num_ar, breaks = c(0, 2000, 4000),
                     expand = expansion(mult = c(0.92, 0.22))) +
  labs(
    x = "Precio por m² (US$)", y = NULL,
    caption = pie(paste0(fuente_pie,
                         ". Los barrios con menos de 100 avisos figuran con el nombre en gris claro: su mediana se apoya en pocos casos."))
  ) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_blank(),
    panel.spacing.x = unit(1.4, "lines")
  )

guardar(rotular(g2, "g2"), "g2_ranking_barrios.png", ancho = 10, alto = 5.3)


# --- G3. No es centro y periferia: es norte y sur ----------------------------

g3 <- ggplot(por_distancia,
             aes(tramo_centro, precio_m2, fill = mitad_ciudad)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  geom_text(aes(label = num_ar(precio_m2)),
            position = position_dodge(width = 0.78), vjust = -0.6,
            size = 3.2, family = fuente_cuerpo, color = colores$secundario) +
  relleno_mitad +
  scale_y_continuous(labels = num_ar, expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Distancia al Obelisco", y = "Precio por m² (US$)",
       # El pie dice que son medianas y cuántos avisos sostienen cada barra: es
       # el gráfico del hallazgo principal y la pregunta obvia es si alguna
       # barra se apoya en pocos casos. Ninguna: la más chica tiene más de 2.600.
       caption = pie(paste0(fuente_pie, ". Medianas, no promedios. Cada barra reúne entre ",
                            num_ar(min(por_distancia$avisos)), " y ",
                            num_ar(max(por_distancia$avisos)), " avisos.")))

guardar(rotular(g3, "g3"), "g3_norte_sur_distancia.png", ancho = 10, alto = 4.7)

# Los números que usa el texto de la diapositiva 1 (script 10): en qué tramo de
# distancia está el máximo y el mínimo de cada mitad.
viejo_anillo <- base |>
  mutate(tramo_centro = cut(dist_centro_km, breaks = c(0, 3, 6, 9, 15),
                            labels = c("Hasta 3 km", "3 a 6 km", "6 a 9 km", "Más de 9 km"))) |>
  group_by(tramo_centro) |>
  summarise(pct_mas50 = 100 * mean(antiguedad == "Más de 50 años"), .groups = "drop")

extremo_tramo <- function(mitad, f) {
  x <- filter(por_distancia, mitad_ciudad == mitad)
  as.character(x$tramo_centro[f(x$precio_m2)])
}
guardar_numeros(
  "03_numeros.csv",
  tramo_max_norte = extremo_tramo("Norte", which.max),
  tramo_max_sur   = extremo_tramo("Sur", which.max),
  tramo_min_norte = extremo_tramo("Norte", which.min),
  tramo_min_sur   = extremo_tramo("Sur", which.min),
  min_cerca_norte = por_distancia$precio_m2[por_distancia$mitad_ciudad == "Norte" &
                                              por_distancia$tramo_centro == "Hasta 3 km"],
  min_cerca_sur   = por_distancia$precio_m2[por_distancia$mitad_ciudad == "Sur" &
                                              por_distancia$tramo_centro == "Hasta 3 km"],
  avisos_min = min(por_distancia$avisos), avisos_max = max(por_distancia$avisos),
  # El anillo de hasta 3 km concentra el stock viejo. Lo usa la diapositiva 13
  # para conectar el grupo viejo del kmeans con el anillo más barato del g3.
  # Se mide con la antigüedad declarada y no con el grupo del kmeans, que se
  # arma en parte con el precio por m²: así no es circular.
  pct_mas50_cerca = viejo_anillo$pct_mas50[viejo_anillo$tramo_centro == "Hasta 3 km"],
  pct_mas50_resto_max = max(viejo_anillo$pct_mas50[viejo_anillo$tramo_centro != "Hasta 3 km"]),
  # La brecha norte-sur (cociente de medianas) en cada tramo de antigüedad. Va
  # al respaldo de la síntesis: crece en los tramos viejos, la misma idea del
  # kmeans con otra partición.
  brecha_estrenar = 1 + brechas_antig$brecha[brechas_antig$antiguedad == "A estrenar"] / 100,
  brecha_hasta20  = 1 + brechas_antig$brecha[brechas_antig$antiguedad == "Hasta 20 años"] / 100,
  brecha_20a50    = 1 + brechas_antig$brecha[brechas_antig$antiguedad == "20 a 50 años"] / 100,
  brecha_mas50    = 1 + brechas_antig$brecha[brechas_antig$antiguedad == "Más de 50 años"] / 100
)


# --- G4. Superficie y precio -------------------------------------------------

g4 <- ggplot(base, aes(metros_cubiertos, precio_usd)) +
  geom_point(alpha = 0.05, size = 0.7, color = colores$apagado) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              se = FALSE, color = colores$acento, linewidth = 1.1) +
  scale_x_continuous(labels = num_ar, breaks = seq(0, 250, 50)) +
  scale_y_continuous(labels = num_ar, breaks = seq(0, 1e6, 2.5e5)) +
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 1e6)) +
  labs(
    x = "Superficie cubierta (m²)", y = "Precio publicado (US$)",
    # El pie se envuelve más angosto que en los demás gráficos porque este se
    # exporta a 5,7 pulgadas para ir al lado de otro en la misma diapositiva.
    caption = pie(paste0(fuente_pie, ". Quedan fuera de la vista ",
                         num_ar(fuera_vista), " avisos (",
                         coma(100 * fuera_vista / nrow(base)),
                         "%) por superar los 250 m² o el millón de dólares."), 72)
  )

guardar(rotular(g4, "g4"), "g4_superficie_precio.png", ancho = 5.7, alto = 4.3)


# --- G5. Las categorías de antigüedad se pisan -------------------------------

g5 <- ggplot(rangos, aes(y = etiqueta)) +
  # Las categorías cerradas, como barras de extremo a extremo.
  geom_segment(data = ~ filter(.x, !abierta),
               aes(x = desde, xend = hasta_dibujo, yend = etiqueta,
                   color = empieza_en_1),
               linewidth = 5, lineend = "butt") +
  # La categoría abierta no tiene extremo superior. La dibujamos hasta un valor
  # arbitrario y lo aclaramos en la etiqueta: una flecha sobre una barra de
  # este grosor sale como un borrón.
  geom_segment(data = ~ filter(.x, abierta),
               aes(x = desde, xend = hasta_dibujo, yend = etiqueta),
               color = colores$apagado, linewidth = 5, lineend = "butt") +
  geom_text(aes(x = hasta_dibujo,
                label = if_else(abierta,
                                paste0("  sin tope,  ", num_ar(avisos), " avisos"),
                                paste0("  ", num_ar(avisos), " avisos"))),
            hjust = 0, size = 3, family = fuente_cuerpo,
            color = colores$secundario) +
  scale_color_manual(values = c(`TRUE` = colores$acento,
                                `FALSE` = colores$apagado), guide = "none") +
  scale_x_continuous(breaks = seq(0, 60, 10),
                     expand = expansion(mult = c(0.02, 0.2))) +
  labs(
    # El pie lleva solo la fuente y la fecha, como en el resto de los gráficos.
    # La conclusión de este gráfico, que por eso describimos la antigüedad con
    # antiguedad_grupo, es interpretación y va en la diapositiva 3.
    x = "Años de antigüedad", y = NULL,
    # El pie dice qué marca el terracota: sin eso, el resaltado no se entiende.
    caption = pie(paste0(fuente_pie, ". En terracota, las ", sum(rangos$empieza_en_1),
                         " categorías que arrancan en el año 1: un aviso de 3 años puede caer en ",
                         "cualquiera de ellas, así que no clasifican."))
  ) +
  theme(panel.grid.major.y = element_blank())

guardar(rotular(g5, "g5"), "g5_antiguedad_solapada.png", ancho = 10, alto = 4.7)


# --- G6. La antigüedad no explica la brecha norte-sur ------------------------

g6 <- ggplot(antig_mitad, aes(antiguedad, precio_m2, fill = mitad_ciudad)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  geom_text(aes(label = num_ar(precio_m2)),
            position = position_dodge(width = 0.78), vjust = -0.6,
            size = 3.2, family = fuente_cuerpo, color = colores$secundario) +
  relleno_mitad +
  scale_y_continuous(labels = num_ar, expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Precio por m² (US$)", caption = pie(fuente_pie))

guardar(rotular(g6, "g6"), "g6_antiguedad_mitad.png", ancho = 10, alto = 4.7)


# --- G7. Distribución del precio ---------------------------------------------

g7 <- ggplot(base, aes(precio_usd)) +
  geom_histogram(binwidth = 25000, fill = colores$apagado,
                 color = colores$fondo, linewidth = 0.2, boundary = 0) +
  geom_vline(xintercept = mediana_precio, color = colores$acento,
             linewidth = 0.7) +
  geom_vline(xintercept = media_precio, color = colores$acento2,
             linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = mediana_precio, y = Inf,
           label = paste0("Mediana ", usd_ar(mediana_precio)),
           hjust = 1.06, vjust = 1.8, size = 3.2, family = fuente_cuerpo,
           color = colores$acento) +
  annotate("text", x = media_precio, y = Inf,
           label = paste0("Media ", usd_ar(media_precio)),
           hjust = -0.06, vjust = 1.8, size = 3.2, family = fuente_cuerpo,
           color = colores$acento2) +
  scale_x_continuous(labels = num_ar, breaks = seq(0, 1e6, 2e5)) +
  scale_y_continuous(labels = num_ar, expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 1e6)) +
  # Como los otros dos gráficos recortados, dice cuántos avisos quedan afuera.
  labs(x = "Precio publicado (US$)", y = "Cantidad de avisos",
       caption = pie(paste0(fuente_pie, ". Quedan fuera de la vista ",
                            num_ar(sum(base$precio_usd > 1e6)),
                            " avisos por encima de US$ 1.000.000.")))

guardar(rotular(g7, "g7"), "g7_distribucion_precio.png", ancho = 10, alto = 4.7)


# --- G8. La superficie -------------------------------------------------------
# Este histograma muestra en qué rango se concentra la oferta publicada, que es
# lo que sostiene la diapositiva donde va. NO lo usamos como evidencia del
# recorte previo: el corte en 400 m² cae en una zona de densidad casi nula y no
# se llega a ver. Esa evidencia es la tabla de mínimos y máximos del script 04.
# Marcamos el rango intercuartílico, que es lo que interesa acá.

rango_central <- quantile(base$metros_cubiertos, c(0.25, 0.75))

g8 <- ggplot(base, aes(metros_cubiertos)) +
  annotate("rect", xmin = rango_central[1], xmax = rango_central[2],
           ymin = 0, ymax = Inf, fill = colores$acento, alpha = 0.10) +
  geom_histogram(binwidth = 5, fill = colores$apagado, color = colores$fondo,
                 linewidth = 0.2, boundary = 15) +
  annotate("text", x = rango_central[2] + 8, y = Inf,
           label = paste0("La mitad de los avisos\nestá entre ",
                          rango_central[1], " y ", rango_central[2], " m²"),
           hjust = 0, vjust = 1.4, size = 3.1, lineheight = 1.05,
           family = fuente_cuerpo, color = colores$acento) +
  scale_x_continuous(labels = num_ar, breaks = seq(0, 250, 50)) +
  scale_y_continuous(labels = num_ar, expand = expansion(mult = c(0, 0.05))) +
  # Recortamos la VISTA en 250 m², no la escala: de 150 en adelante casi no hay
  # datos y dos tercios del gráfico quedaban vacíos. Con coord_cartesian los
  # avisos grandes siguen contando en los intervalos, solo no se ven. Además
  # queda con el mismo tope que el gráfico de superficie contra precio, que va
  # al lado en la misma diapositiva.
  coord_cartesian(xlim = c(0, 250)) +
  labs(x = "Superficie cubierta (m²)", y = "Cantidad de avisos",
       caption = pie(paste0(fuente_pie, ". Quedan fuera de la vista ",
                            num_ar(sum(base$metros_cubiertos > 250)),
                            " avisos por encima de 250 m²."), 72))

guardar(rotular(g8, "g8"), "g8_superficie.png", ancho = 5.7, alto = 4.3)

cat("\nListo. Gráficos en output/graficos/\n")

