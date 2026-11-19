# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 3: visualización y comunicación de resultados
# Script 11: los dos gráficos de la entrega (exploratorio y comunicativo)
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: output/tablas/09_barrio_grupo.csv y 09_perfiles.csv (script 09),
#          input/base_analisis.csv, y el modelo del paso 3 del script 08,
#          que se vuelve a correr acá (no se duplica la definición de la
#          muestra ni del modelo).
# Salida : output/graficos/grupo_08_exploratorio.jpg y grupo_08_comunicativo.jpg,
#          output/tablas/11_numeros.csv
#
# Esta entrega son dos imágenes que se tienen que sostener solas: a
# diferencia de los gráficos de las instancias 1 y 2, cada una lleva adentro
# título, bajada, nombres de ejes y fuente con el método. Los números de
# títulos y anotaciones salen de los objetos, nunca escritos a mano.
#
# Hay que correr antes 06, 08 y 09 (este script vuelve a correr el 08).
# =============================================================================

library(tidyverse)
library(patchwork)
source("script/00_tema.R")

# ari(), pintar() y segmentos_dendro() están en utils/ (las carga 00_tema.R).

# El tema del trabajo, con el tamaño de letra calibrado para 24 cm de ancho.
# Las imágenes anteriores salían a 9 o 10 pulgadas (23 a 25 cm) con base 14;
# acá la base baja a 12 porque estas imágenes llevan además título y bajada
# largos, y en el exploratorio entran 42 nombres de barrio.
base_letra <- 12
theme_set(tema_tp(base_size = base_letra))

ancho_cm <- 24
guardar_jpg <- function(grafico, nombre, alto_cm) {
  # ggsave con device = "jpeg" usa ragg (está instalado), que toma Segoe UI y
  # Georgia del sistema y escala el texto con el dpi del archivo. El fondo se
  # pasa también con bg: JPG no tiene transparencia.
  ggsave(file.path("output/graficos", nombre), grafico, device = "jpeg",
         width = ancho_cm, height = alto_cm, units = "cm", dpi = 300,
         bg = colores$fondo)
  cat("guardado:", nombre, sprintf("(%d x %d px)\n",
                                   round(ancho_cm / 2.54 * 300), round(alto_cm / 2.54 * 300)))
}

fuente_jpg <- paste0("Fuente: elaboración propia en base a avisos de departamentos en venta en ",
                     "MercadoLibre, Ciudad de Buenos Aires, relevados en noviembre de 2024.")


# =============================================================================
# 1. EXPLORATORIO: LOS BARRIOS AGRUPADOS POR SU COMPOSICIÓN
# =============================================================================
# Lo pidió la devolución de la instancia 2: con el cluster de cada aviso ya
# definido, armar un cluster jerárquico de los barrios según la proporción de
# sus avisos en cada cluster.
#
# La matriz de 42 barrios x 3 tipos sale del script 09 (09_barrio_grupo.csv):
# no se vuelve a correr el kmeans.

cat("\n#### 1. EXPLORATORIO ####\n\n")

barrio_grupo <- read_csv("output/tablas/09_barrio_grupo.csv", show_col_types = FALSE)
perfil       <- read_csv("output/tablas/09_perfiles.csv", show_col_types = FALSE)

# Los nombres de los tres tipos, verificados contra los perfiles como en el
# script 10: si el kmeans cambiara, el script se frena en vez de mentir.
stopifnot(which.max(perfil$pct_mas50) == 1, which.min(perfil$m2) == 2,
          which.max(perfil$m2) == 3, which.max(perfil$precio_m2) == 3)
nombre_tipo <- c(`1` = "Viejos y baratos", `2` = "Nuevos y chicos", `3` = "Grandes y caros")

composicion <- barrio_grupo |>
  select(barrio_nom, grupo, pct) |>
  pivot_wider(names_from = grupo, values_from = pct, values_fill = 0, names_prefix = "g") |>
  arrange(barrio_nom)
M <- as.matrix(composicion[, c("g1", "g2", "g3")]) / 100
rownames(M) <- composicion$barrio_nom
stopifnot(nrow(M) == 42, all(abs(rowSums(M) - 1) < 1e-9))

# La geografía de cada barrio: su comuna (cada barrio está entero en una) y
# la mitad de la ciudad donde están la mayoría de sus avisos. La mitad se
# definió en el script 02 con la latitud mediana de los avisos.
geo <- read_csv("input/base_analisis.csv", show_col_types = FALSE) |>
  filter(barrio_nom %in% rownames(M)) |>
  group_by(barrio_nom) |>
  summarise(comunas = n_distinct(comuna), comuna = first(comuna),
            mitad = if_else(mean(mitad_ciudad == "Norte") > 0.5, "Norte", "Sur"),
            .groups = "drop") |>
  arrange(barrio_nom)
stopifnot(identical(geo$barrio_nom, rownames(M)), all(geo$comunas == 1))

# --- La distancia y el enlace -------------------------------------------------
# Elegimos distancia euclídea entre las proporciones y enlace de Ward. Ward
# une los grupos que menos aumentan la varianza dentro de cada uno: es la
# misma lógica del kmeans del que salen las proporciones, y con distancia
# euclídea es el resultado más conservador.
#
# PARA LA DEFENSA. Probamos alternativas:
#   - Distancia de Hellinger (euclídea sobre la raíz de las proporciones) +
#     Ward: el ARI contra la mitad norte-sur sube de 0,26 a 0,50. Hellinger es
#     la distancia que habitualmente se recomienda para datos de composición
#     (proporciones que suman 1). No la usamos porque elegimos por el método y
#     no por el resultado: la euclídea es la del kmeans. Dicho así, la
#     asociación con la geografía es de al menos 0,26.
#   - Enlace promedio y completo (euclídea o Manhattan): con dos grupos dan
#     41 barrios contra 1 (Puerto Madero solo). No es un agrupamiento, así que
#     ahí la geografía no puede aparecer ni dejar de aparecer.
#   Entre los métodos que sí forman grupos, la geografía aparece en los dos.
#
# k = 2. Con k = 3, Ward deja a Puerto Madero solo en un tercer grupo, y un
# grupo de un barrio no es un grupo.

h <- hclust(dist(M), method = "ward.D2")
k <- 2
grupo_b <- cutree(h, k)

alternativas <- list(
  "Euclídea + Ward"     = h,
  "Hellinger + Ward"    = hclust(dist(sqrt(M)), method = "ward.D2"),
  "Euclídea + promedio" = hclust(dist(M), method = "average"),
  "Euclídea + completo" = hclust(dist(M), method = "complete")
)
comparacion_jer <- imap_dfr(alternativas, function(hh, nombre) {
  cc <- cutree(hh, k)
  tibble(metodo = nombre, tamanos = paste(sort(table(cc), decreasing = TRUE), collapse = " / "),
         ari_mitad = ari(cc, geo$mitad), ari_comuna = ari(cc, geo$comuna))
})
print(comparacion_jer |> mutate(across(where(is.double), ~ round(.x, 3))))

# ¿Es más de lo que daría el azar? Permutación de las mitades entre barrios.
set.seed(2026)
ari_mitad <- ari(grupo_b, geo$mitad)
ari_comuna <- ari(grupo_b, geo$comuna)
p_perm <- mean(replicate(5000, ari(grupo_b, sample(geo$mitad))) >= ari_mitad)
cat("\nARI contra la mitad:", round(ari_mitad, 3), " (p de permutación:", p_perm, ")\n")
cat("ARI contra la comuna:", round(ari_comuna, 3), "\n")

# Qué grupo es cuál: el de mayor proporción media de stock viejo.
resumen_b <- tibble(grupo_b, as_tibble(M), mitad = geo$mitad) |>
  group_by(grupo_b) |>
  summarise(barrios = n(), viejo = mean(g1), nuevo = mean(g2), grande = mean(g3),
            sur = sum(mitad == "Sur"), norte = sum(mitad == "Norte"), .groups = "drop")
print(resumen_b)
g_viejo <- resumen_b$grupo_b[which.max(resumen_b$viejo)]
fila_viejo <- filter(resumen_b, grupo_b == g_viejo)
fila_otro  <- filter(resumen_b, grupo_b != g_viejo)
# El título afirma que el grupo viejo es casi todo del sur: se verifica.
stopifnot(fila_viejo$sur / fila_viejo$barrios >= 0.8)

ari_hellinger <- comparacion_jer$ari_mitad[comparacion_jer$metodo == "Hellinger + Ward"]
tam_promedio  <- comparacion_jer$tamanos[comparacion_jer$metodo == "Euclídea + promedio"]

# --- Coordenadas del dendrograma ---------------------------------------------
# Se dibuja con ggplot a partir de la matriz de uniones de hclust, para que
# use el mismo tema que el resto (segmentos_dendro() está en utils/).
seg <- segmentos_dendro(h)

# El orden de los barrios en los dos paneles: el del dendrograma.
orden <- tibble(barrio_nom = rownames(M)[h$order], pos = seq_along(h$order)) |>
  left_join(geo, by = "barrio_nom") |>
  mutate(grupo_b = grupo_b[barrio_nom],
         etiqueta = paste0(barrio_nom, " (", comuna, ")"),
         letra = if_else(mitad == "Norte", "N", "S"))

centro_grupo <- orden |> group_by(grupo_b) |> summarise(centro = mean(pos), .groups = "drop")
lim_y <- c(0.4, nrow(M) + 0.6)

# Cada grupo de barrios se rotula a la izquierda de la raíz, a la altura de su
# rama, donde no hay líneas del árbol. El corte en dos grupos se marca en el
# panel de las barras (línea punteada entre los grupos): una línea vertical
# acá cruzaba los rótulos.
rotulos_grupo <- tibble(
  grupo_b = c(g_viejo, fila_otro$grupo_b),
  texto = c(paste0("Más stock\nviejo:\n", fila_viejo$barrios, " barrios,\n", fila_viejo$sur, " del sur"),
            paste0("Más stock\nnuevo y\ngrande:\n", fila_otro$barrios, " barrios,\n", fila_otro$norte,
                   " del norte"))) |>
  left_join(centro_grupo, by = "grupo_b") |>
  mutate(x = max(h$height) * 1.04)

g_dendro <- ggplot(seg) +
  geom_segment(aes(x = alt, xend = alt_fin, y = pos, yend = pos_fin),
               color = colores$texto, linewidth = 0.45) +
  # El eje está invertido: hjust = 1 hace que el texto crezca hacia la izquierda.
  geom_text(data = rotulos_grupo, aes(x = x, y = centro, label = texto),
            hjust = 1, size = 3.1, lineheight = 0.95, family = fuente_cuerpo,
            color = colores$texto, fontface = "bold") +   # no el naranja: ese es "Grandes y caros"
  # Margen izquierdo amplio para los rótulos; las marcas solo en el rango del árbol.
  scale_x_reverse(labels = \(x) coma_ar(x, 1),
                  breaks = Filter(\(b) b <= max(h$height), pretty(c(0, max(h$height)))),
                  # Con el eje invertido, el primer valor de expand es el del
                  # lado izquierdo de la imagen.
                  expand = expansion(mult = c(0.7, 0.02))) +
  scale_y_continuous(limits = lim_y, expand = expansion(0)) +
  labs(x = "Altura de unión\n(distancia euclídea, Ward)", y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = colores$grilla, linewidth = 0.25))

# Composición de cada barrio, en el mismo orden. A la izquierda de las barras,
# una columna con la mitad de la ciudad (N o S, con letra y tono, para que se
# lea también en escala de grises).
comp_largo <- barrio_grupo |>
  left_join(select(orden, barrio_nom, pos), by = "barrio_nom") |>
  mutate(tipo = factor(nombre_tipo[as.character(grupo)], levels = rev(nombre_tipo)))

colores_tipo <- setNames(c(colores$acento2, colores$apagado, colores$acento), nombre_tipo)
# Separación entre los dos grupos de barrios, en el orden del dendrograma.
limite_grupos <- orden$pos[which(diff(orden$grupo_b) != 0)] + 0.5

g_barras <- ggplot(comp_largo, aes(x = pct, y = pos, fill = tipo)) +
  geom_col(orientation = "y", width = 0.78) +
  geom_tile(data = orden, aes(x = -7, y = pos), inherit.aes = FALSE,
            width = 9, height = 0.78,
            fill = if_else(orden$mitad == "Norte", colores$texto, colores$grilla)) +
  geom_text(data = orden, aes(x = -7, y = pos, label = letra), inherit.aes = FALSE,
            size = 2.7, family = fuente_cuerpo, fontface = "bold",
            color = if_else(orden$mitad == "Norte", "white", colores$texto)) +
  geom_hline(yintercept = limite_grupos, color = colores$texto, linewidth = 0.5,
             linetype = "22") +
  # unname(): con un vector con nombres, ggplot usa los nombres ("1", "2",
  # "3") como etiquetas de la leyenda.
  scale_fill_manual(values = colores_tipo, breaks = unname(nombre_tipo),
                    name = "Tipo de departamento (kmeans)",
                    guide = guide_legend(title.position = "top")) +
  # La columna N/S no lleva marca en el eje (se pegaba con el 0%): la explica
  # la bajada.
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%"),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(breaks = orden$pos, labels = orden$etiqueta, limits = lim_y,
                     expand = expansion(0)) +
  labs(x = "Avisos del barrio en cada tipo de departamento (%)", y = "Barrio (comuna)") +
  theme(axis.text.y = element_text(size = base_letra * 0.72, color = colores$texto),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(color = colores$secundario, size = base_letra * 0.85))

titulo_expl <- paste0("Agrupados por el tipo de departamento que publican, ",
                      fila_viejo$sur, " de los ", fila_viejo$barrios,
                      " barrios con más stock viejo quedan en la mitad sur de la ciudad")
bajada_expl <- paste0(
  "A la derecha, qué parte de los avisos de cada barrio cae en cada uno de los tres tipos de ",
  "departamento que encontró el kmeans. A la izquierda, el agrupamiento jerárquico de los ",
  nrow(M), " barrios según esa composición, cortado en dos grupos (línea punteada). N o S: la ",
  "mitad de la ciudad donde están la mayoría ",
  "de los avisos del barrio. ", num_ar(sum(barrio_grupo$n)), " avisos, noviembre de 2024. ",
  "La asociación con la geografía es parcial: índice de Rand ajustado de ", coma_ar(ari_mitad, 2),
  " contra la mitad norte-sur.")
fuente_expl <- paste0(
  fuente_jpg, " Método: kmeans (k = 3, estandarizado) sobre log del precio por m², log de la ",
  "superficie y tramo de antigüedad, sin variables de ubicación. Jerárquico sobre la proporción ",
  "de cada barrio en cada tipo, con distancia euclídea y enlace de Ward, cortado en 2 grupos. ",
  "Con distancia de Hellinger (la habitual para proporciones) y el mismo enlace, el índice de ",
  "Rand ajustado contra la mitad norte-sur es ", coma_ar(ari_hellinger, 2), "; con enlace ",
  "promedio o completo no se forman grupos (", tam_promedio, "). ",
  "Barrios con al menos 100 avisos.")

exploratorio <- (g_dendro + g_barras + plot_layout(widths = c(1.3, 2.3))) +
  plot_annotation(title = titulo(titulo_expl, 80), subtitle = bajada(bajada_expl, 125),
                  caption = pie(fuente_expl, 150),
                  theme = theme(plot.background = element_rect(fill = colores$fondo, color = NA)))

guardar_jpg(exploratorio, "grupo_08_exploratorio.jpg", alto_cm = 27)

# Versión para la diapositiva de la presentación final: el nombre descriptivo
# de la figura (auxiliar/nombres_figuras.csv) en lugar del título afirmativo,
# sin bajada (la pone la diapositiva) y con la misma nota de fuente. El JPG de
# la entrega de visualizaciones, el de arriba, no cambia.
exploratorio_diapo <- (g_dendro + g_barras + plot_layout(widths = c(1.3, 2.3))) +
  # Cortado a 50 caracteres: a 80 quedaba una sola palabra en la segunda línea.
  plot_annotation(title = titulo(nombre_figura("grupo_08_exploratorio_diapositiva.jpg"), 50),
                  caption = pie(fuente_expl, 150),
                  theme = theme(plot.background = element_rect(fill = colores$fondo, color = NA)))
guardar_jpg(exploratorio_diapo, "grupo_08_exploratorio_diapositiva.jpg", alto_cm = 27)


# =============================================================================
# 2. COMUNICATIVO: A IGUAL DEPARTAMENTO, CUÁNTO MUEVE EL BARRIO EL PRECIO
# =============================================================================
# Los 38.895 avisos de la regresión, log del precio contra log de la
# superficie. Se destacan en color los dos barrios de los extremos de la prima
# del paso 3; los otros 40 van en gris.
#
# Devolución de la instancia 3 (8/10): la categoría base pasa a ser Villa
# Lugano, el extremo bajo, en lugar de Balvanera. Así la etiqueta de Puerto
# Madero lee directo la diferencia que muestra el gráfico, sin pasar por un
# tercer barrio. Es una reparametrización del mismo modelo: mismo ajuste,
# mismos residuos, mismo R² y mismos precios predichos (se verifica abajo);
# solo cambian los coeficientes de barrio. El script 08 no se toca: el
# modelo se vuelve a estimar acá con la base cambiada. Además: los nombres de
# los dos barrios van coloreados en el título (element_markdown de ggtext,
# como en la clase de OWID), la bajada ya no explica los colores y la nota al
# pie deja solo la fuente, qué son las rectas y por qué esos dos barrios.
#
# Por qué el paso 3 y no el modelo completo: el paso 3 controla la vivienda
# (superficie, ambientes, baños, antigüedad). El completo agrega la distancia
# al Obelisco, que también es ubicación, y le resta al barrio parte de lo que
# este gráfico quiere mostrar. Contesta además la pregunta de la devolución
# ("¿con qué modelo se quedan?"): con el de barrio.

cat("\n#### 2. COMUNICATIVO ####\n\n")

# Se vuelve a correr el script 08 para tener la muestra y el modelo, sin
# duplicar su definición.
e08 <- new.env()
sys.source("script/08_regresion.R", envir = e08)
theme_set(tema_tp(base_size = base_letra))   # el 08 vuelve a fijar el tema de siempre

m3  <- e08$m3                       # paso 3 del 08, con base Balvanera
primas <- e08$por_barrio            # prima del paso 3 por barrio, con Balvanera = 0
barrio_alto <- primas$barrio_nom[which.max(primas$prima)]
barrio_bajo <- primas$barrio_nom[which.min(primas$prima)]
referencia  <- barrio_bajo          # la nueva categoría base: el extremo bajo
stopifnot(referencia == "Villa Lugano")

# El mismo modelo con Villa Lugano como base.
d <- e08$d |> mutate(barrio = relevel(barrio, ref = referencia))
m3_ref <- lm(formula(m3), data = d)
stopifnot(
  isTRUE(all.equal(unname(fitted(m3_ref)), unname(fitted(m3)), tolerance = 1e-10)),
  isTRUE(all.equal(unname(resid(m3_ref)),  unname(resid(m3)),  tolerance = 1e-10)),
  abs(summary(m3_ref)$r.squared - summary(m3)$r.squared) < 1e-12
)

brecha <- exp(max(primas$prima) - min(primas$prima))
stopifnot(abs(brecha - as.numeric(read_csv("output/tablas/08_numeros.csv", show_col_types = FALSE) |>
                                     filter(nombre == "brecha_p3") |> pull(valor))) < 1e-9)
# Con la base nueva, el coeficiente de Puerto Madero es directamente la brecha.
coef_alto <- coef(m3_ref)[[paste0("barrio", barrio_alto)]]
stopifnot(abs(exp(coef_alto) - brecha) < 1e-9)
prima_alto_pct <- 100 * (exp(coef_alto) - 1)
cat(barrio_alto, "respecto de", referencia, ":", coma_ar(prima_alto_pct, 1), "%\n")

# Las rectas: el precio que predice el paso 3 para un mismo departamento en
# cada barrio, con ambientes, baños y antigüedad en sus valores medianos. Son
# paralelas (la elasticidad es la misma), así que la distancia vertical entre
# ellas es la misma en cualquier superficie: es la brecha.
amb_med   <- median(d$ambientes)
banos_med <- median(d$banos)
antig_med <- levels(d$antiguedad)[median(as.integer(d$antiguedad))]
rango_m2  <- quantile(d$metros_cubiertos, c(0.01, 0.99))
rectas <- expand_grid(barrio_nom = c(barrio_alto, barrio_bajo),
                      metros_cubiertos = exp(seq(log(rango_m2[1]), log(rango_m2[2]), length.out = 50))) |>
  mutate(lm2 = log(metros_cubiertos), ambientes = amb_med, banos = banos_med,
         antiguedad = factor(antig_med, levels = levels(d$antiguedad)),
         barrio = factor(barrio_nom, levels = levels(d$barrio)),
         precio = exp(predict(m3_ref, newdata = pick(everything()))))
# Los precios de las rectas son los mismos con cualquiera de las dos bases.
precio_base_balvanera <- exp(predict(m3, newdata = mutate(rectas, barrio = factor(barrio_nom,
                                                                 levels = levels(e08$d$barrio)))))
stopifnot(isTRUE(all.equal(unname(rectas$precio), unname(precio_base_balvanera), tolerance = 1e-10)))

# La anotación: a la superficie mediana de la base.
m2_ref <- median(d$metros_cubiertos)
precio_en <- function(b) {
  nd <- tibble(lm2 = log(m2_ref), ambientes = amb_med, banos = banos_med,
               antiguedad = factor(antig_med, levels = levels(d$antiguedad)),
               barrio = factor(b, levels = levels(d$barrio)))
  exp(predict(m3_ref, newdata = nd))
}
p_alto <- precio_en(barrio_alto); p_bajo <- precio_en(barrio_bajo)
stopifnot(abs(p_alto / p_bajo - brecha) < 1e-9)
cat("Brecha", barrio_alto, "/", barrio_bajo, ":", coma_ar(brecha, 2), "\n")
cat("A", m2_ref, "m²:", usd_ar(p_alto), "contra", usd_ar(p_bajo), "\n")

col_barrio <- setNames(c(colores$acento, colores$acento2), c(barrio_alto, barrio_bajo))
puntos_fondo <- filter(d, !barrio_nom %in% c(barrio_alto, barrio_bajo))
puntos_dest  <- filter(d, barrio_nom %in% c(barrio_alto, barrio_bajo))
stopifnot(n_distinct(puntos_fondo$barrio_nom) == 40)

# Etiqueta directa al final de cada recta (en lugar de leyenda): así el
# terracota y el verde azulado, que en escala de grises casi no se distinguen,
# no dependen solo del color. Los puntos además cambian de forma.
etiq_rectas <- rectas |> group_by(barrio_nom) |> slice_max(metros_cubiertos, n = 1) |> ungroup()
etiq_rectas$texto <- if_else(
  etiq_rectas$barrio_nom == referencia,
  paste0(referencia, "\n(referencia)"),
  paste0(barrio_alto, "\n", if_else(prima_alto_pct > 0, "+", "-"), coma_ar(abs(prima_alto_pct), 0),
         "% respecto de\n", referencia))

# Título con los dos barrios pintados del color de su serie (técnica de la
# clase de OWID, clase24_graficos_owid.R). pintar() es la de esa clase y
# está en utils/pintar.R.
titulo_com <- paste0("Por el mismo departamento, en ", barrio_alto, " se pide ",
                     coma_ar(brecha, 1), " veces lo que en ", barrio_bajo)
# El título con formato lleva el corte de línea a mano (<br>), como en la
# clase de OWID: cada línea es una unidad de sentido. La primera fija la
# comparación ("por el mismo departamento") y la segunda tiene el hallazgo
# entero, con los dos barrios en la misma línea.
titulo_com_md <- paste0("Por el mismo departamento,<br>en ",
                        pintar(barrio_alto, col_barrio[[barrio_alto]]), " se pide ",
                        coma_ar(brecha, 1), " veces lo que en ",
                        pintar(barrio_bajo, col_barrio[[barrio_bajo]]))
stopifnot(str_remove_all(titulo_com_md, "<[^>]+>") ==
            str_replace(titulo_com, "departamento, ", "departamento,"))

bajada_com <- paste0(
  "Precio publicado contra superficie cubierta de ", num_ar(nrow(d)), " avisos de departamentos ",
  "en venta en ", n_distinct(d$barrio_nom), " barrios de CABA, noviembre de 2024. Los dos ejes ",
  "en escala logarítmica. Las rectas comparan un mismo departamento en los dos barrios: ",
  "la distancia vertical entre ellas es la diferencia de precio asociada al barrio.")
fuente_com <- paste0(
  fuente_jpg, " Las rectas son el precio estimado para un mismo departamento en cada barrio: ",
  amb_med, " ambientes, ", banos_med, " baño y antigüedad «", str_to_lower(antig_med),
  "» (las medianas de la base); la flecha está en ", num_ar(m2_ref),
  " m², la superficie mediana. ", barrio_alto, " y ", barrio_bajo, " son los extremos de la ",
  "prima estimada por barrio: los de precio más alto y más bajo a igual superficie, ambientes, ",
  "baños y antigüedad, entre los ", n_distinct(d$barrio_nom), " barrios. ",
  "Barrios con al menos 100 avisos.")   # el universo: explica por qué son 42 y no 48

comunicativo <- ggplot() +
  geom_point(data = puntos_fondo, aes(metros_cubiertos, precio_usd),
             color = colores$apagado_cl, size = 0.35, alpha = 0.35, shape = 16) +
  geom_point(data = puntos_dest, aes(metros_cubiertos, precio_usd, color = barrio_nom,
                                     shape = barrio_nom),
             size = 1.1, alpha = 0.7) +
  geom_line(data = rectas, aes(metros_cubiertos, precio, color = barrio_nom), linewidth = 1) +
  geom_text(data = etiq_rectas, aes(metros_cubiertos * 1.06, precio, label = texto,
                                    color = barrio_nom),
            hjust = 0, size = 3.3, lineheight = 0.9, family = fuente_cuerpo, fontface = "bold") +
  # La brecha, a la superficie mediana: flecha doble entre las dos rectas.
  annotate("segment", x = m2_ref, xend = m2_ref, y = p_bajo, yend = p_alto,
           color = colores$texto, linewidth = 0.6,
           arrow = arrow(ends = "both", length = unit(0.18, "cm"), type = "closed")) +
  # El texto de la brecha va arriba a la izquierda, donde no hay avisos, unido
  # a la flecha por una línea fina: al lado de la flecha tapaba la recta de
  # Puerto Madero.
  annotate("segment", x = m2_ref * 0.98, xend = 30, y = p_alto * 1.05, yend = p_alto * 2.3,
           color = colores$texto, linewidth = 0.3) +
  annotate("text", x = 18, y = p_alto * 2.4,
           label = paste0("×", coma_ar(brecha, 1), " por el mismo departamento\n",
                          "de ", num_ar(m2_ref), " m²: ", usd_ar(p_alto), " en ", barrio_alto,
                          ",\n", usd_ar(p_bajo), " en ", barrio_bajo),
           hjust = 0, vjust = 0, size = 3.4, lineheight = 0.95, family = fuente_cuerpo,
           color = colores$texto, fontface = "bold") +
  scale_color_manual(values = col_barrio, guide = "none") +
  scale_shape_manual(values = setNames(c(16, 17), c(barrio_alto, barrio_bajo)), guide = "none") +
  scale_x_log10(labels = num_ar, breaks = c(20, 30, 50, 100, 200, 400),
                expand = expansion(mult = c(0.02, 0.28))) +
  scale_y_log10(labels = usd_ar, breaks = c(25e3, 50e3, 1e5, 2e5, 5e5, 1e6, 3e6)) +
  labs(
    title = titulo_com_md,
    subtitle = bajada(bajada_com, 120),
    caption = pie(fuente_com, 150),
    x = "Superficie cubierta (m², escala logarítmica)",
    y = "Precio publicado (US$, escala logarítmica)"
  ) +
  # Solo en este gráfico: el título como element_markdown, con el mismo
  # formato que el de tema_tp() (00_tema.R no se toca).
  theme(plot.title = ggtext::element_markdown(family = fuente_titulo, face = "bold",
                                              size = base_letra * 1.25, color = colores$texto,
                                              hjust = 0, lineheight = 1.1, margin = margin(b = 4)))

guardar_jpg(comunicativo, "grupo_08_comunicativo.jpg", alto_cm = 17)

# Versión para la diapositiva: el nombre descriptivo en lugar del título, con
# los dos barrios pintados del color de su serie (es la única clave de colores
# del gráfico, porque la bajada no los explica), sin bajada y con la fuente.
nombre_com_md <- nombre_figura("grupo_08_comunicativo_diapositiva.jpg")
stopifnot(str_detect(nombre_com_md, fixed(barrio_alto)), str_detect(nombre_com_md, fixed(barrio_bajo)))
nombre_com_md <- nombre_com_md |>
  str_replace(fixed(barrio_alto), pintar(barrio_alto, col_barrio[[barrio_alto]])) |>
  str_replace(fixed(barrio_bajo), pintar(barrio_bajo, col_barrio[[barrio_bajo]]))
comunicativo_diapo <- comunicativo + labs(title = nombre_com_md, subtitle = NULL)
guardar_jpg(comunicativo_diapo, "grupo_08_comunicativo_diapositiva.jpg", alto_cm = 17)


# =============================================================================
# 3. NÚMEROS
# =============================================================================

# Los textos exactos de cada imagen, para revisarlos sin abrir el JPG.
writeLines(c("EXPLORATORIO", "", "Título:", titulo_expl, "", "Bajada:", bajada_expl, "",
             "Nota al pie:", fuente_expl, "", "", "COMUNICATIVO", "", "Título:", titulo_com, "",
             "Bajada:", bajada_com, "", "Nota al pie:", fuente_com),
           "output/tablas/11_textos_graficos.txt", useBytes = FALSE)

guardar_numeros(
  "11_numeros.csv",
  ari_mitad = ari_mitad, ari_comuna = ari_comuna, p_perm = p_perm,
  ari_hellinger = ari_hellinger, tam_promedio = tam_promedio,
  grupo_viejo_barrios = fila_viejo$barrios, grupo_viejo_sur = fila_viejo$sur,
  grupo_otro_barrios = fila_otro$barrios, grupo_otro_norte = fila_otro$norte,
  barrio_alto = barrio_alto, barrio_bajo = barrio_bajo, brecha = brecha,
  m2_ref = m2_ref, p_alto = p_alto, p_bajo = p_bajo,
  referencia_com = referencia, prima_alto_pct = prima_alto_pct
)
