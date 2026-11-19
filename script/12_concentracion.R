# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Entrega final: herramienta de análisis económico
# Script 12: concentración de la oferta publicada por barrio
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: input/base_analisis.csv y la muestra y el modelo del paso 3 del
#          script 08, que se vuelve a correr acá (no se duplica la definición
#          de la muestra).
# Salida : output/tablas/12_concentracion.txt, 12_numeros.csv,
#          12_concentracion_barrios.csv, 12_precision_barrio.csv,
#          output/graficos/g19_lorenz_barrios.png
#
# La consigna de la entrega final pide al menos una herramienta de análisis
# económico (índice compuesto, indexaciones, concentración o ventajas
# comparativas), aparte de los métodos estadísticos. Elegimos un índice de
# concentración de los avisos entre barrios porque mide algo que el trabajo
# venía afirmando sin cuantificar: los modelos se restringen a 42 barrios
# porque seis tienen menos de 100 avisos, y esa restricción estaba
# justificada solo de forma cualitativa.
#
# Qué mide y qué no. Concentración de AVISOS PUBLICADOS, no del stock de
# viviendas ni de las operaciones. Un barrio puede tener muchos avisos porque
# tiene muchas viviendas, porque tiene más rotación o porque sus vendedores
# usan más este portal, y con esta base no se pueden separar las tres cosas.
# Además, los barrios son unidades administrativas de superficies muy
# distintas, y la base no tiene la superficie de cada uno, así que no podemos
# expresar los avisos por kilómetro cuadrado. Parte de la concentración que
# mide el índice sale de cómo están trazados los barrios y no del mercado.
#
# Tres medidas, sobre los 39.305 avisos de la base de análisis en 48 barrios:
#   - HHI (Herfindahl-Hirschman) y su inversa, el número equivalente de
#     barrios;
#   - curva de Lorenz y Gini;
#   - los descriptivos para leerlos: participación de los cinco barrios más
#     grandes, cuántos barrios hacen falta para llegar al 50%, y los seis
#     barrios con menos de 100 avisos.
# Y la comparación con los 42 barrios de la muestra de los modelos.
# =============================================================================

library(tidyverse)
library(lmtest)
library(sandwich)
source("script/00_tema.R")

# El 08 se corre primero (tiene su propio sink): da la muestra de los modelos
# (d), los seis barrios chicos (barrios_chicos) y el modelo del paso 3 (m3).
e08 <- new.env()
invisible(capture.output(sys.source("script/08_regresion.R", envir = e08)))
theme_set(tema_tp())   # el 08 vuelve a fijar el tema; se deja el de siempre

base <- read_csv("input/base_analisis.csv", show_col_types = FALSE)

sink("output/tablas/12_concentracion.txt", split = TRUE)
options(width = 130)


# =============================================================================
# 1. LAS PARTICIPACIONES
# =============================================================================

cat("#### 1. LAS PARTICIPACIONES ####\n\n")

por_barrio <- base |>
  count(barrio_nom, name = "avisos") |>
  arrange(desc(avisos)) |>
  mutate(participacion = avisos / sum(avisos),
         acumulada     = cumsum(participacion),
         puesto        = row_number())

n_avisos  <- sum(por_barrio$avisos)
n_barrios <- nrow(por_barrio)
stopifnot(n_avisos == nrow(base), n_barrios == 48)

cat("Avisos:", num_ar(n_avisos), "  Barrios:", n_barrios, "\n\n")
print(por_barrio |> mutate(across(c(participacion, acumulada), \(x) round(100 * x, 2))),
      n = Inf)


# =============================================================================
# 2. HHI Y NÚMERO EQUIVALENTE DE BARRIOS
# =============================================================================
# HHI = suma de las participaciones al cuadrado. Con 48 barrios va de 1/48
# (todos con la misma cantidad de avisos) a 1 (todos en uno). 1/HHI es el
# número de barrios del mismo tamaño que darían la misma concentración.
# Como el mínimo depende de cuántos barrios hay, para comparar 48 contra 42 se
# usa también el HHI normalizado: (HHI − 1/N) / (1 − 1/N), de 0 a 1.
# hhi(), hhi_normalizado(), curva_lorenz() y gini() están en utils/.

cat("\n\n#### 2. HHI ####\n\n")

hhi_norm <- hhi_normalizado

hhi_48   <- hhi(por_barrio$avisos)
equiv_48 <- 1 / hhi_48
hhin_48  <- hhi_norm(por_barrio$avisos)

cat("HHI (0 a 1)          :", round(hhi_48, 4), "  en escala 0 a 10.000:", num_ar(10000 * hhi_48), "\n")
cat("Mínimo posible (1/48):", round(1 / n_barrios, 4), "\n")
cat("Número equivalente   :", coma_ar(equiv_48, 1), "barrios del mismo tamaño, de", n_barrios, "\n")
cat("HHI normalizado      :", round(hhin_48, 4), "\n")


# =============================================================================
# 3. CURVA DE LORENZ Y GINI
# =============================================================================

cat("\n\n#### 3. LORENZ Y GINI ####\n\n")

lorenz_48 <- curva_lorenz(por_barrio$avisos)
gini_48   <- gini(por_barrio$avisos)
# Control: con unidades del mismo peso, el Gini por trapecios es la diferencia
# media de Gini.
x <- por_barrio$avisos
stopifnot(abs(gini_48 - sum(abs(outer(x, x, "-"))) / (2 * length(x)^2 * mean(x))) < 1e-12)

cat("Gini:", round(gini_48, 4), " (máximo posible con 48 barrios:", round(47 / 48, 4), ")\n")


# =============================================================================
# 4. LOS DESCRIPTIVOS PARA LEERLOS
# =============================================================================

cat("\n\n#### 4. DESCRIPTIVOS ####\n\n")

top5      <- head(por_barrio, 5)
cr5       <- sum(top5$participacion)
k50       <- which(por_barrio$acumulada >= 0.5)[1]
part_k50  <- por_barrio$acumulada[k50]
# Los seis barrios chicos son los mismos que saca el 08.
chicos <- por_barrio |> filter(barrio_nom %in% e08$barrios_chicos$barrio_nom) |>
  arrange(avisos) |>
  left_join(base |> group_by(barrio_nom) |>
              summarise(comuna = first(comuna),
                        mitad = if_else(mean(mitad_ciudad == "Norte") > 0.5, "Norte", "Sur"),
                        .groups = "drop"),
            by = "barrio_nom")
stopifnot(nrow(chicos) == 6, all(chicos$avisos < 100))
part_chicos <- sum(chicos$participacion)

cat("Los cinco barrios más grandes:", paste(top5$barrio_nom, collapse = ", "), "\n")
cat("  concentran el", coma_ar(100 * cr5, 1), "% de los avisos\n")
cat("  (en partes iguales, cinco barrios tendrían el", coma_ar(100 * 5 / n_barrios, 1), "%)\n")
cat("Barrios necesarios para llegar al 50%:", k50, "de", n_barrios,
    " (llegan al", coma_ar(100 * part_k50, 1), "%)\n")
cat("Palermo solo:", coma_ar(100 * por_barrio$participacion[1], 1), "%\n\n")
cat("Los seis barrios con menos de 100 avisos:\n")
print(chicos |> transmute(barrio_nom, avisos, pct = round(100 * participacion, 2), comuna, mitad))
cat("Juntos:", num_ar(sum(chicos$avisos)), "avisos,", coma_ar(100 * part_chicos, 2),
    "% del total (en partes iguales serían el", coma_ar(100 * 6 / n_barrios, 1), "%)\n")
cat("El más chico de los seis tiene", num_ar(min(chicos$avisos)), "avisos; Palermo,",
    num_ar(por_barrio$avisos[1]), ": ", coma_ar(por_barrio$avisos[1] / min(chicos$avisos), 0),
    "veces más\n")


# =============================================================================
# 5. DE 48 A 42 BARRIOS: LA MUESTRA DE LOS MODELOS
# =============================================================================
# Sobre la muestra del 08 (sin los 40 avisos que no son departamentos y sin
# los seis barrios chicos).

cat("\n\n#### 5. DE 48 A 42 BARRIOS ####\n\n")

avisos_42 <- e08$d |> count(barrio_nom, name = "avisos") |> pull(avisos)
stopifnot(length(avisos_42) == 42)
hhi_42   <- hhi(avisos_42)
equiv_42 <- 1 / hhi_42
hhin_42  <- hhi_norm(avisos_42)
gini_42  <- gini(avisos_42)
cr5_42   <- sum(head(sort(avisos_42, decreasing = TRUE), 5)) / sum(avisos_42)

comparacion <- tibble(
  medida = c("Avisos", "Barrios", "HHI (0 a 10.000)", "Número equivalente",
             "HHI normalizado", "Gini", "Cinco más grandes (%)"),
  `48 barrios` = c(n_avisos, n_barrios, 10000 * hhi_48, equiv_48, hhin_48, gini_48, 100 * cr5),
  `42 barrios` = c(sum(avisos_42), 42, 10000 * hhi_42, equiv_42, hhin_42, gini_42, 100 * cr5_42)
)
print(comparacion |> mutate(across(where(is.double), \(x) round(x, 4))))
cat("\nLa muestra de los modelos se queda con el", coma_ar(100 * sum(avisos_42) / n_avisos, 1),
    "% de los avisos y el", coma_ar(100 * 42 / 48, 1), "% de los barrios.\n")


# =============================================================================
# 6. CONSECUENCIA ESTADÍSTICA: AVISOS POR BARRIO Y PRECISIÓN DEL COEFICIENTE
# =============================================================================
# En el paso 3 del 08, el error estándar HC1 del coeficiente de cada barrio
# (respecto de Balvanera) contra la cantidad de avisos del barrio. Si la
# precisión depende sobre todo del tamaño, el error cae como 1/raíz(n): una
# pendiente cercana a −0,5 en logaritmos. Los seis barrios chicos no están en
# el modelo, así que su error estándar no se observa.

cat("\n\n#### 6. AVISOS Y PRECISIÓN ####\n\n")

ct <- coeftest(e08$m3, vcov = vcovHC(e08$m3, type = "HC1"))
precision <- tibble(termino = rownames(ct), ee = ct[, "Std. Error"]) |>
  filter(str_starts(termino, "barrio")) |>
  mutate(barrio_nom = str_remove(termino, "^barrio")) |>
  left_join(e08$d |> count(barrio_nom, name = "avisos"), by = "barrio_nom") |>
  arrange(avisos) |>
  select(barrio_nom, avisos, ee)
stopifnot(nrow(precision) == 41, !anyNA(precision$avisos))

ajuste_ee   <- lm(log(ee) ~ log(avisos), data = precision)
pend_ee     <- unname(coef(ajuste_ee)[2])
r2_ee       <- summary(ajuste_ee)$r.squared
cor_ee      <- cor(log(precision$avisos), log(precision$ee))
cor_ee_sp   <- cor(precision$avisos, precision$ee, method = "spearman")
ee_chico    <- slice_min(precision, avisos, n = 1)
ee_grande   <- slice_max(precision, avisos, n = 1)

print(precision |> mutate(ee = round(ee, 4)), n = Inf)
cat("\nlog(EE) ~ log(avisos): pendiente", round(pend_ee, 3), " R²", round(r2_ee, 3),
    " correlación", round(cor_ee, 3), " Spearman", round(cor_ee_sp, 3), "\n")
cat("Barrio con menos avisos del modelo:", ee_chico$barrio_nom, "(", ee_chico$avisos, "avisos ), EE",
    round(ee_chico$ee, 4), "\n")
cat("Barrio con más avisos:", ee_grande$barrio_nom, "(", ee_grande$avisos, "avisos ), EE",
    round(ee_grande$ee, 4), "  razón:", coma_ar(ee_chico$ee / ee_grande$ee, 1), "\n")

sink()


# =============================================================================
# 7. GRÁFICO: LA CURVA DE LORENZ
# =============================================================================
# Sin título ni bajada, como el resto de los gráficos que van a diapositiva.
# Se marcan los tres puntos que cita el texto. Con los barrios ordenados de
# menos a más avisos:
#   - los seis chicos son el primer tramo: x = 6/48, y = su participación;
#   - los cinco más grandes son el último: arrancan en x = 43/48;
#   - los k50 barrios que llegan al 50% arrancan en x = (48 − k50)/48.

pt_chicos <- tibble(x = 6 / n_barrios, y = part_chicos,
                    texto = paste0("Los 6 barrios con menos\nde 100 avisos: el ",
                                   coma_ar(100 * part_chicos, 1), "%"))
pt_k50 <- tibble(x = (n_barrios - k50) / n_barrios, y = 1 - part_k50,
                 texto = paste0("Los ", k50, " barrios con más avisos\nsuman el ",
                                coma_ar(100 * part_k50, 0), "%"))
pt_top5 <- tibble(x = (n_barrios - 5) / n_barrios, y = 1 - cr5,
                  texto = paste0("Los 5 más grandes, el ", coma_ar(100 * cr5, 0), "%:\n",
                                 paste(top5$barrio_nom[1:3], collapse = ", "), ",\n",
                                 paste(top5$barrio_nom[4:5], collapse = " y ")))
puntos_citados <- bind_rows(pt_chicos, pt_k50, pt_top5)

pct_eje <- \(x) paste0(num_ar(100 * x), "%")

g19 <- ggplot(lorenz_48, aes(prop_unidades, prop_acumulada)) +
  # La diagonal: los 48 barrios con la misma cantidad de avisos.
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1,
           color = colores$apagado, linewidth = 0.5, linetype = "22") +
  # Los textos van en el triángulo de arriba a la izquierda, que está vacío,
  # sin cruzar la diagonal; una línea fina los une a su punto.
  annotate("text", x = 0.27, y = 0.37, hjust = 1, size = 3.1, lineheight = 0.95,
           family = fuente_cuerpo, color = colores$secundario,
           label = "Si los 48 barrios tuvieran\nla misma cantidad de avisos") +
  geom_area(fill = colores$acento, alpha = 0.10) +
  geom_line(color = colores$acento, linewidth = 0.9) +
  geom_point(data = lorenz_48[-1, ], color = colores$acento, size = 1.1) +
  # Los tres puntos citados.
  geom_point(data = puntos_citados, aes(x, y), color = colores$texto, size = 2.6) +
  annotate("text", x = 0.21, y = 0.165, hjust = 0, size = 3.1, lineheight = 0.95,
           family = fuente_cuerpo, color = colores$texto, label = pt_chicos$texto) +
  annotate("segment", x = 0.205, y = 0.14, xend = pt_chicos$x + 0.004, yend = pt_chicos$y + 0.012,
           color = colores$texto, linewidth = 0.3) +
  annotate("text", x = 0.62, y = 0.71, hjust = 1, size = 3.1, lineheight = 0.95,
           family = fuente_cuerpo, color = colores$texto, label = pt_k50$texto) +
  annotate("segment", x = 0.625, y = 0.69, xend = pt_k50$x - 0.006, yend = pt_k50$y + 0.008,
           color = colores$texto, linewidth = 0.3) +
  annotate("text", x = 0.62, y = 0.90, hjust = 1, size = 3.1, lineheight = 0.95,
           family = fuente_cuerpo, color = colores$texto, label = pt_top5$texto) +
  annotate("segment", x = 0.625, y = 0.88, xend = pt_top5$x - 0.006, yend = pt_top5$y + 0.008,
           color = colores$texto, linewidth = 0.3) +
  scale_x_continuous(labels = pct_eje, breaks = seq(0, 1, 0.25), expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = pct_eje, breaks = seq(0, 1, 0.25), expand = expansion(mult = 0.01)) +
  coord_fixed() +
  labs(x = "Barrios, acumulados de menos a más avisos",
       y = "Avisos acumulados",
       caption = pie(paste0(
         fuente_pie, ". Curva de Lorenz de los ", num_ar(n_avisos), " avisos de la base de ",
         "análisis entre los ", n_barrios, " barrios; cada punto es un barrio. Gini = ",
         coma_ar(gini_48, 2), ". Índice de Herfindahl-Hirschman = ", num_ar(10000 * hhi_48),
         " (sobre 10.000): la misma concentración que ", coma_ar(equiv_48, 1),
         " barrios del mismo tamaño. Mide la concentración de los avisos publicados, ",
         "no la del stock de viviendas.")))

guardar(g19, "g19_lorenz_barrios.png", ancho = 9, alto = 7)


# =============================================================================
# 8. TABLAS Y NÚMEROS
# =============================================================================

write_csv(por_barrio, "output/tablas/12_concentracion_barrios.csv")
write_csv(precision, "output/tablas/12_precision_barrio.csv")

guardar_numeros(
  "12_numeros.csv",
  n_avisos = n_avisos, n_barrios = n_barrios,
  hhi = hhi_48, hhi_10000 = 10000 * hhi_48, hhi_min = 1 / n_barrios,
  n_equivalente = equiv_48, hhi_normalizado = hhin_48,
  gini = gini_48, gini_max = (n_barrios - 1) / n_barrios,
  cr5 = cr5, top5 = paste(top5$barrio_nom, collapse = ", "),
  cr5_iguales = 5 / n_barrios,
  palermo = por_barrio$participacion[1],
  k50 = k50, part_k50 = part_k50,
  n_chicos = nrow(chicos), avisos_chicos = sum(chicos$avisos), part_chicos = part_chicos,
  chicos = paste(chicos$barrio_nom, collapse = ", "),
  min_chicos = min(chicos$avisos),
  n_avisos_42 = sum(avisos_42), hhi_10000_42 = 10000 * hhi_42, n_equivalente_42 = equiv_42,
  hhi_normalizado_42 = hhin_42, gini_42 = gini_42, cr5_42 = cr5_42,
  ee_pendiente = pend_ee, ee_r2 = r2_ee, ee_cor = cor_ee, ee_spearman = cor_ee_sp,
  ee_barrio_chico = ee_chico$barrio_nom, ee_avisos_chico = ee_chico$avisos, ee_chico = ee_chico$ee,
  ee_barrio_grande = ee_grande$barrio_nom, ee_avisos_grande = ee_grande$avisos, ee_grande = ee_grande$ee
)

cat("\nDiagnóstico guardado en output/tablas/12_concentracion.txt\n")
