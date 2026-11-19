# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Entrega final
# Script 13: los números de la presentación final, verificados
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: output/tablas/0X_numeros.csv (03, 06, 07, 08, 09, 11 y 12), los CSV
#          de los scripts 02, 03, 04, 08 y 09, input/base_analisis.csv (para la
#          latitud de cada barrio) y README.md (para verificar sus números).
# Salida : output/tablas/13_numeros_diapositivas.csv  (todos los números de
#            cada diapositiva, con su fuente)
#          output/tablas/13_numeros.csv  (los que no guardaba ningún script)
#          output/graficos/g20_prima_latitud.png
#
# Este script es análisis, no texto. Junta en una sola tabla todos los
# números que usa cada diapositiva de la presentación final, ya formateados,
# con el archivo y la clave de donde sale cada uno, y verifica que sean
# coherentes entre sí y con lo que afirma el trabajo. La prosa de las
# diapositivas no está acá: la consigna limita el uso de IA en el armado de la
# presentación, así que los textos son un documento de trabajo del grupo, fuera
# del repositorio.
#
# Calcula además lo que ningún script guardaba: la correlación entre la prima
# de cada barrio y su latitud (diapositiva 13), con su gráfico.
#
# Hay que correr antes del 02 al 12.
# =============================================================================

library(tidyverse)
source("script/00_tema.R")


# =============================================================================
# 1. LAS FUENTES
# =============================================================================

N3  <- leer_numeros("03_numeros.csv")
N6  <- leer_numeros("06_numeros.csv")
N7  <- leer_numeros("07_numeros.csv")
N8  <- leer_numeros("08_numeros.csv")
N9  <- leer_numeros("09_numeros.csv")
N11 <- leer_numeros("11_numeros.csv")
N12 <- leer_numeros("12_numeros.csv")

g  <- function(N, v) as.numeric(N[[v]])
f  <- function(N, v, dec = 0) if (dec == 0) num_ar(g(N, v)) else coma_ar(g(N, v), dec)
pc <- function(N, v, dec = 1) coma_ar(100 * g(N, v), dec)
pts <- function(v, dec = 1) coma_ar(100 * g(N8, v), dec)
con_signo <- function(x, dec = 3) paste0(if_else(x > 0, "+", "-"), coma_ar(abs(x), dec))

como_texto <- cols(.default = col_character())
titulos_i1 <- read_csv("output/tablas/03_titulos_graficos.csv", show_col_types = FALSE)
ficha      <- read_csv("output/tablas/04_ficha_base.csv", col_types = como_texto)
var_viv    <- read_csv("output/tablas/04_variables_vivienda.csv", col_types = como_texto)
var_ubi    <- read_csv("output/tablas/04_variables_ubicacion.csv", col_types = como_texto)
correl     <- read_csv("output/tablas/04_correlaciones.csv", col_types = como_texto)
varianza   <- read_csv("output/tablas/04_varianza_barrios.csv", col_types = como_texto)
descr      <- read_csv("output/tablas/04_descriptivas.csv", col_types = como_texto)
recorte    <- read_csv("output/tablas/04_evidencia_recorte.csv", col_types = como_texto)
repetidos  <- read_csv("output/tablas/02_clasificacion_repetidos.csv", show_col_types = FALSE)
efecto     <- read_csv("output/tablas/02_efecto_deduplicacion.csv", show_col_types = FALSE)
prima_p3   <- read_csv("output/tablas/08_prima_superficie_barrio.csv", show_col_types = FALSE)
primas_p4  <- read_csv("output/tablas/08_primas_barrio.csv", show_col_types = FALSE)
perfil     <- read_csv("output/tablas/09_perfiles.csv", show_col_types = FALSE)
ari_km     <- read_csv("output/tablas/09_comparacion_ari.csv", show_col_types = FALSE)
dentro     <- read_csv("output/tablas/09_dentro_grupo.csv", show_col_types = FALSE)
ejemplo    <- read_csv("output/tablas/09_ejemplo_recoleta.csv", show_col_types = FALSE)

campo   <- function(x) ficha$valor[ficha$campo == x]
extremo <- function(var, col) recorte[[col]][recorte$Variable == var]
a  <- function(que, dec = 2) coma_ar(ari_km$ari[ari_km$comparacion == que], dec)
p  <- function(gr, col, dec = 1) coma_ar(perfil[[col]][perfil$grupo == gr], dec)
ej <- function(barrio, gr, col) ejemplo[[col]][ejemplo$barrio_nom == barrio & ejemplo$grupo == gr]


# =============================================================================
# 2. DIAPOSITIVA 13: LA PRIMA DE CADA BARRIO Y SU LATITUD
# =============================================================================
# Prima del modelo completo (08_primas_barrio.csv, respecto de la base de la
# regresión) contra la latitud mediana de los avisos de cada barrio, en la
# muestra de los modelos (sin los avisos que no son departamentos). Mediana y
# no media, por consistencia con el resto del trabajo; con la media la
# correlación da casi lo mismo (0,68 en vez de 0,67). La correlación no
# depende de qué barrio sea la base: cambiarla suma una constante a todas las
# primas.

base_reg <- prima_p3$barrio_nom[prima_p3$prima == 0]
base_com <- N11[["referencia_com"]]
stopifnot(length(base_reg) == 1, base_com == N11[["barrio_bajo"]])

avisos_dep <- read_csv("input/base_analisis.csv", show_col_types = FALSE) |> filter(!no_es_depto)
lat_barrio <- avisos_dep |>
  group_by(barrio_nom) |>
  summarise(lat = median(lat), pct_norte = mean(mitad_ciudad == "Norte"), .groups = "drop")
prima_lat <- primas_p4 |>
  transmute(barrio_nom = barrio, beta) |>
  bind_rows(tibble(barrio_nom = base_reg, beta = 0)) |>
  left_join(lat_barrio, by = "barrio_nom")
stopifnot(nrow(prima_lat) == g(N8, "n_barrios"), !anyNA(prima_lat$lat))

pm        <- N8[["prima_max_barrio"]]
r_lat     <- cor(prima_lat$beta, prima_lat$lat)
r_lat_sin <- with(filter(prima_lat, barrio_nom != pm), cor(beta, lat))
pm_norte  <- prima_lat$pct_norte[prima_lat$barrio_nom == pm]
# "Cerca de la mitad" es r²: la parte de la variación de las primas entre
# barrios que acompaña a la latitud. Si deja de estar cerca de la mitad, o si
# Puerto Madero deja de estar entero en la mitad sur, el script se frena.
stopifnot(r_lat^2 > 0.4, r_lat^2 < 0.6, pm_norte == 0, pm == N11[["barrio_alto"]],
          prima_lat$barrio_nom[which.max(prima_lat$beta)] == pm)

guardar_numeros(
  "13_numeros.csv",
  cor_prima_lat = r_lat, cor_prima_lat_sin_pm = r_lat_sin, r2_prima_lat = r_lat^2,
  pm_pct_norte = pm_norte, base_regresion = base_reg, base_comunicativo = base_com
)

# --- G20. La prima de cada barrio contra su latitud ---------------------------
# Eje x: kilómetros al norte (+) o al sur (−) de la latitud mediana de los
# avisos, que es el corte entre las dos mitades de la ciudad (un grado de
# latitud son 111 km, como en el script 08). Eje y: la prima en logaritmo,
# que es donde se ajusta la recta y se calcula la correlación, con las marcas
# rotuladas en porcentaje exacto, (exp(β) − 1) × 100.

lat_corte <- median(avisos_dep$lat)
prima_lat <- prima_lat |> mutate(km_norte = (lat - lat_corte) * 111)
marcas_pct <- c(-20, 0, 25, 50, 100, 150)

g20 <- ggplot(prima_lat, aes(km_norte, beta)) +
  geom_vline(xintercept = 0, color = colores$grilla, linewidth = 0.6) +
  # Abajo a la derecha del corte, que está vacío: arriba pisaba a Puerto Madero.
  annotate("text", x = 0.15, y = min(prima_lat$beta), hjust = 0, vjust = 0,
           size = 3, family = fuente_cuerpo, color = colores$secundario,
           label = "Latitud mediana de los avisos:\nel corte entre las dos mitades") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = colores$acento2, linewidth = 0.8, linetype = "22") +
  geom_point(aes(color = barrio_nom == pm), size = 2.6, alpha = 0.9) +
  geom_text(data = filter(prima_lat, barrio_nom == pm),
            aes(label = paste0(pm, ": todos sus avisos en la\nmitad sur y la prima más alta")),
            hjust = 0, nudge_x = 0.35, size = 3.3, lineheight = 0.95,
            family = fuente_cuerpo, color = colores$acento, fontface = "bold") +
  geom_text(data = filter(prima_lat, barrio_nom == base_reg),
            aes(label = paste0(base_reg, " (base)")), hjust = 1, nudge_x = -0.3, size = 2.9,
            family = fuente_cuerpo, color = colores$secundario) +
  scale_color_manual(values = c(`TRUE` = colores$acento, `FALSE` = colores$apagado), guide = "none") +
  scale_x_continuous(labels = \(x) paste0(case_when(x > 0 ~ "+", x < 0 ~ "−", TRUE ~ ""),
                                          num_ar(abs(x)), " km"),
                     expand = expansion(mult = c(0.04, 0.30))) +
  scale_y_continuous(breaks = log(1 + marcas_pct / 100),
                     labels = paste0(if_else(marcas_pct > 0, "+", ""), marcas_pct, "%")) +
  labs(x = "Latitud mediana de los avisos del barrio, en km al norte (+) o al sur (−) del corte",
       y = paste0("Prima del barrio respecto de ", base_reg, "\n(escala logarítmica)"),
       caption = pie(paste0(
         fuente_pie, ". Un punto por barrio (", nrow(prima_lat), " barrios con al menos 100 avisos). ",
         "Prima: coeficiente del barrio en la regresión de log(precio) con superficie, ambientes, baños, ",
         "antigüedad, distancia al Obelisco y quién publica. Recta ajustada por mínimos cuadrados sobre ",
         "el logaritmo de la prima. Correlación: ", coma_ar(r_lat, 2), "; sin ", pm, ": ",
         coma_ar(r_lat_sin, 2), ".")))

guardar(g20, "g20_prima_latitud.png", ancho = 9, alto = 5.6)


# =============================================================================
# 3. LOS CONTROLES SOBRE LOS VALORES
# =============================================================================

# La base y las variables.
vars_orig <- str_extract(campo("Variables"), "^\\d+")
vars_uso  <- nrow(var_viv) + nrow(var_ubi)
comunas   <- str_match(campo("Cobertura"), "(\\d+) comunas")[, 2]
periodo   <- str_extract(campo("Período"), "noviembre de \\d{4}")
stopifnot(!is.na(vars_orig), !is.na(comunas), !is.na(periodo),
          str_detect(campo("Variables"), paste0(vars_uso, " las que usamos")),
          str_detect(campo("Observaciones"), num_ar(g(N6, "n_crudo"))),
          str_detect(campo("Observaciones"), num_ar(g(N6, "n_base"))))

# Las republicaciones cierran: crudo menos copias es la base de análisis.
es_repub     <- str_starts(repetidos$tipo, "Republicación")
grupos_repub <- sum(repetidos$grupos[es_repub])
copias_repub <- sum(repetidos$copias_redundantes[es_repub])
grupos_dist  <- repetidos$grupos[str_starts(repetidos$tipo, "Unidades distintas")]
stopifnot(g(N6, "n_crudo") - copias_repub == g(N6, "n_base"))

# Las seis combinaciones de regla y escala: la más laxa cubre casi todo.
seis <- as.numeric(N6[c("iqr_orig", "z_orig", "zrob_orig", "iqr_log", "z_log", "zrob_log")])
stopifnot(max(seis) == g(N6, "marcados_max"), g(N6, "marcados_max") <= g(N6, "marcados_alguna"))

# La muestra de los modelos es la misma en el 08 y en el 12.
stopifnot(g(N8, "n_mod") == g(N12, "n_avisos_42"),
          g(N8, "n_chicos") == g(N12, "avisos_chicos"),
          g(N8, "n_barrios_chicos") == g(N12, "n_chicos"),
          g(N12, "n_avisos") == g(N6, "n_base"),
          g(N12, "part_chicos") < 0.01, g(N12, "part_k50") >= 0.5)

# El eje norte-sur de la instancia 1 se sostiene como lo dicen los textos.
pct_g3 <- str_extract_all(titulos_i1$titulo[titulos_i1$id == "g3"], "\\d+%")[[1]]
stopifnot(length(pct_g3) == 2,
          N3[["tramo_max_norte"]] == N3[["tramo_max_sur"]],
          N3[["tramo_min_norte"]] == N3[["tramo_min_sur"]])

# Los nombres de los tipos del kmeans siguen describiendo a los grupos.
stopifnot(which.max(perfil$pct_mas50) == 1, which.min(perfil$precio_m2) == 1,
          which.max(perfil$pct_estrenar) == 2, which.min(perfil$m2) == 2,
          which.max(perfil$m2) == 3, which.max(perfil$precio_m2) == 3,
          which.min(perfil$dist_km) == 1,
          N9[["comunas_todos"]] == N9[["n_comunas"]], N9[["barrios_todos"]] == N9[["n_barrios"]])

# La síntesis: tres escalones del modelo, estrictamente decrecientes, y el
# kmeans aparte, como verificación independiente (un piso, no un escalón).
dentro_res <- dentro |>
  group_by(grupo) |>
  summarise(spearman = cor(pm2, prima_pct, method = "spearman"),
            rango = max(pm2) / min(pm2), .groups = "drop")
stopifnot(g(N9, "rango_total") > g(N8, "brecha_p3"),
          g(N8, "brecha_p3") > g(N8, "brecha_extremos"),
          abs(g(N11, "brecha") - g(N8, "brecha_p3")) < 1e-9,
          min(dentro_res$rango) > 1)

# El gráfico comunicativo y la regresión dicen lo mismo con bases distintas.
stopifnot(abs((1 + g(N11, "prima_alto_pct") / 100) - g(N8, "brecha_p3")) < 1e-9)

# Los números del README (sección 1) coinciden con los del 08.
readme <- paste(readLines("README.md", encoding = "UTF-8"), collapse = "\n")
num_readme <- function(patron) str_match(readme, patron)[, 2]
stopifnot(
  num_readme("es (\\d+,\\d) veces el del más barato") == f(N8, "brecha_p3", 1),
  num_readme("apenas aporta nada, (\\d+,\\d+) puntos de R²") == coma_ar(100 * g(N8, "dr2_sup_m2"), 2),
  num_readme("da cuenta de esa variación: (\\d+,\\d) puntos") == coma_ar(100 * g(N8, "dr2_bar_m2"), 1)
)


# =============================================================================
# 4. LA TABLA: CADA NÚMERO DE CADA DIAPOSITIVA, CON SU FUENTE
# =============================================================================
# Un renglón por número, formateado como se usa en la diapositiva (coma
# decimal, punto de miles). Cuando el mismo número va con distinta cantidad de
# decimales en el texto corto y en el respaldo, van los dos.

filas <- list()
dato <- function(diapo, clave, valor, fuente) {
  filas[[length(filas) + 1]] <<- tibble(diapositiva = diapo, clave = clave,
                                        valor = as.character(valor), fuente = fuente)
}

# --- 2. La base -----------------------------------------------------------------
dato(2, "avisos_crudo", f(N6, "n_crudo"), "06_numeros: n_crudo")
dato(2, "periodo", periodo, "04_ficha_base: Período")
dato(2, "barrios", f(N12, "n_barrios"), "12_numeros: n_barrios")
dato(2, "comunas", comunas, "04_ficha_base: Cobertura")
dato(2, "variables_originales", vars_orig, "04_ficha_base: Variables")
dato(2, "superficie_max_m2", extremo("metros_cubiertos", "Máximo"), "04_evidencia_recorte")
dato(2, "precio_max_usd", extremo("precio_usd", "Máximo"), "04_evidencia_recorte")
dato(2, "geocod_min", extremo("geocod_score", "Mínimo"), "04_evidencia_recorte")

# --- 3. Variables y base de análisis ---------------------------------------------
dato(3, "variables_originales", vars_orig, "04_ficha_base: Variables")
dato(3, "variables_usadas", vars_uso, "04_variables_vivienda + 04_variables_ubicacion")
dato(3, "variables_vivienda", nrow(var_viv), "04_variables_vivienda")
dato(3, "variables_ubicacion", nrow(var_ubi), "04_variables_ubicacion")
dato(3, "conjuntos_republicados", num_ar(grupos_repub), "02_clasificacion_repetidos")
dato(3, "copias_sacadas", num_ar(copias_repub), "02_clasificacion_repetidos")
dato(3, "conjuntos_unidades_distintas", num_ar(grupos_dist), "02_clasificacion_repetidos")
dato(3, "avisos_crudo", f(N6, "n_crudo"), "06_numeros: n_crudo")
dato(3, "avisos_base", f(N6, "n_base"), "06_numeros: n_base")
dato(3, "mediana_pm2_crudo", num_ar(efecto$precio_m2_mediano[efecto$version == "Base cruda"]),
     "02_efecto_deduplicacion")
dato(3, "mediana_pm2_base", num_ar(efecto$precio_m2_mediano[efecto$version == "Base de análisis"]),
     "02_efecto_deduplicacion")

# --- 4. Concentración --------------------------------------------------------------
dato(4, "barrios_mitad", f(N12, "k50"), "12_numeros: k50")
dato(4, "pct_barrios_mitad", pc(N12, "part_k50"), "12_numeros: part_k50")
dato(4, "barrios", f(N12, "n_barrios"), "12_numeros: n_barrios")
dato(4, "barrios_chicos", f(N12, "n_chicos"), "12_numeros: n_chicos")
dato(4, "numero_equivalente", f(N12, "n_equivalente", 1), "12_numeros: n_equivalente")
dato(4, "hhi_10000", num_ar(g(N12, "hhi_10000")), "12_numeros: hhi_10000")
dato(4, "hhi_minimo_10000", num_ar(10000 * g(N12, "hhi_min")), "12_numeros: hhi_min")
dato(4, "gini", f(N12, "gini", 2), "12_numeros: gini")
dato(4, "cr5_pct", pc(N12, "cr5"), "12_numeros: cr5")
dato(4, "cr5_parejo_pct", pc(N12, "cr5_iguales"), "12_numeros: cr5_iguales")
dato(4, "top5", N12[["top5"]], "12_numeros: top5")
dato(4, "palermo_pct", pc(N12, "palermo"), "12_numeros: palermo")
dato(4, "barrios_modelos", f(N8, "n_barrios"), "08_numeros: n_barrios")
dato(4, "pct_chicos_1dec", pc(N12, "part_chicos"), "12_numeros: part_chicos")
dato(4, "pct_chicos_2dec", pc(N12, "part_chicos", 2), "12_numeros: part_chicos")
dato(4, "pct_chicos_parejo", coma_ar(100 * g(N12, "n_chicos") / g(N12, "n_barrios"), 1),
     "12_numeros: n_chicos / n_barrios")
dato(4, "barrios_chicos_nombres", N12[["chicos"]], "12_numeros: chicos")
dato(4, "avisos_chicos", f(N12, "avisos_chicos"), "12_numeros: avisos_chicos")
dato(4, "avisos_modelos", f(N12, "n_avisos_42"), "12_numeros: n_avisos_42")
dato(4, "no_departamentos", f(N8, "n_no_depto"), "08_numeros: n_no_depto")
dato(4, "hhi_10000_42", num_ar(g(N12, "hhi_10000_42")), "12_numeros: hhi_10000_42")
dato(4, "numero_equivalente_42", f(N12, "n_equivalente_42", 1), "12_numeros: n_equivalente_42")
dato(4, "gini_42", f(N12, "gini_42", 2), "12_numeros: gini_42")
dato(4, "ee_correlacion_log", f(N12, "ee_cor", 2), "12_numeros: ee_cor")
dato(4, "ee_barrio_chico", N12[["ee_barrio_chico"]], "12_numeros: ee_barrio_chico")
dato(4, "ee_avisos_chico", f(N12, "ee_avisos_chico"), "12_numeros: ee_avisos_chico")
dato(4, "ee_chico", f(N12, "ee_chico", 4), "12_numeros: ee_chico")
dato(4, "ee_barrio_grande", N12[["ee_barrio_grande"]], "12_numeros: ee_barrio_grande")
dato(4, "ee_avisos_grande", f(N12, "ee_avisos_grande"), "12_numeros: ee_avisos_grande")
dato(4, "ee_grande", f(N12, "ee_grande", 4), "12_numeros: ee_grande")
dato(4, "ee_razon", coma_ar(g(N12, "ee_chico") / g(N12, "ee_grande"), 1), "12_numeros: ee_chico / ee_grande")
dato(4, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")

# --- 5. Faltantes ------------------------------------------------------------------
dato(5, "na_inmobiliaria", f(N6, "na_inmo"), "06_numeros: na_inmo")
dato(5, "pct_na_inmobiliaria", f(N6, "pct_na_inmo", 1), "06_numeros: pct_na_inmo")
dato(5, "na_antiguedad", f(N6, "na_antig"), "06_numeros: na_antig")
dato(5, "pct_na_antiguedad", f(N6, "pct_na_antig", 1), "06_numeros: pct_na_antig")
dato(5, "filas_completas", f(N6, "filas_completas"), "06_numeros: filas_completas")
dato(5, "avisos_base", f(N6, "n_base"), "06_numeros: n_base")
dato(5, "barrios_100", f(N6, "part_n_barrios"), "06_numeros: part_n_barrios")
dato(5, "particulares_max_pct", f(N6, "part_max"), "06_numeros: part_max")
dato(5, "particulares_max_barrio", N6[["part_max_barrio"]], "06_numeros: part_max_barrio")
dato(5, "particulares_min_pct", f(N6, "part_min"), "06_numeros: part_min")
dato(5, "particulares_min_barrio", N6[["part_min_barrio"]], "06_numeros: part_min_barrio")
dato(5, "particulares_correlacion", f(N6, "part_pearson", 2), "06_numeros: part_pearson")
dato(5, "pct_sin_dato_publicacion", f(N6, "pct_pubrec", 1), "06_numeros: pct_pubrec")

# --- 6. Atípicos -------------------------------------------------------------------
dato(6, "seis_min", num_ar(min(seis)), "06_numeros: mínimo de las seis combinaciones")
dato(6, "seis_max", num_ar(max(seis)), "06_numeros: máximo de las seis combinaciones")
dato(6, "marcados_max", f(N6, "marcados_max"), "06_numeros: marcados_max")
dato(6, "marcados_alguna", f(N6, "marcados_alguna"), "06_numeros: marcados_alguna")
dato(6, "z_original", f(N6, "z_orig"), "06_numeros: z_orig")
dato(6, "zrob_original", f(N6, "zrob_orig"), "06_numeros: zrob_orig")
dato(6, "z_log", f(N6, "z_log"), "06_numeros: z_log")
dato(6, "iqr_log", f(N6, "iqr_log"), "06_numeros: iqr_log")
dato(6, "pm_top100", f(N6, "pm_top100"), "06_numeros: pm_top100")
dato(6, "no_departamentos", f(N6, "no_depto"), "06_numeros: no_depto")
dato(6, "no_departamentos_tipos", N6[["no_depto_tipos"]], "06_numeros: no_depto_tipos")
dato(6, "dist_asimetria", f(N6, "dist_asim_orig", 2), "06_numeros: dist_asim_orig")
dato(6, "dist_iqr_original", f(N6, "dist_iqr_orig"), "06_numeros: dist_iqr_orig")
dato(6, "dist_iqr_log", f(N6, "dist_iqr_log"), "06_numeros: dist_iqr_log")
dato(6, "dist_limite_m", f(N6, "dist_log_lim_m"), "06_numeros: dist_log_lim_m")
dato(6, "sacados_ric", f(N6, "sacados_ric"), "06_numeros: sacados_ric")
dato(6, "p95_base", f(N6, "p95_base"), "06_numeros: p95_base")
dato(6, "p95_sin_ric", f(N6, "p95_sin_ric"), "06_numeros: p95_sin_ric")
dato(6, "winsor_sobre_p99", f(N8, "w_sobre_p99"), "08_numeros: w_sobre_p99")
dato(6, "winsor_p99", f(N8, "w_p99"), "08_numeros: w_p99")
dato(6, "winsor_pm", f(N8, "w_pm_sobre_p99"), "08_numeros: w_pm_sobre_p99")
dato(6, "winsor_prima_pm", f(N8, "w_prima_pm", 1), "08_numeros: w_prima_pm (respecto de la base)")
dato(6, "winsor_prima_pm_w", f(N8, "w_prima_pm_w", 1), "08_numeros: w_prima_pm_w (respecto de la base)")
dato(6, "elasticidad", f(N8, "b4", 3), "08_numeros: b4")
dato(6, "winsor_elasticidad", f(N8, "w_b4", 3), "08_numeros: w_b4")
dato(6, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")

# --- 7. El precio y el eje norte-sur -----------------------------------------------
dato(7, "titulo_g3", titulos_i1$titulo[titulos_i1$id == "g3"], "03_titulos_graficos: g3")
dato(7, "norte_mas_caro_min", pct_g3[1], "03_titulos_graficos: g3")
dato(7, "norte_mas_caro_max", pct_g3[2], "03_titulos_graficos: g3")
dato(7, "mediana_precio", descr$Mediana[str_starts(descr$Variable, "Precio publicado")], "04_descriptivas")
dato(7, "media_precio", f(N6, "media_base"), "06_numeros: media_base")
dato(7, "cor_pm2_distancia", correl$`Precio por m²`[str_starts(correl$`Correlación con`, "Distancia")],
     "04_correlaciones")
dato(7, "cor_pm2_latitud", correl$`Precio por m²`[str_starts(correl$`Correlación con`, "Latitud")],
     "04_correlaciones")
dato(7, "tramo_max", N3[["tramo_max_norte"]], "03_numeros: tramo_max_norte (= tramo_max_sur)")
dato(7, "tramo_min", N3[["tramo_min_norte"]], "03_numeros: tramo_min_norte (= tramo_min_sur)")
dato(7, "pm2_cerca_norte", f(N3, "min_cerca_norte"), "03_numeros: min_cerca_norte")
dato(7, "pm2_cerca_sur", f(N3, "min_cerca_sur"), "03_numeros: min_cerca_sur")
dato(7, "brecha_estrenar", f(N3, "brecha_estrenar", 2), "03_numeros: brecha_estrenar")
dato(7, "brecha_hasta20", f(N3, "brecha_hasta20", 2), "03_numeros: brecha_hasta20")
dato(7, "brecha_20a50", f(N3, "brecha_20a50", 2), "03_numeros: brecha_20a50")
dato(7, "brecha_mas50", f(N3, "brecha_mas50", 2), "03_numeros: brecha_mas50")
dato(7, "pm2_entre_barrios_pct", varianza$`Entre barrios (%)`[varianza$Variable == "Precio por m²"],
     "04_varianza_barrios")
dato(7, "pm2_dentro_barrios_pct", varianza$`Dentro de cada barrio (%)`[varianza$Variable == "Precio por m²"],
     "04_varianza_barrios")

# --- 8. Cercanía -------------------------------------------------------------------
cos2_pm2 <- 100 * (g(N7, "pm2_pc1")^2 + g(N7, "pm2_pc2")^2)
cos2_lat <- 100 * (g(N7, "lat_pc1")^2 + g(N7, "lat_pc2")^2)
dato(8, "pc1_pct", f(N7, "pc1", 1), "07_numeros: pc1")
dato(8, "pc2_pct", f(N7, "pc2", 1), "07_numeros: pc2")
dato(8, "obelisco_pc1", f(N7, "obelisco_pc1", 2), "07_numeros: obelisco_pc1")
# Las coordenadas van con tres decimales: con dos, la suma de sus cuadrados no
# reproduce el porcentaje (0,12² + 0,18² da 4,7% y el valor es 4,9%). Se
# verifica que con lo que se muestra la cuenta cierre.
dato(8, "pm2_pc1", f(N7, "pm2_pc1", 3), "07_numeros: pm2_pc1")
dato(8, "pm2_pc2", f(N7, "pm2_pc2", 3), "07_numeros: pm2_pc2")
dato(8, "lat_pc1", f(N7, "lat_pc1", 3), "07_numeros: lat_pc1")
dato(8, "lat_pc2", f(N7, "lat_pc2", 3), "07_numeros: lat_pc2")
cos2_visible <- function(a, b) coma_ar(100 * (round(g(N7, a), 3)^2 + round(g(N7, b), 3)^2), 1)
stopifnot(cos2_visible("pm2_pc1", "pm2_pc2") == coma_ar(cos2_pm2, 1),
          cos2_visible("lat_pc1", "lat_pc2") == coma_ar(cos2_lat, 1))
dato(8, "cos2_pm2_pct", coma_ar(cos2_pm2, 1), "07_numeros: pm2_pc1² + pm2_pc2²")
dato(8, "cos2_lat_pct", coma_ar(cos2_lat, 1), "07_numeros: lat_pc1² + lat_pc2²")
dato(8, "tope_pm2_pct", ceiling(cos2_pm2), "07_numeros: cota redonda de cos2_pm2")
dato(8, "tope_lat_pct", ceiling(cos2_lat), "07_numeros: cota redonda de cos2_lat")

# --- 9. La regresión: por qué, H0 y H1 ---------------------------------------------
dato(9, "avisos_modelos", f(N8, "n_mod"), "08_numeros: n_mod")
dato(9, "barrios_modelos", f(N8, "n_barrios"), "08_numeros: n_barrios")
dato(9, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")
dato(9, "F_barrio", f(N8, "F_h0", 2), "08_numeros: F_h0")
dato(9, "gl1", f(N8, "gl1_h0"), "08_numeros: gl1_h0")
dato(9, "gl2", f(N8, "gl2_h0"), "08_numeros: gl2_h0")
dato(9, "r2_paso2_4dec", f(N8, "r2_2", 4), "08_numeros: r2_2")
dato(9, "r2_paso3_4dec", f(N8, "r2_3", 4), "08_numeros: r2_3")
dato(9, "delta_r2_barrio_puntos", coma_ar(100 * (round(g(N8, "r2_3"), 4) - round(g(N8, "r2_2"), 4)), 2),
     "08_numeros: r2_3 − r2_2, con cuatro decimales")

# --- 10. Los cuatro pasos ------------------------------------------------------------
dato(10, "pct_10_mas", f(N8, "pct_10mas", 1), "08_numeros: pct_10mas")
for (k in 1:4) {
  dato(10, paste0("r2_paso", k, "_3dec"), f(N8, paste0("r2_", k), 3), paste0("08_numeros: r2_", k))
  dato(10, paste0("r2_paso", k, "_4dec"), f(N8, paste0("r2_", k), 4), paste0("08_numeros: r2_", k))
}
dato(10, "r2_ajustado_paso4", f(N8, "r2aj_4", 4), "08_numeros: r2aj_4")
dato(10, "elasticidad", f(N8, "b4", 3), "08_numeros: b4")
dato(10, "categorias_barrio", f(N8, "gl1_h0"), "08_numeros: gl1_h0")
dato(10, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")

# --- 11. Sesgo por variable omitida -------------------------------------------------
dato(11, "b_paso1", f(N8, "b1", 3), "08_numeros: b1")
dato(11, "b_paso2", f(N8, "b2", 3), "08_numeros: b2")
dato(11, "b_paso3", f(N8, "b3", 3), "08_numeros: b3")
dato(11, "aporte_ambientes", con_signo(g(N8, "amb_aporte")), "08_numeros: amb_aporte")
dato(11, "aporte_banos", con_signo(g(N8, "banos_aporte")), "08_numeros: banos_aporte")
dato(11, "aporte_barrio", con_signo(g(N8, "barrio_aporte")), "08_numeros: barrio_aporte")
dato(11, "cor_ambientes_m2", f(N8, "cor_amb_m2", 2), "08_numeros: cor_amb_m2")
dato(11, "cor_grafico", f(N8, "cor_graf", 2), "08_numeros: cor_graf")
dato(11, "cor_condicional", f(N8, "cor_cond", 2), "08_numeros: cor_cond")
dato(11, "cor_tercil_caro", f(N8, "cor_tercil_caro", 2), "08_numeros: cor_tercil_caro")
dato(11, "cor_tercil_barato", f(N8, "cor_tercil_barato", 2), "08_numeros: cor_tercil_barato")
dato(11, "cor_sin_pm", f(N8, "cor_sin_pm", 2), "08_numeros: cor_sin_pm")
dato(11, "ambientes_paso2", f(N8, "amb_2", 3), "08_numeros: amb_2")
dato(11, "ambientes_paso3", f(N8, "amb_3", 3), "08_numeros: amb_3")
dato(11, "ambientes_p_paso3", f(N8, "amb_p3", 2), "08_numeros: amb_p3")
dato(11, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")

# --- 12. Identidad y aporte de cada bloque -------------------------------------------
dato(12, "dr2_superficie_precio", pts("dr2_sup_p"), "08_numeros: dr2_sup_p (en puntos)")
dato(12, "dr2_barrio_precio", pts("dr2_bar_p"), "08_numeros: dr2_bar_p (en puntos)")
dato(12, "dr2_barrio_pm2", pts("dr2_bar_m2"), "08_numeros: dr2_bar_m2 (en puntos)")
dato(12, "dr2_superficie_pm2", pts("dr2_sup_m2", 2), "08_numeros: dr2_sup_m2 (en puntos)")
dato(12, "F_superficie_pm2", f(N8, "F_sup_m2", 1), "08_numeros: F_sup_m2")
dato(12, "F_superficie_gl1", f(N8, "F_sup_m2_gl1"), "08_numeros: F_sup_m2_gl1")
dato(12, "F_superficie_gl2", f(N8, "F_sup_m2_gl2"), "08_numeros: F_sup_m2_gl2")
dato(12, "caida_scr_barrio", f(N8, "scr_bar"), "08_numeros: scr_bar")
dato(12, "avisos_modelos", f(N8, "n_mod"), "08_numeros: n_mod")

# --- 13. Con qué modelo nos quedamos -------------------------------------------------
dato(13, "r2_barrio_3dec", f(N8, "r2_4", 3), "08_numeros: r2_4")
dato(13, "r2_latitud_3dec", f(N8, "r2_lat", 3), "08_numeros: r2_lat")
dato(13, "r2_barrio_4dec", f(N8, "r2_4", 4), "08_numeros: r2_4")
dato(13, "r2_latitud_4dec", f(N8, "r2_lat", 4), "08_numeros: r2_lat")
dato(13, "categorias_barrio", f(N8, "gl1_h0"), "08_numeros: gl1_h0")
dato(13, "latitud_km_pct_1dec", f(N8, "lat_km_pct", 1), "08_numeros: lat_km_pct")
dato(13, "latitud_km_pct_2dec", f(N8, "lat_km_pct", 2), "08_numeros: lat_km_pct")
dato(13, "barrio_prima_max", pm, "08_numeros: prima_max_barrio")
dato(13, "prima_max_pct", f(N8, "prima_max_pct", 1), "08_numeros: prima_max_pct (respecto de la base)")
dato(13, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")
dato(13, "pm_pct_norte", coma_ar(100 * pm_norte, 0), "13_numeros: pm_pct_norte")
dato(13, "cor_prima_latitud", coma_ar(r_lat, 2), "13_numeros: cor_prima_lat")
dato(13, "cor_prima_latitud_sin_pm", coma_ar(r_lat_sin, 2), "13_numeros: cor_prima_lat_sin_pm")
dato(13, "r2_prima_latitud", coma_ar(r_lat^2, 2), "13_numeros: r2_prima_lat")

# --- 14. Supuestos y limitaciones ----------------------------------------------------
m2_elast <- str_remove(str_subset(names(N8), "^elast_"), "^elast_")
stopifnot(length(m2_elast) == 2)
dato(14, "reset", f(N8, "reset"), "08_numeros: reset")
dato(14, "reset_cuadratico", f(N8, "reset_q"), "08_numeros: reset_q")
dato(14, "avisos_modelos", f(N8, "n_mod"), "08_numeros: n_mod")
dato(14, "elasticidad_m2_chico", m2_elast[1], "08_numeros: elast_*")
dato(14, "elasticidad_m2_grande", m2_elast[2], "08_numeros: elast_*")
dato(14, "elasticidad_chico", f(N8, paste0("elast_", m2_elast[1]), 2), paste0("08_numeros: elast_", m2_elast[1]))
dato(14, "elasticidad_grande", f(N8, paste0("elast_", m2_elast[2]), 2), paste0("08_numeros: elast_", m2_elast[2]))
dato(14, "F_barrio", f(N8, "F_h0"), "08_numeros: F_h0")
dato(14, "F_barrio_cuadratico", f(N8, "F_h0_q"), "08_numeros: F_h0_q")
dato(14, "dr2_superficie_pm2_cuadratico", pts("dr2_sup_m2_q", 2), "08_numeros: dr2_sup_m2_q (en puntos)")
dato(14, "breusch_pagan", f(N8, "bp", 1), "08_numeros: bp")
dato(14, "cook_n", f(N8, "cook_n"), "08_numeros: cook_n")
dato(14, "cook_pct", f(N8, "cook_pct", 1), "08_numeros: cook_pct")
dato(14, "elasticidad", f(N8, "b4", 3), "08_numeros: b4")
dato(14, "elasticidad_sin_cook", f(N8, "b4_sin", 3), "08_numeros: b4_sin")
dato(14, "prima_pm_con", f(N8, "pm_con", 1), "08_numeros: pm_con (respecto de la base)")
dato(14, "prima_pm_sin", f(N8, "pm_sin", 1), "08_numeros: pm_sin (respecto de la base)")
dato(14, "brecha_con", f(N8, "brecha_con", 2), "08_numeros: brecha_con")
dato(14, "brecha_sin", f(N8, "brecha_sin", 2), "08_numeros: brecha_sin")
dato(14, "gvif_max", f(N8, "gvif_max", 2), "08_numeros: gvif_max")
dato(14, "r2_distancia_barrio_pct", coma_ar(100 * g(N8, "r2_dist_barrio"), 1), "08_numeros: r2_dist_barrio")
dato(14, "avisos_chicos", f(N8, "n_chicos"), "08_numeros: n_chicos")
dato(14, "barrios_chicos", f(N8, "n_barrios_chicos"), "08_numeros: n_barrios_chicos")
dato(14, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")

# --- 15. Kmeans: decisiones y k ------------------------------------------------------
dato(15, "k_silueta", f(N9, "k_sil"), "09_numeros: k_sil")
dato(15, "k_elegido", f(N9, "K"), "09_numeros: K")
dato(15, "silueta_k_silueta", f(N9, "silueta_k2", 2), "09_numeros: silueta_k2")
dato(15, "silueta_k_elegido", f(N9, "silueta", 2), "09_numeros: silueta")
dato(15, "gap_empate", N9[["gap_empate"]], "09_numeros: gap_empate (1 = el gap no distingue)")

# --- 16. Perfiles y geografía --------------------------------------------------------
for (gr in 1:3) {
  dato(16, paste0("grupo", gr, "_pct_0dec"), p(gr, "pct", 0), "09_perfiles: pct")
  dato(16, paste0("grupo", gr, "_pct_1dec"), p(gr, "pct"), "09_perfiles: pct")
  dato(16, paste0("grupo", gr, "_pm2"), num_ar(perfil$precio_m2[gr]), "09_perfiles: precio_m2")
  dato(16, paste0("grupo", gr, "_m2"), num_ar(perfil$m2[gr]), "09_perfiles: m2")
  dato(16, paste0("grupo", gr, "_pct_estrenar"), p(gr, "pct_estrenar"), "09_perfiles: pct_estrenar")
  dato(16, paste0("grupo", gr, "_pct_mas50"), p(gr, "pct_mas50"), "09_perfiles: pct_mas50")
  dato(16, paste0("grupo", gr, "_dist_km"), p(gr, "dist_km"), "09_perfiles: dist_km")
  dato(16, paste0("grupo", gr, "_pct_norte"), p(gr, "pct_norte", 0), "09_perfiles: pct_norte")
}
dato(16, "tramo_cercano", N3[["tramo_min_norte"]], "03_numeros: tramo_min_norte")
dato(16, "pct_mas50_cerca", f(N3, "pct_mas50_cerca", 1), "03_numeros: pct_mas50_cerca")
dato(16, "pct_mas50_resto_max", f(N3, "pct_mas50_resto_max", 1), "03_numeros: pct_mas50_resto_max")
dato(16, "ari_barrio", a("Grupos contra barrio", 3), "09_comparacion_ari")
dato(16, "ari_comuna", a("Grupos contra comuna", 3), "09_comparacion_ari")
dato(16, "ari_mitad", a("Grupos contra mitad norte-sur", 3), "09_comparacion_ari")
dato(16, "comunas", f(N9, "n_comunas"), "09_numeros: n_comunas")
dato(16, "barrios_con_los_tres", f(N9, "barrios_todos"), "09_numeros: barrios_todos")
dato(16, "pam_pct_igual", f(N9, "pct_igual_pam", 1), "09_numeros: pct_igual_pam")
dato(16, "ari_pam", a("kmeans contra PAM"), "09_comparacion_ari")
dato(16, "extremos", f(N9, "n_extremos"), "09_numeros: n_extremos")
dato(16, "ari_sin_extremos", a("kmeans con y sin extremos"), "09_comparacion_ari")
dato(16, "silueta", f(N9, "silueta", 2), "09_numeros: silueta")
dato(16, "ari_sin_estandarizar", a("kmeans con y sin estandarizar"), "09_comparacion_ari")

# --- 17. El jerárquico de barrios ----------------------------------------------------
dato(17, "grupo_viejo_barrios", f(N11, "grupo_viejo_barrios"), "11_numeros: grupo_viejo_barrios")
dato(17, "grupo_viejo_sur", f(N11, "grupo_viejo_sur"), "11_numeros: grupo_viejo_sur")
dato(17, "grupo_otro_barrios", f(N11, "grupo_otro_barrios"), "11_numeros: grupo_otro_barrios")
dato(17, "grupo_otro_norte", f(N11, "grupo_otro_norte"), "11_numeros: grupo_otro_norte")
dato(17, "barrios_modelos", f(N8, "n_barrios"), "08_numeros: n_barrios")
dato(17, "ari_mitad", f(N11, "ari_mitad", 2), "11_numeros: ari_mitad")
dato(17, "p_permutacion", f(N11, "p_perm", 4), "11_numeros: p_perm")
dato(17, "ari_comuna", f(N11, "ari_comuna", 2), "11_numeros: ari_comuna")
dato(17, "ari_hellinger", f(N11, "ari_hellinger", 2), "11_numeros: ari_hellinger")
dato(17, "tamanos_promedio_completo", N11[["tam_promedio"]], "11_numeros: tam_promedio")
dato(17, "ari_kmeans_mitad", a("Grupos contra mitad norte-sur", 3), "09_comparacion_ari")
dato(17, "barrio_solo_con_k3", N11[["barrio_alto"]], "11_numeros: barrio_alto")

# --- 18. Síntesis ---------------------------------------------------------------------
# Tres escalones del modelo, y el kmeans aparte.
dato(18, "escalon1_crudo", f(N9, "rango_total", 1), "09_numeros: rango_total")
dato(18, "escalon2_igual_vivienda", f(N8, "brecha_p3", 1), "08_numeros: brecha_p3")
dato(18, "escalon2_barrios", paste(N8[["p3_max_barrio"]], "contra", N8[["p3_min_barrio"]]),
     "08_numeros: p3_max_barrio, p3_min_barrio")
dato(18, "escalon3_con_entorno", f(N8, "brecha_extremos", 1), "08_numeros: brecha_extremos")
dato(18, "escalon3_barrios", paste(N8[["prima_max_barrio"]], "contra", N8[["prima_min_barrio"]]),
     "08_numeros: prima_max_barrio, prima_min_barrio")
dato(18, "kmeans_piso_min", coma_ar(min(dentro_res$rango), 1), "09_dentro_grupo: mínimo del rango por tipo")
dato(18, "kmeans_piso_max", coma_ar(max(dentro_res$rango), 1), "09_dentro_grupo: máximo del rango por tipo")
dato(18, "avisos_modelos", f(N8, "n_mod"), "08_numeros: n_mod")
dato(18, "base_comunicativo", base_com, "11_numeros: referencia_com")
dato(18, "barrio_alto", N11[["barrio_alto"]], "11_numeros: barrio_alto")
dato(18, "prima_alto_pct_0dec", f(N11, "prima_alto_pct"), "11_numeros: prima_alto_pct (respecto de Villa Lugano)")
dato(18, "prima_alto_pct_1dec", f(N11, "prima_alto_pct", 1), "11_numeros: prima_alto_pct (respecto de Villa Lugano)")
dato(18, "brecha_grafico", f(N11, "brecha", 1), "11_numeros: brecha")
dato(18, "m2_mediana", f(N11, "m2_ref"), "11_numeros: m2_ref")
dato(18, "precio_alto", f(N11, "p_alto"), "11_numeros: p_alto")
dato(18, "precio_bajo", f(N11, "p_bajo"), "11_numeros: p_bajo")
dato(18, "base_regresion", base_reg, "08_prima_superficie_barrio (prima 0)")
dato(18, "prima_recoleta_p3", coma_ar(prima_p3$prima_pct[prima_p3$barrio_nom == "Recoleta"], 0),
     "08_prima_superficie_barrio: Recoleta (respecto de la base)")
dato(18, "recoleta_pct_grupo_viejo", coma_ar(ej("Recoleta", 1, "pct"), 1), "09_ejemplo_recoleta")
dato(18, "recoleta_pm2_grupo_viejo", num_ar(ej("Recoleta", 1, "pm2")), "09_ejemplo_recoleta")
dato(18, "base_pm2_grupo_viejo", num_ar(ej(base_reg, 1, "pm2")), "09_ejemplo_recoleta")

# --- 19. Conclusiones -------------------------------------------------------------------
dato(19, "brecha_igual_vivienda", f(N8, "brecha_p3", 1), "08_numeros: brecha_p3")
dato(19, "dr2_superficie_pm2", coma_ar(100 * g(N8, "dr2_sup_m2"), 2), "08_numeros: dr2_sup_m2 (en puntos)")
dato(19, "dr2_barrio_pm2", coma_ar(100 * g(N8, "dr2_bar_m2"), 1), "08_numeros: dr2_bar_m2 (en puntos)")
dato(19, "pm2_dentro_barrios_pct", varianza$`Dentro de cada barrio (%)`[varianza$Variable == "Precio por m²"],
     "04_varianza_barrios")
dato(19, "r2_prima_latitud", coma_ar(r_lat^2, 2), "13_numeros: r2_prima_lat")

numeros_diapo <- bind_rows(filas) |> arrange(diapositiva)
stopifnot(!anyNA(numeros_diapo$valor), !any(duplicated(numeros_diapo[c("diapositiva", "clave")])))
write_csv(numeros_diapo, "output/tablas/13_numeros_diapositivas.csv")
cat("Números de la presentación guardados:", nrow(numeros_diapo), "valores en",
    n_distinct(numeros_diapo$diapositiva), "diapositivas (13_numeros_diapositivas.csv)\n")
cat("Correlación prima-latitud:", coma_ar(r_lat, 3), " sin", pm, ":", coma_ar(r_lat_sin, 3),
    " r²:", coma_ar(r_lat^2, 3), "\n")
