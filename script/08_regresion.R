# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 2: métodos estadísticos y cuantitativos
# Script 08: regresión lineal (sección 3 de la consigna)
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: input/base_analisis.csv (con las marcas que agrega el script 06)
# Salida : output/tablas/08_regresion.txt, output/tablas/08_*.csv,
#          output/graficos/g15_*.png, output/tablas/t10 a t15
#
# Método: clase 18, integrador de Properati. Seguimos su recorrido pero no su
# modelo: la base de la clase tiene precio, m², ambientes y barrio, y la
# nuestra además baños, antigüedad, distancia al centro, quién publica y
# latitud.
#
# Lo que ordena el script:
#   1. La muestra, con el N calculado y no escrito a mano.
#   2. Los cuatro pasos y el modelo con latitud.
#   3. Cuánto se mueve el coeficiente de la superficie en cada paso y por qué:
#      el sesgo por variable omitida de la clase 18.
#   4. La identidad entre las dos lecturas y el aporte de cada bloque.
#   5. Los supuestos.
#
# Errores robustos HC1 en todo, como indica la clase para corte transversal.
# Las primas de las dummies siempre con (exp(β) − 1) × 100, nunca con 100 × β.
# =============================================================================

library(tidyverse)
library(lmtest)     # coeftest(), waldtest(), bptest(), resettest()
library(sandwich)   # vcovHC()
library(car)        # vif()
source("script/00_tema.R")

base <- read_csv("input/base_analisis.csv", show_col_types = FALSE)

sink("output/tablas/08_regresion.txt", split = TRUE)
options(width = 140)


# =============================================================================
# 1. LA MUESTRA
# =============================================================================
# Dos recortes sobre la base de análisis, y nada más:
#   - los 40 avisos que no son departamentos (script 06);
#   - los barrios con menos de 100 avisos. La clase usa 300 porque su base es
#     más chica; con la nuestra, 300 dejaría 29 barrios y tiraría el 8%.
# Los dos recortes no se superponen y el orden en que se aplican no cambia el
# resultado. Ninguna variable de los modelos tiene faltantes, así que todos los
# modelos usan exactamente la misma muestra, condición de la clase para poder
# compararlos.

cat("#### 1. LA MUESTRA ####\n\n")

n_base <- nrow(base)
barrios_chicos <- base |> count(barrio_nom) |> filter(n < 100) |> arrange(desc(n))
n_no_depto <- sum(base$no_es_depto)
n_chicos   <- sum(barrios_chicos$n)

d <- base |>
  filter(!no_es_depto, !barrio_nom %in% barrios_chicos$barrio_nom) |>
  mutate(
    barrio     = relevel(factor(barrio_nom), ref = "Balvanera"),
    antiguedad = factor(antiguedad, levels = c("A estrenar", "Hasta 20 años",
                                               "20 a 50 años", "Más de 50 años")),
    publicado_por = factor(publicado_por, levels = c("Particular", "Inmobiliaria")),
    lp   = log(precio_usd),
    lpm2 = log(precio_m2_usd),
    lm2  = log(metros_cubiertos)
  )

n_mod <- nrow(d)
stopifnot(n_mod == n_base - n_no_depto - n_chicos,
          sum(base$no_es_depto & base$barrio_nom %in% barrios_chicos$barrio_nom) == 0)

cat("Base de análisis              :", num_ar(n_base), "\n")
cat("  menos no departamentos      :", num_ar(n_no_depto), "\n")
cat("  menos barrios con < 100     :", num_ar(n_chicos), "\n")
cat("Muestra de los modelos        :", num_ar(n_mod), "avisos,",
    n_distinct(d$barrio), "barrios\n\n")
print(barrios_chicos)

# El pie de la tabla de regresión sale de acá.
pie_n <- paste0(
  "N = ", num_ar(n_mod), " avisos de los ", n_distinct(d$barrio),
  " barrios con al menos 100 avisos. La diferencia con los ", num_ar(n_base),
  " de la base de análisis son ", num_ar(n_no_depto),
  " avisos que no son departamentos y ", num_ar(n_chicos), " de los ",
  nrow(barrios_chicos), " barrios con menos de 100 avisos (",
  paste(barrios_chicos$barrio_nom, collapse = ", "), ").")


# =============================================================================
# 2. LOS CUATRO PASOS Y EL MODELO CON LATITUD
# =============================================================================
# Se agrega un bloque por paso, para ver cuánto se mueve el coeficiente de la
# superficie cada vez (la tabla "solo educación / con controles" de la clase).
# La distancia al centro va en kilómetros: es la misma variable que
# dist_centro_m, pero el coeficiente queda en una unidad que se puede leer.
#
# El modelo con latitud reemplaza las 41 dummies de barrio por una sola
# variable. lat y barrio no pueden ir juntas: con las dos el VIF del barrio da
# 655, porque cada barrio ocupa una franja de latitudes.

cat("\n\n#### 2. LOS CUATRO PASOS ####\n\n")

f1 <- lp ~ lm2
f2 <- lp ~ lm2 + ambientes + banos + antiguedad
f3 <- update(f2, . ~ . + barrio)
f4 <- update(f3, . ~ . + dist_centro_km + publicado_por)
f_lat <- lp ~ lm2 + ambientes + banos + antiguedad + dist_centro_km +
  publicado_por + lat

m1 <- lm(f1, d); m2 <- lm(f2, d); m3 <- lm(f3, d); m4 <- lm(f4, d)
m_lat <- lm(f_lat, d)
modelos <- list(m1, m2, m3, m4, m_lat)
stopifnot(all(sapply(modelos, nobs) == n_mod))

vc <- function(m) vcovHC(m, type = "HC1")
b_m2 <- function(m) unname(coef(m)["lm2"])

pasos <- tibble(
  paso = c("1. Superficie", "2. + características", "3. + barrio",
           "4. + entorno", "Latitud en vez de barrio"),
  beta_lm2 = sapply(modelos, b_m2),
  r2       = sapply(modelos, \(m) summary(m)$r.squared),
  r2_aj    = sapply(modelos, \(m) summary(m)$adj.r.squared),
  variables = sapply(modelos, \(m) length(coef(m)) - 1),
  n        = sapply(modelos, nobs)
)
print(pasos |> mutate(across(where(is.double), ~ round(.x, 4))))
write_csv(pasos, "output/tablas/08_pasos.csv")

cat("\nModelo 4 con errores robustos HC1 (sin las dummies de barrio):\n")
ct4 <- coeftest(m4, vcov = vc(m4))
print(ct4[!str_starts(rownames(ct4), "barrio"), ])

# Lectura de la elasticidad: un departamento 10% más grande.
cat("\nUn departamento 10% más grande, a igual barrio y características:",
    coma_ar(100 * (1.1^b_m2(m4) - 1), 1), "% más caro\n")

cat("\n--- Modelo con latitud ---\n")
ct_lat <- coeftest(m_lat, vcov = vc(m_lat))
print(ct_lat)
# lat está en grados decimales. Un grado de latitud son 111 km sobre el
# meridiano, así que el efecto de un kilómetro al norte es β / 111.
b_km <- coef(m_lat)["lat"] / 111
cat("Un kilómetro más al norte, a igual distancia al centro:",
    coma_ar(100 * (exp(b_km) - 1), 2), "% más de precio\n")


# =============================================================================
# 3. SESGO POR VARIABLE OMITIDA
# =============================================================================
# El coeficiente de la superficie sube al entrar las características
# (paso 1 -> 2) y baja al entrar el barrio (paso 2 -> 3). La clase 18 lo llama
# sesgo por variable omitida: "cuando un coeficiente cambia mucho al agregar un
# control, el control importaba".
#
# Para saber QUÉ lo mueve usamos la fórmula del sesgo por variable omitida:
#
#   β corto − β largo = Σ  γ_k × δ_k
#
#   γ_k  coeficiente de la variable omitida k en el modelo largo
#   δ_k  coeficiente de log(m²) cuando se regresa la variable k contra log(m²)
#        (y contra los demás controles del modelo corto)
#
# O sea: cuánto pesa la variable en el precio por cuánto se mueve junto con la
# superficie. Tiene una ventaja sobre agregar las variables de a una: el reparto
# no depende del orden, y los aportes suman exacto el cambio total. Las
# entradas de a una igual se reportan, porque son la intuición.

cat("\n\n#### 3. SESGO POR VARIABLE OMITIDA ####\n\n")

# --- 3.1 Paso 1 -> 2: las características -----------------------------------

cat("--- Paso 1 -> 2: cada característica entrando sola ---\n")
de_a_una <- map_dfr(c(ambientes = "ambientes", banos = "banos",
                      antiguedad = "antiguedad"), function(v) {
  m <- lm(as.formula(paste("lp ~ lm2 +", v)), d)
  tibble(variable = v, beta_lm2 = b_m2(m), cambio_solo = b_m2(m) - b_m2(m1))
})
print(de_a_una |> mutate(across(where(is.double), ~ round(.x, 4))))

cat("\n--- Paso 1 -> 2: reparto exacto ---\n")
X2 <- model.matrix(m2)[, -(1:2), drop = FALSE]
reparto <- tibble(
  termino = colnames(X2),
  gamma   = coef(m2)[colnames(X2)],
  delta   = sapply(colnames(X2), \(k) coef(lm(X2[, k] ~ d$lm2))[2])
) |>
  # El signo se da vuelta para leerlo como "cuánto sube β al entrar".
  mutate(aporte = -gamma * delta,
         bloque = if_else(str_starts(termino, "antiguedad"), "antiguedad", termino))
print(reparto |> mutate(across(where(is.double), ~ round(.x, 4))))

aporte_carac <- reparto |> group_by(bloque) |> summarise(aporte = sum(aporte))
stopifnot(abs(sum(aporte_carac$aporte) - (b_m2(m2) - b_m2(m1))) < 1e-10)
cat("\nSuma de los aportes:", round(sum(aporte_carac$aporte), 4),
    "  Cambio de β, paso 1 -> 2:", round(b_m2(m2) - b_m2(m1), 4), "\n")
cat("Correlación con log(m²): ambientes", coma_ar(cor(d$lm2, d$ambientes), 3),
    "  baños", coma_ar(cor(d$lm2, d$banos), 3), "\n")

# Lo más importante de esta sección. El coeficiente negativo de ambientes es
# lo que empuja β hacia arriba en el paso 2, pero ese coeficiente desaparece
# cuando entra el barrio. "A igual superficie, un ambiente más es más barato"
# vale en el paso 2 y NO en el modelo final.
cat("\n--- El coeficiente de ambientes, paso por paso (HC1) ---\n")
amb <- map_dfr(list(`2` = m2, `3` = m3, `4` = m4), function(m) {
  ct <- coeftest(m, vcov = vc(m))["ambientes", ]
  tibble(beta = ct[1], ee = ct[2], t = ct[3], p = ct[4])
}, .id = "paso")
print(amb |> mutate(across(where(is.double), ~ signif(.x, 4))))

# --- 3.2 Paso 2 -> 3: el barrio ---------------------------------------------
# Las 41 dummies son un bloque. Su aporte se mide con la prima de barrio que el
# modelo le asigna a cada aviso: si esa prima sube con la superficie (a igual
# características), omitir el barrio le carga parte de la prima al tamaño.

cat("\n--- Paso 2 -> 3: el barrio ---\n")
gam_b <- coef(m3)[str_starts(names(coef(m3)), "barrio")]
d$prima_barrio <- as.vector(model.matrix(m3)[, names(gam_b)] %*% gam_b)

aporte_barrio <- -coef(lm(update(f2, prima_barrio ~ .), d))["lm2"]
stopifnot(abs(aporte_barrio - (b_m2(m3) - b_m2(m2))) < 1e-10)
aporte_barrio_sin_ctrl <- -coef(lm(prima_barrio ~ lm2, d))["lm2"]
cat("Aporte del barrio al cambio de β:", round(aporte_barrio, 4),
    "  Cambio de β, paso 2 -> 3:", round(b_m2(m3) - b_m2(m2), 4), "\n")
cat("El mismo cálculo sin controlar por características:",
    round(aporte_barrio_sin_ctrl, 4), "\n")

# Barrio por barrio. La superficie "condicional" es log(m²) descontados
# ambientes, baños y antigüedad: lo que el aviso tiene de amplio para su
# cantidad de ambientes.
d$lm2_cond <- resid(lm(lm2 ~ ambientes + banos + antiguedad, d))

por_barrio <- d |>
  group_by(barrio_nom) |>
  summarise(avisos = n(), m2_promedio = mean(metros_cubiertos),
            m2_mediano = median(metros_cubiertos),
            lm2_cond = mean(lm2_cond), prima = first(prima_barrio),
            .groups = "drop") |>
  mutate(prima_pct = 100 * (exp(prima) - 1)) |>
  arrange(desc(prima))

# Todas las correlaciones se calculan sobre lo que muestra el gráfico: un punto
# por barrio, sin ponderar, superficie promedio en m² contra prima en %.
cor_graf     <- cor(por_barrio$m2_promedio, por_barrio$prima_pct)
cor_spearman <- cor(por_barrio$m2_promedio, por_barrio$prima_pct, method = "spearman")
cor_sin_pm   <- with(filter(por_barrio, barrio_nom != "Puerto Madero"),
                     cor(m2_promedio, prima_pct))
cor_sin_top3 <- with(filter(por_barrio, !barrio_nom %in%
                              c("Puerto Madero", "Retiro", "Recoleta")),
                     cor(m2_promedio, prima_pct))
cor_cond     <- cor(por_barrio$lm2_cond, por_barrio$prima_pct)

# Dónde está la relación. Partimos los 42 barrios en dos mitades según su
# prima: los 21 más caros y los 21 más baratos. Ojo al leerlo: cortar por la
# prima achica su rango dentro de cada mitad, y eso por sí solo baja las dos
# correlaciones. Por eso importa el contraste entre mitades, no el nivel.
mitad_alta <- slice_max(por_barrio, prima, n = 21)
mitad_baja <- slice_min(por_barrio, prima, n = 21)
cor_alta <- cor(mitad_alta$m2_promedio, mitad_alta$prima_pct)
cor_baja <- cor(mitad_baja$m2_promedio, mitad_baja$prima_pct)

# El mismo contraste por terciles, para verificar que no es efecto del corte en
# la mediana: también lo probamos con cortes 14/28, 28/14 y 10/32 y con otras
# medidas de superficie, y se sostiene. La frase segura es la de los terciles.
tercil <- por_barrio |> mutate(tercil = ntile(-prima, 3)) |>
  group_by(tercil) |> summarise(r = cor(m2_promedio, prima_pct), .groups = "drop")
cor_tercil_caro   <- tercil$r[tercil$tercil == 1]
cor_tercil_medio  <- tercil$r[tercil$tercil == 2]
cor_tercil_barato <- tercil$r[tercil$tercil == 3]

cat("\nCorrelación entre la superficie promedio del barrio y su prima:\n")
cat("  Pearson (la del gráfico)           :", coma_ar(cor_graf, 2), "\n")
cat("  Spearman (por rangos)              :", coma_ar(cor_spearman, 2), "\n")
cat("  Pearson sin Puerto Madero          :", coma_ar(cor_sin_pm, 2), "\n")
cat("  Pearson sin Puerto Madero, Retiro y Recoleta:", coma_ar(cor_sin_top3, 2), "\n")
cat("  Pearson con la superficie condicional a ambientes, baños y antigüedad:",
    coma_ar(cor_cond, 2), "\n")
cat("  Pearson en los 21 barrios de mayor prima:", coma_ar(cor_alta, 2), "\n")
cat("  Pearson en los 21 barrios de menor prima:", coma_ar(cor_baja, 2), "\n")
cat("  Por terciles de prima: caro", coma_ar(cor_tercil_caro, 2), " medio",
    coma_ar(cor_tercil_medio, 2), " barato", coma_ar(cor_tercil_barato, 2), "\n")
cat("\nParte de la varianza de log(m²) que está entre barrios:",
    coma_ar(100 * summary(lm(lm2 ~ barrio, d))$r.squared, 1), "%\n\n")
print(por_barrio |> mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)

sesgo <- bind_rows(
  de_a_una |> select(bloque = variable, cambio_solo) |>
    left_join(aporte_carac, by = "bloque"),
  tibble(bloque = "barrio", cambio_solo = b_m2(m3) - b_m2(m2),
         aporte = aporte_barrio)
)
write_csv(sesgo, "output/tablas/08_sesgo_omitida.csv")
write_csv(por_barrio, "output/tablas/08_prima_superficie_barrio.csv")


# =============================================================================
# 4. LA IDENTIDAD ENTRE LAS DOS LECTURAS Y EL APORTE DE CADA BLOQUE
# =============================================================================
# Como precio_m2 = precio / m², en logaritmos log(precio/m²) = log(precio) −
# log(m²). Con log(m²) y los mismos controles a la derecha, las dos regresiones
# son la misma ecuación: el coeficiente de la superficie baja exactamente 1 y
# todo lo demás queda igual. Por eso cada bloque reduce la suma de cuadrados
# de los residuos en lo mismo en las dos, y lo único que cambia es la suma de
# cuadrados total por la que se divide.

cat("\n\n#### 4. IDENTIDAD Y APORTE DE CADA BLOQUE ####\n\n")

m4p <- lm(update(f4, lpm2 ~ .), d)

no_lm2 <- names(coef(m4)) != "lm2"
identidad <- c(
  dif_coef_resto  = max(abs(coef(m4)[no_lm2] - coef(m4p)[no_lm2])),
  dif_beta_lm2    = unname(b_m2(m4) - b_m2(m4p)),
  dif_residuos    = max(abs(resid(m4) - resid(m4p))),
  dif_ee_hc1      = max(abs(sqrt(diag(vc(m4)))[no_lm2] - sqrt(diag(vc(m4p)))[no_lm2]))
)
print(signif(identidad, 3))
stopifnot(abs(identidad["dif_beta_lm2"] - 1) < 1e-8, identidad["dif_residuos"] < 1e-8)

scr <- function(m) sum(resid(m)^2)
sct <- function(y) sum((y - mean(y))^2)

bloques <- list(
  "Superficie"                 = "lm2",
  "Barrio (41 dummies)"        = "barrio",
  "Características (ambientes, baños, antigüedad)" = c("ambientes", "banos", "antiguedad")
)

aporte <- map_dfr(names(bloques), function(b) {
  quitar <- paste("-", bloques[[b]], collapse = " ")
  map_dfr(list(precio = m4, precio_m2 = m4p), function(m) {
    m_sin <- update(m, as.formula(paste(". ~ .", quitar)))
    w <- waldtest(m_sin, m, vcov = vc(m), test = "F")
    y <- model.response(model.frame(m))
    tibble(bloque = b,
           caida_scr  = scr(m_sin) - scr(m),
           sct        = sct(y),
           delta_r2   = (scr(m_sin) - scr(m)) / sct(y),
           r2_parcial = (scr(m_sin) - scr(m)) / scr(m_sin),
           wald_F = w$F[2], gl1 = abs(w$Df[2]), gl2 = df.residual(m),
           p = w$`Pr(>F)`[2])
  }, .id = "dependiente")
})
print(aporte |> mutate(across(where(is.double), ~ signif(.x, 4))), width = Inf)
write_csv(aporte, "output/tablas/08_aporte_bloques.csv")

cat("\nR² del modelo completo: log(precio)", coma_ar(summary(m4)$r.squared, 3),
    "  log(precio/m²)", coma_ar(summary(m4p)$r.squared, 3), "\n")

# --- La hipótesis nula, en términos del problema ------------------------------
# H0: una vez que se controla por superficie y características de la vivienda,
#     el barrio no agrega nada a la explicación del precio (las 41 primas son
#     cero a la vez: todos los barrios valen como Balvanera).
# H1: al menos un barrio se aparta de Balvanera.
# Es exactamente la comparación del paso 2 contra el paso 3.
cat("\n--- H0: el barrio no agrega nada, controlando superficie y características ---\n")
w_h0 <- waldtest(m2, m3, vcov = vc(m3), test = "F")
print(w_h0)


# =============================================================================
# 5. SUPUESTOS
# =============================================================================
# Sobre el modelo completo. La lógica de todos los tests, como dice la clase:
# H0 = "no hay problema"; con 38 mil observaciones casi todo rechaza, así que
# importa más qué se hace después que el rechazo en sí.

cat("\n\n#### 5. SUPUESTOS ####\n\n")

reset <- resettest(m4)
bp    <- bptest(m4)
print(reset); print(bp)

# Cook: no se sacan avisos porque molestan. Se reporta el modelo con y sin.
cook <- cooks.distance(m4)
infl <- cook > 4 / n_mod
m4_sin <- update(m4, data = d[!infl, ])
cat("Avisos con distancia de Cook > 4/n:", num_ar(sum(infl)),
    sprintf("(%s%%)", coma_ar(100 * mean(infl))), "\n")
cat("De Puerto Madero:", sum(infl & d$barrio_nom == "Puerto Madero"), "de",
    sum(d$barrio_nom == "Puerto Madero"), "\n")

terminos_cook <- c("lm2", "ambientes", "banos", "dist_centro_km",
                   "publicado_porInmobiliaria", "barrioPuerto Madero", "barrioPalermo")
cook_tabla <- tibble(termino = terminos_cook,
                     con = coef(m4)[terminos_cook], sin = coef(m4_sin)[terminos_cook])
print(cook_tabla |> mutate(across(where(is.double), ~ round(.x, 4))))
cat("R² con y sin:", coma_ar(summary(m4)$r.squared, 3),
    coma_ar(summary(m4_sin)$r.squared, 3), "\n")

# Las primas de barrio también se mueven sin los influyentes. Lo que importa es
# la dirección: si la brecha entre barrios se achicara, el resultado dependería
# de los extremos; si crece, no.
primas_de <- function(m) c(Balvanera = 0, coef(m)[str_starts(names(coef(m)), "barrio")])
brecha_de <- function(m) exp(max(primas_de(m)) - min(primas_de(m)))
cat("Prima de Puerto Madero con y sin influyentes:",
    coma_ar(100 * (exp(coef(m4)["barrioPuerto Madero"]) - 1), 1), "% ->",
    coma_ar(100 * (exp(coef(m4_sin)["barrioPuerto Madero"]) - 1), 1), "%\n")
cat("Brecha entre los barrios extremos con y sin influyentes:",
    coma_ar(brecha_de(m4), 2), "->", coma_ar(brecha_de(m4_sin), 2), "\n")

# Winsorizar: la alternativa de t03 que no elegimos. Conserva todos los avisos
# pero reemplaza los precios por encima del p99 por el valor del p99. Lo
# probamos para poder decir por qué no: cambia valores genuinos por uno
# inventado, y en logaritmo casi no mueve los resultados, así que no hace falta.
cat("\n--- Sensibilidad: precio winsorizado en el p99 ---\n")
p99 <- quantile(d$precio_usd, 0.99)
d$lp_w <- log(pmin(d$precio_usd, p99))
m4_w <- lm(update(f4, lp_w ~ .), d)
prima_pm <- function(m) 100 * (exp(coef(m)["barrioPuerto Madero"]) - 1)
cat("p99: US$", num_ar(p99), " avisos por encima:", sum(d$precio_usd > p99),
    " de Puerto Madero:", sum(d$precio_usd > p99 & d$barrio_nom == "Puerto Madero"), "de",
    sum(d$barrio_nom == "Puerto Madero"), "\n")
cat("Prima de Puerto Madero: original", coma_ar(prima_pm(m4), 1), "% winsorizado",
    coma_ar(prima_pm(m4_w), 1), "%\n")
cat("Elasticidad: original", coma_ar(b_m2(m4), 3), " winsorizado", coma_ar(b_m2(m4_w), 3), "\n")

# VIF. Con un factor de 41 niveles el VIF común no sirve: car::vif() devuelve
# el GVIF, y GVIF^(1/(2 gl)) es la versión comparable entre variables con
# distinta cantidad de columnas. Su umbral equivalente a VIF = 10 es √10 ≈ 3,16.
cat("\n--- GVIF ---\n")
gvif <- as_tibble(vif(m4), rownames = "variable") |>
  rename(gvif = GVIF, gl = Df, gvif_aj = `GVIF^(1/(2*Df))`)
print(gvif |> mutate(across(where(is.double), ~ round(.x, 2))))

# Por qué salta la distancia y no el barrio, si se superponen. La distancia es
# casi predecible desde el resto del modelo: una variable sola se deja explicar
# por 41 dummies. Al revés no: 41 dummies no se dejan explicar por una sola
# variable, y el GVIF ajustado del barrio reparte su superposición entre sus 41
# columnas. Se mide con la regresión auxiliar de la distancia contra todo lo
# demás; su R² es 1 − 1/VIF.
r2_aux_dist <- summary(lm(dist_centro_km ~ lm2 + ambientes + banos + antiguedad + barrio +
                            publicado_por, d))$r.squared
r2_dist_barrio <- summary(lm(dist_centro_km ~ barrio, d))$r.squared
stopifnot(abs(r2_aux_dist - (1 - 1 / gvif$gvif[gvif$variable == "dist_centro_km"])) < 1e-6)
cat("El resto del modelo explica el", coma_ar(100 * r2_aux_dist, 1), "% de la distancia al Obelisco",
    "(el barrio solo, el", coma_ar(100 * r2_dist_barrio, 1), "%)\n")

# --- RESET: ¿no linealidad en las variables que sí están? ---------------------
# Con 38 mil observaciones el RESET rechaza casi siempre. "Faltan variables que
# la base no tiene" no es la única lectura: también puede faltar curvatura en
# la superficie (la clase 18 encuentra exactamente eso en su integrador). Se
# agrega log(m²)² al modelo completo y se mira si cambian las tres conclusiones:
# la elasticidad, el test del barrio y el aporte de cada bloque.
cat("\n--- RESET: el modelo completo con log(m²)² ---\n")
m4q  <- update(m4, . ~ . + I(lm2^2))
m4qp <- update(m4p, . ~ . + I(lm2^2))
print(coeftest(m4q, vcov = vc(m4q))[c("lm2", "I(lm2^2)"), ])
reset_q <- resettest(m4q)
print(reset_q)

# Con el cuadrado, la elasticidad depende del tamaño: β1 + 2 β2 log(m²).
elast_q <- function(m2) coef(m4q)["lm2"] + 2 * coef(m4q)["I(lm2^2)"] * log(m2)
elasticidades <- tibble(m2 = c(35, 50, 70, 100, 150),
                        elasticidad = map_dbl(m2, elast_q))
cat("\nElasticidad según el tamaño (sin cuadrado: ", coma_ar(b_m2(m4), 3), "):\n", sep = "")
print(elasticidades |> mutate(elasticidad = round(elasticidad, 3)))
cat("Avisos entre 35 y 150 m²:", coma_ar(100 * mean(between(d$metros_cubiertos, 35, 150))), "%\n")

# El test del barrio, con el cuadrado en los dos modelos.
m2q <- update(m2, . ~ . + I(lm2^2)); m3q <- update(m3, . ~ . + I(lm2^2))
w_h0_q <- waldtest(m2q, m3q, vcov = vc(m3q), test = "F")
cat("\nH0 del barrio con el cuadrado: F =", coma_ar(w_h0_q$F[2], 2),
    " (sin cuadrado:", coma_ar(w_h0$F[2], 2), ")\n")

# El aporte de cada bloque, con el cuadrado (la superficie son los dos términos).
aporte_q <- map_dfr(list(`Superficie` = "- lm2 - I(lm2^2)", `Barrio` = "- barrio",
                         `Características` = "- ambientes - banos - antiguedad"),
  function(q) map_dfr(list(precio = m4q, precio_m2 = m4qp), function(m) {
    m_sin <- update(m, as.formula(paste(". ~ .", q)))
    y <- model.response(model.frame(m))
    tibble(delta_r2 = (scr(m_sin) - scr(m)) / sct(y))
  }, .id = "dependiente"), .id = "bloque")
cat("\nΔR² de cada bloque con el cuadrado:\n")
print(aporte_q |> pivot_wider(names_from = dependiente, values_from = delta_r2) |>
        mutate(across(where(is.double), ~ round(100 * .x, 2))))
cat("R² con el cuadrado:", coma_ar(summary(m4q)$r.squared, 4), " sin:",
    coma_ar(summary(m4)$r.squared, 4), "\n")

# Los números que van a los textos de la presentación (script 10).
ap <- function(b, dep, col = "delta_r2") aporte[[col]][str_starts(aporte$bloque, b) &
                                                       aporte$dependiente == dep]
apq <- function(b, dep) aporte_q$delta_r2[aporte_q$bloque == b & aporte_q$dependiente == dep]
sg <- function(b, col) sesgo[[col]][sesgo$bloque == b]
# Las primas extremas del modelo completo, con Balvanera (prima 0) incluida.
# exp(β máx − β mín) es cuántas veces más caro es el barrio de prima más alta
# que el de prima más baja, a igual superficie, características, distancia al
# centro y quién publica. Se compara con lo que da el kmeans dentro de cada tipo.
b_barrio <- c(Balvanera = 0, setNames(coef(m4)[str_starts(names(coef(m4)), "barrio")],
                                      str_remove(names(coef(m4))[str_starts(names(coef(m4)), "barrio")],
                                                 "^barrio")))
cat("\nBrecha entre el barrio de prima más alta (", names(which.max(b_barrio)), ") y el de prima ",
    "más baja (", names(which.min(b_barrio)), "), modelo completo: ",
    coma_ar(exp(max(b_barrio) - min(b_barrio)), 2), " veces\n", sep = "")
guardar_numeros(
  "08_numeros.csv",
  n_mod = n_mod, n_barrios = n_distinct(d$barrio), n_no_depto = n_no_depto,
  n_chicos = n_chicos, n_barrios_chicos = nrow(barrios_chicos),
  b1 = b_m2(m1), b2 = b_m2(m2), b3 = b_m2(m3), b4 = b_m2(m4), b_lat = b_m2(m_lat),
  r2_1 = pasos$r2[1], r2_2 = pasos$r2[2], r2_3 = pasos$r2[3], r2_4 = pasos$r2[4],
  r2_lat = pasos$r2[5], r2aj_3 = pasos$r2_aj[3], r2aj_4 = pasos$r2_aj[4],
  r2_4p = summary(m4p)$r.squared,
  pct_10mas = 100 * (1.1^b_m2(m4) - 1),
  lat_coef = coef(m_lat)["lat"], lat_km_pct = 100 * (exp(b_km) - 1),
  amb_solo = sg("ambientes", "cambio_solo"), banos_solo = sg("banos", "cambio_solo"),
  antig_solo = sg("antiguedad", "cambio_solo"),
  amb_aporte = sg("ambientes", "aporte"), banos_aporte = sg("banos", "aporte"),
  antig_aporte = sg("antiguedad", "aporte"),
  barrio_aporte = aporte_barrio, barrio_aporte_sin_ctrl = aporte_barrio_sin_ctrl,
  cor_amb_m2 = cor(d$lm2, d$ambientes), cor_banos_m2 = cor(d$lm2, d$banos),
  amb_2 = amb$beta[amb$paso == "2"], amb_3 = amb$beta[amb$paso == "3"],
  amb_p3 = amb$p[amb$paso == "3"], amb_lat = coef(m_lat)["ambientes"],
  banos_2 = coef(m2)["banos"],
  cor_graf = cor_graf, cor_cond = cor_cond, cor_sin_pm = cor_sin_pm,
  cor_alta = cor_alta, cor_baja = cor_baja,
  cor_tercil_caro = cor_tercil_caro, cor_tercil_medio = cor_tercil_medio,
  cor_tercil_barato = cor_tercil_barato,
  dr2_sup_p = ap("Superficie", "precio"), dr2_sup_m2 = ap("Superficie", "precio_m2"),
  dr2_bar_p = ap("Barrio", "precio"), dr2_bar_m2 = ap("Barrio", "precio_m2"),
  dr2_car_p = ap("Caracter", "precio"), dr2_car_m2 = ap("Caracter", "precio_m2"),
  r2p_bar = ap("Barrio", "precio", "r2_parcial"), r2p_car = ap("Caracter", "precio", "r2_parcial"),
  r2p_sup_p = ap("Superficie", "precio", "r2_parcial"),
  scr_bar = ap("Barrio", "precio", "caida_scr"),
  sct_p = ap("Barrio", "precio", "sct"), sct_m2 = ap("Barrio", "precio_m2", "sct"),
  F_sup_m2 = ap("Superficie", "precio_m2", "wald_F"), p_sup_m2 = ap("Superficie", "precio_m2", "p"),
  F_sup_m2_gl1 = ap("Superficie", "precio_m2", "gl1"), F_sup_m2_gl2 = ap("Superficie", "precio_m2", "gl2"),
  F_h0 = w_h0$F[2], gl1_h0 = abs(w_h0$Df[2]), gl2_h0 = w_h0$Res.Df[2],
  reset = reset$statistic, reset_gl2 = reset$parameter[2], bp = bp$statistic,
  bp_gl = bp$parameter,
  cook_n = sum(infl), cook_pct = 100 * mean(infl), b4_sin = b_m2(m4_sin),
  pm_con = 100 * (exp(coef(m4)["barrioPuerto Madero"]) - 1),
  pm_sin = 100 * (exp(coef(m4_sin)["barrioPuerto Madero"]) - 1),
  brecha_con = brecha_de(m4), brecha_sin = brecha_de(m4_sin),
  r2_sin = summary(m4_sin)$r.squared,
  gvif_max = max(gvif$gvif_aj), gvif_barrio = gvif$gvif_aj[gvif$variable == "barrio"],
  r2_aux_dist = r2_aux_dist, r2_dist_barrio = r2_dist_barrio,
  reset_q = reset_q$statistic, elast_35 = elast_q(35), elast_150 = elast_q(150),
  F_h0_q = w_h0_q$F[2], dr2_sup_m2_q = apq("Superficie", "precio_m2"),
  w_p99 = p99, w_sobre_p99 = sum(d$precio_usd > p99),
  w_pm_sobre_p99 = sum(d$precio_usd > p99 & d$barrio_nom == "Puerto Madero"),
  w_pm_total = sum(d$barrio_nom == "Puerto Madero"),
  w_prima_pm = prima_pm(m4), w_prima_pm_w = prima_pm(m4_w),
  w_b4 = b_m2(m4_w),
  prima_max_barrio = names(which.max(b_barrio)),
  prima_max_pct = 100 * (exp(max(b_barrio)) - 1),
  prima_min_barrio = names(which.min(b_barrio)),
  prima_min_pct = 100 * (exp(min(b_barrio)) - 1),
  brecha_extremos = exp(max(b_barrio) - min(b_barrio)),
  # La misma brecha en el paso 3, que controla solo las características de la
  # vivienda. Es el escalón intermedio de la secuencia de la síntesis: crudo,
  # paso 3, modelo completo, dentro de cada tipo del kmeans.
  p3_max_barrio = por_barrio$barrio_nom[which.max(por_barrio$prima)],
  p3_min_barrio = por_barrio$barrio_nom[which.min(por_barrio$prima)],
  brecha_p3 = exp(max(por_barrio$prima) - min(por_barrio$prima)),
  dif_coef = identidad["dif_coef_resto"]
)

supuestos <- tibble(
  test = c("RESET", "Breusch-Pagan"),
  estadistico = c(reset$statistic, bp$statistic),
  gl = c(paste(reset$parameter, collapse = " y "), as.character(bp$parameter)),
  p = c(reset$p.value, bp$p.value)
)
write_csv(supuestos, "output/tablas/08_supuestos.csv")

sink()
cat("\nDiagnóstico guardado en output/tablas/08_regresion.txt\n")


# =============================================================================
# 6. GRÁFICO
# =============================================================================

# --- G15. La prima del barrio y la superficie --------------------------------
# Un punto por barrio. Eje x: superficie promedio de sus avisos. Eje y: prima
# del barrio sobre Balvanera en el paso 3, en % exacto. En terracota, los tres
# barrios que arrastran la correlación. En gris, Palermo y Belgrano, que son
# los dos puntos más grandes y alguien va a preguntar cuáles son, y la base.

etiquetados <- c("Puerto Madero", "Retiro", "Recoleta")
en_gris     <- c("Palermo", "Belgrano")

g15 <- ggplot(por_barrio, aes(m2_promedio, prima_pct)) +
  geom_hline(yintercept = 0, color = colores$grilla, linewidth = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = colores$acento2, linewidth = 0.7, linetype = "22") +
  geom_point(aes(size = avisos),
             color = if_else(por_barrio$barrio_nom %in% etiquetados,
                             colores$acento, colores$apagado),
             alpha = 0.85) +
  # Recoleta va abajo a la derecha de su punto: arriba a la izquierda lo pisaba.
  geom_text(data = filter(por_barrio, barrio_nom %in% etiquetados) |>
              mutate(hj = if_else(barrio_nom == "Recoleta", -0.25, 1.15),
                     vj = if_else(barrio_nom == "Recoleta", 1.4, -0.6)),
            aes(label = barrio_nom, hjust = hj, vjust = vj), size = 3.3,
            family = fuente_cuerpo, color = colores$acento, fontface = "bold") +
  geom_text(data = filter(por_barrio, barrio_nom %in% en_gris),
            aes(label = barrio_nom), hjust = -0.25, vjust = 0.4, size = 3,
            family = fuente_cuerpo, color = colores$secundario) +
  geom_text(data = filter(por_barrio, barrio_nom == "Balvanera"),
            aes(label = "Balvanera (base)"), hjust = -0.15, vjust = 1.6, size = 2.9,
            family = fuente_cuerpo, color = colores$secundario) +
  scale_size_area(max_size = 9, guide = "none") +
  scale_x_continuous(labels = \(x) paste(num_ar(x), "m²")) +
  scale_y_continuous(labels = \(x) paste0(if_else(x > 0, "+", ""), num_ar(x), "%")) +
  labs(x = "Superficie promedio de los avisos del barrio",
       y = "Prima del barrio sobre Balvanera",
       caption = pie(paste0(
         fuente_pie, ". Un punto por barrio, de tamaño proporcional a sus avisos. ",
         "Prima: (exp(β) − 1) × 100, a igual superficie, ambientes, baños y antigüedad. ",
         "Correlación de lo que muestra el gráfico: ", coma_ar(cor_graf, 2),
         "; sin Puerto Madero: ", coma_ar(cor_sin_pm, 2),
         # Por terciles, como el texto de la diapositiva: es la frase segura,
         # porque se sostiene con cualquier corte (decisiones.md).
         "; por terciles de prima: ", coma_ar(cor_tercil_caro, 2), " en el caro, ",
         coma_ar(cor_tercil_medio, 2), " en el medio y ", coma_ar(cor_tercil_barato, 2),
         " en el barato.")))

guardar(g15, "g15_prima_superficie_barrio.png", ancho = 10, alto = 5.4)


# =============================================================================
# 7. TABLAS COMO IMAGEN
# =============================================================================

fmt_p <- function(p) if_else(p < 2.2e-16, "< 2,2 × 10⁻¹⁶",
                             if_else(p < 0.001, "< 0,001", coma_ar(p, 3)))
estrellas <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                                   p < 0.05 ~ "*", TRUE ~ "")

# --- T10. Los cuatro pasos y el modelo con latitud ---------------------------
filas_t10 <- c(
  lm2 = "Log superficie",
  ambientes = "Ambientes",
  banos = "Baños",
  `antiguedadHasta 20 años` = "Hasta 20 años (base: a estrenar)",
  `antiguedad20 a 50 años` = "20 a 50 años",
  `antiguedadMás de 50 años` = "Más de 50 años",
  dist_centro_km = "Distancia al Obelisco (km)",
  publicado_porInmobiliaria = "Publica una inmobiliaria",
  lat = "Latitud (grados)"
)
cols_t10 <- c("1. Superficie", "2. + Características", "3. + Barrio",
              "4. + Entorno", "Latitud en vez de barrio")

# Tres decimales, salvo los valores que con tres quedarían en "0,000".
num_coef <- function(x) coma_ar(x, if (abs(x) < 0.0005) 4 else 3)

celda <- function(m, termino) {
  ct <- coeftest(m, vcov = vc(m))
  if (!termino %in% rownames(ct)) return(c("", ""))
  c(paste0(num_coef(ct[termino, 1]), estrellas(ct[termino, 4])),
    paste0("(", num_coef(ct[termino, 2]), ")"))
}

cuerpo_t10 <- map_dfr(names(filas_t10), function(tm) {
  vals <- map(modelos, \(m) celda(m, tm))
  bind_rows(
    tibble(Variable = filas_t10[[tm]], !!!set_names(map(vals, 1), cols_t10)),
    tibble(Variable = "", !!!set_names(map(vals, 2), cols_t10))
  )
})
pie_t10 <- bind_rows(
  tibble(Variable = "Barrio (41 dummies, base Balvanera)",
         !!!set_names(as.list(c("No", "No", "Sí", "Sí", "No")), cols_t10)),
  # Cuatro decimales: con tres, R² y R² ajustado salen idénticos y la fila no
  # contesta lo que tiene que contestar (que 41 dummies no inflan el R²).
  tibble(Variable = "R²", !!!set_names(as.list(coma_ar(pasos$r2, 4)), cols_t10)),
  tibble(Variable = "R² ajustado", !!!set_names(as.list(coma_ar(pasos$r2_aj, 4)), cols_t10)),
  tibble(Variable = "Avisos", !!!set_names(as.list(num_ar(pasos$n)), cols_t10))
)
t10 <- bind_rows(cuerpo_t10, pie_t10)

guardar_tabla(tabla_png(t10, anchos = c(3, 1.3, 1.5, 1.3, 1.3, 1.6),
                        size = 2.9, alto_fila = 0.3,
                        destacar = 1,
                        pie_tabla = paste0(
                          "Variable dependiente: log(precio publicado). Errores estándar robustos (HC1) entre paréntesis. ",
                          "* p < 0,05; ** p < 0,01; *** p < 0,001. ", pie_n)),
              "t10_regresion_pasos.png", ancho = 11)

# --- T11. El sesgo por variable omitida --------------------------------------
etiq_bloque <- c(ambientes = "Ambientes", banos = "Baños",
                 antiguedad = "Antigüedad", barrio = "Barrio (41 dummies)")
# unname(): sin esto c() arma nombres como "ambientes.ambientes" y la tabla
# queda vacía en esas celdas.
gam_txt <- c(ambientes = coma_ar(unname(coef(m2)["ambientes"]), 3),
             banos     = coma_ar(unname(coef(m2)["banos"]), 3),
             antiguedad = paste0(coma_ar(min(coef(m2)[str_starts(names(coef(m2)), "antig")]), 2),
                                 " a ", coma_ar(max(coef(m2)[str_starts(names(coef(m2)), "antig")]), 2)),
             barrio    = paste0(coma_ar(min(gam_b), 2), " a +", coma_ar(max(gam_b), 2)))
signo <- function(x, dec = 3) paste0(if_else(x > 0, "+", "-"), coma_ar(abs(x), dec))

# Cuatro decimales, todos tal como salen del cálculo, sin ajustar ninguno para
# que sume. Los aportes suman el cambio total (0,0855), pero la resta de los
# coeficientes mostrados puede diferir en el último decimal (1,1238 − 1,0384 =
# 0,0854) porque cada coeficiente se redondea por su lado. Lo dice el pie.
dec_t11 <- 4
b1_r <- round(b_m2(m1), dec_t11); b2_r <- round(b_m2(m2), dec_t11); b3_r <- round(b_m2(m3), dec_t11)
aporte_r <- round(sesgo$aporte[1:3], dec_t11)

t11 <- bind_rows(
  tibble(`Variable que entra` = paste0(etiq_bloque[sesgo$bloque[1:3]]),
         `Su coeficiente` = gam_txt[sesgo$bloque[1:3]],
         `Si entra sola` = signo(sesgo$cambio_solo[1:3]),
         `Aporte al cambio` = signo(aporte_r, dec_t11)),
  tibble(`Variable que entra` = "Las tres características juntas",
         `Su coeficiente` = "",
         `Si entra sola` = "",
         `Aporte al cambio` = paste0(signo(b_m2(m2) - b_m2(m1), dec_t11), "  (",
                                     coma_ar(b1_r, dec_t11), " → ", coma_ar(b2_r, dec_t11), ")")),
  tibble(`Variable que entra` = etiq_bloque[["barrio"]],
         `Su coeficiente` = gam_txt[["barrio"]],
         `Si entra sola` = "",
         `Aporte al cambio` = paste0(signo(aporte_barrio, dec_t11), "  (",
                                     coma_ar(b2_r, dec_t11), " → ", coma_ar(b3_r, dec_t11), ")"))
)
guardar_tabla(tabla_png(t11, anchos = c(2.6, 1.4, 1.3, 2.2),
                        destacar = c(4, 5),
                        pie_tabla = paste0(
                          "Cambio del coeficiente de log(superficie). «Si entra sola»: agregándola al paso 1 sin las otras; no suman el total ",
                          "porque ambientes y baños se mueven juntos. «Aporte»: coeficiente de la variable × su relación con la superficie; ",
                          "suma exacto el cambio y no depende del orden. Las diferencias en el último decimal son redondeo: ",
                          "cada coeficiente se redondea por su lado. ",
                          "El coeficiente de ambientes pasa de ",
                          coma_ar(coef(m2)["ambientes"], 3), " a ", coma_ar(coef(m3)["ambientes"], 3),
                          " al entrar el barrio. El rango de las primas de barrio es el del paso 3; ",
                          "las del anexo son del modelo completo y por eso difieren un poco.")),
              "t11_sesgo_omitida.png", ancho = 10)

# --- T12. La identidad y el aporte de cada bloque ----------------------------
# La columna del test muestra el p-valor y no el F. El F del barrio que va a la
# presentación es uno solo, el de la hipótesis nula (paso 2 contra paso 3,
# F = 651); el de esta tabla se mide sobre el modelo completo y da 617. Dos F
# para lo mismo confunden. El p-valor, en cambio, es lo que hace falta acá: la
# superficie sobre log(precio/m²) es significativa y su ΔR² es casi nulo, que es
# el ejemplo de por qué el tamaño del efecto importa.
a_ancho <- aporte |>
  select(bloque, dependiente, caida_scr, delta_r2, r2_parcial, p) |>
  pivot_wider(names_from = dependiente,
              values_from = c(caida_scr, delta_r2, r2_parcial, p))

t12 <- bind_rows(
  a_ancho |> transmute(
    Bloque = bloque,
    `Reduce la SCR en` = if_else(abs(caida_scr_precio - caida_scr_precio_m2) < 1e-6,
                                 num_ar(caida_scr_precio),
                                 paste(num_ar(caida_scr_precio), "/", num_ar(caida_scr_precio_m2))),
    # Las dos columnas de ΔR² se comparan entre sí: un decimal en las dos. La
    # excepción es un valor por debajo de 1, que con un decimal quedaría en 0,1
    # y perdería que es casi nulo (la superficie sobre el precio por m², 0,08).
    `ΔR² log(precio)` = paste0(map_chr(100 * delta_r2_precio, \(x) coma_ar(x, if (x < 1) 2 else 1)), "%"),
    `ΔR² log(precio/m²)` = paste0(map_chr(100 * delta_r2_precio_m2, \(x) coma_ar(x, if (x < 1) 2 else 1)), "%"),
    `R² parcial` = if_else(abs(r2_parcial_precio - r2_parcial_precio_m2) < 1e-6,
                           coma_ar(r2_parcial_precio, 3),
                           paste(coma_ar(r2_parcial_precio, 3), "/", coma_ar(r2_parcial_precio_m2, 3))),
    `p-valor (Wald)` = if_else(fmt_p(p_precio) == fmt_p(p_precio_m2), fmt_p(p_precio),
                               paste(fmt_p(p_precio), "/", fmt_p(p_precio_m2)))
  ),
  tibble(Bloque = "Suma de cuadrados total (el denominador)",
         `Reduce la SCR en` = "",
         `ΔR² log(precio)` = num_ar(sct(d$lp)),
         `ΔR² log(precio/m²)` = num_ar(sct(d$lpm2)),
         `R² parcial` = "", `p-valor (Wald)` = ""),
  tibble(Bloque = "R² del modelo completo",
         `Reduce la SCR en` = "",
         `ΔR² log(precio)` = coma_ar(summary(m4)$r.squared, 3),
         `ΔR² log(precio/m²)` = coma_ar(summary(m4p)$r.squared, 3),
         `R² parcial` = "", `p-valor (Wald)` = "")
)
guardar_tabla(tabla_png(t12, anchos = c(3.4, 1.4, 1.4, 1.5, 1.2, 1.6),
                        destacar = 2,
                        pie_tabla = paste0(
                          "Modelo completo (paso 4). Cada bloque se quita del modelo y se mide cuánto sube la suma de cuadrados de los residuos (SCR). ",
                          "Las dos ecuaciones son la misma: con log(superficie) a la derecha, pasar de log(precio) a log(precio/m²) solo le resta 1 a su ",
                          "coeficiente; residuos y demás coeficientes son idénticos. Donde hay un solo número, vale para las dos. ",
                          "p-valor del test de Wald con errores HC1 sobre el bloque. ",
                          pie_n)),
              "t12_identidad_aporte.png", ancho = 11.5)

# --- T13. Supuestos (cuerpo) -------------------------------------------------
# El máximo se toma sobre todas las variables, barrio incluido: su GVIF
# ajustado es bajo (el GVIF crudo, 31, no es comparable porque son 41 columnas).
gvif_max <- gvif |> slice_max(gvif_aj, n = 1)
gvif_barrio <- gvif |> filter(variable == "barrio")
t13 <- tibble(
  Supuesto = c("Forma funcional", "Varianza constante", "Observaciones influyentes",
               "Multicolinealidad"),
  Test = c("RESET", "Breusch-Pagan", "Distancia de Cook > 4/n", "GVIF ajustado"),
  # Las celdas largas van en dos líneas ("\n"), con filas más altas: en una
  # sola, la del GVIF se metía en la columna de al lado.
  Resultado = c(
    paste0("F = ", coma_ar(reset$statistic, 1), " (", paste(num_ar(reset$parameter), collapse = " y "),
           " gl),\n", fmt_p(reset$p.value)),
    paste0("BP = ", num_ar(bp$statistic), " (", bp$parameter, " gl),\n", fmt_p(bp$p.value)),
    paste0(num_ar(sum(infl)), " avisos (", coma_ar(100 * mean(infl)), "%)"),
    paste0("Máximo ", coma_ar(gvif_max$gvif_aj, 2), " (distancia al Obelisco);\nbarrio ",
           coma_ar(gvif_barrio$gvif_aj, 2), "; umbral 3,16")
  ),
  `Qué hicimos` = c(
    paste0("Probamos log(m²)²: sigue rechazando (F = ", num_ar(reset_q$statistic), ", ",
           paste(num_ar(reset_q$parameter), collapse = " y "), " gl),\npero las conclusiones no cambian"),
    "Rechaza. Errores robustos HC1 en todo",
    paste0("Con y sin: β de la superficie ", coma_ar(b_m2(m4), 3), " → ", coma_ar(b_m2(m4_sin), 3),
           ". No se sacan"),
    # El barrio solo ya explica lo mismo que todo el resto del modelo: la
    # superposición es entera con el barrio.
    # "por sí solo" y no "solo": "el barrio solo explica" se podía leer como
    # "explica apenas", que es lo contrario de lo que decimos.
    paste0("El barrio por sí solo explica el ", coma_ar(100 * r2_dist_barrio, 0), "% de la distancia; al revés no: ",
           "una variable\nno explica 41 categorías. Es un control: no se interpreta")
  )
)
stopifnot(gvif_max$variable == "dist_centro_km")   # el texto de la celda lo nombra
guardar_tabla(tabla_png(t13, anchos = c(1.5, 1.5, 3.1, 5),
                        alinear = c("izq", "izq", "izq", "izq"),
                        alto_fila = 0.62,
                        pie_tabla = paste0("Sobre el modelo completo (paso 4). ", pie_n)),
              "t13_supuestos.png", ancho = 12.5)

# --- T14. Primas exactas de los 41 barrios (anexo) ---------------------------
primas <- tibble(termino = names(coef(m4)), beta = coef(m4)) |>
  filter(str_starts(termino, "barrio")) |>
  mutate(barrio = str_remove(termino, "^barrio"),
         ee = sqrt(diag(vc(m4)))[termino],
         aproximado = 100 * beta, exacto = 100 * (exp(beta) - 1)) |>
  arrange(desc(beta))
write_csv(primas, "output/tablas/08_primas_barrio.csv")

mitad <- ceiling(nrow(primas) / 2)
fila_prima <- function(p) transmute(p, Barrio = barrio, `β` = coma_ar(beta, 3),
                                    `100 × β` = paste0(coma_ar(aproximado, 1), "%"),
                                    `Exacta` = paste0(coma_ar(exacto, 1), "%"))
izq <- fila_prima(slice(primas, 1:mitad))
der <- fila_prima(slice(primas, (mitad + 1):n()))
der <- bind_rows(der, tibble(Barrio = rep("", mitad - nrow(der)), `β` = "", `100 × β` = "", Exacta = ""))
names(der) <- paste0(names(der), " ")
t14 <- bind_cols(izq, der)
guardar_tabla(tabla_png(t14, anchos = c(2, 0.9, 1, 1, 2, 0.9, 1, 1),
                        alinear = c("izq", "der", "der", "der", "izq", "der", "der", "der"),
                        size = 2.7, alto_fila = 0.26,
                        pie_tabla = paste0(
                          "Prima de cada barrio sobre Balvanera en el modelo completo. La lectura 100 × β es una aproximación ",
                          "que se rompe cuando β es grande; la correcta es (exp(β) − 1) × 100.")),
              "t14_primas_barrio.png", ancho = 11)

# --- T15. Cook y GVIF en detalle (anexo) -------------------------------------
t15 <- bind_rows(
  cook_tabla |> transmute(Concepto = c("Log superficie", "Ambientes", "Baños",
                                       "Distancia al Obelisco (km)", "Publica una inmobiliaria",
                                       "Puerto Madero", "Palermo"),
                          `Con todos` = map_chr(con, num_coef),
                          `Sin influyentes` = map_chr(sin, num_coef)),
  tibble(Concepto = "R²", `Con todos` = coma_ar(summary(m4)$r.squared, 3),
         `Sin influyentes` = coma_ar(summary(m4_sin)$r.squared, 3)),
  tibble(Concepto = "Avisos", `Con todos` = num_ar(n_mod), `Sin influyentes` = num_ar(nobs(m4_sin)))
)
guardar_tabla(tabla_png(t15, anchos = c(2.6, 1.2, 1.4),
                        pie_tabla = "Coeficientes del modelo completo con y sin los avisos de distancia de Cook mayor a 4/n."),
              "t15_cook.png", ancho = 7)

cat("\nGráficos y tablas de la regresión generados.\n")
