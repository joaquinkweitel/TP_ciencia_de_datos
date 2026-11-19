# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 2: métodos estadísticos y cuantitativos
# Script 10: los textos de la presentación
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: output/tablas/0X_numeros.csv y los CSV de los scripts 03, 08 y 09
# Salida : output/tablas/textos_instancia2.md
#
# La presentación se arma en Canva. Este script escribe, para cada diapositiva,
# el título, la bajada, qué imagen va y el texto de interpretación, y marca qué
# va al cuerpo y qué al anexo.
#
# Ningún número se escribe a mano: todos salen de lo que guardaron los scripts
# 06 a 09. Si un script cambia, se vuelve a correr este y los textos se
# actualizan solos. Hay que correr antes 06, 07, 08 y 09.
#
# Reglas de escritura (prompt de la instancia 1, siguen vigentes): primera
# persona del plural, frases cortas, nada de vocabulario causal ("se asocia
# con", "es mayor en", nunca "determina" ni "explica el X%"), títulos que dicen
# el hallazgo, coma decimal y punto de miles.
# =============================================================================

library(tidyverse)
source("script/00_tema.R")

# --- Los números -------------------------------------------------------------
# leer_numeros() está en utils/leer_numeros.R (la carga 00_tema.R).
N <-c(leer_numeros("06_numeros.csv"), leer_numeros("07_numeros.csv"))
N3 <- leer_numeros("03_numeros.csv")

# Qué parte de la variación de cada suplementaria del PCA de cercanía capturan
# los dos componentes: la suma de sus cargas al cuadrado (cos²). En la
# diapositiva va como cota redonda ("menos del 5%"), calculada desde el valor.
cos2_pm2 <- 100 * (as.numeric(N[["pm2_pc1"]])^2 + as.numeric(N[["pm2_pc2"]])^2)
cos2_lat <- 100 * (as.numeric(N[["lat_pc1"]])^2 + as.numeric(N[["lat_pc2"]])^2)
tope_pm2 <- ceiling(cos2_pm2); tope_lat <- ceiling(cos2_lat)

# La diapositiva 1 dice que en las dos mitades el máximo está en el mismo tramo
# y que el tramo más cercano al Obelisco es el más barato de cada una. Si deja
# de ser cierto, el script se frena.
stopifnot(N3[["tramo_max_norte"]] == N3[["tramo_max_sur"]],
          N3[["tramo_min_norte"]] == "Hasta 3 km", N3[["tramo_min_sur"]] == "Hasta 3 km",
          # La diapositiva 13 dice que el anillo cercano concentra el stock viejo.
          as.numeric(N3[["pct_mas50_cerca"]]) > as.numeric(N3[["pct_mas50_resto_max"]]))
N8 <- leer_numeros("08_numeros.csv")
N9 <- leer_numeros("09_numeros.csv")

# Un número guardado, ya formateado. dec = 0 usa punto de miles. Se llama nn()
# y no n() para no tapar el n() de dplyr dentro de summarise().
nn <- function(v, x = N, dec = 0) {
  val <- as.numeric(x[[v]])
  if (dec == 0) num_ar(val) else coma_ar(val, dec)
}
n8 <- function(v, dec = 3) nn(v, N8, dec)
n9 <- function(v, dec = 2) nn(v, N9, dec)
# Un ΔR² se lee en puntos de R² (proporción de la variabilidad total), no como
# porcentaje del R².
pts8 <- function(v, dec = 1) paste(coma_ar(100 * as.numeric(N8[[v]]), dec), "puntos")
txt <- function(v, x = N) x[[v]]
# El mismo guion que usa coma_ar() para los negativos, para no mezclar signos.
con_signo <- function(x, dec = 3) paste0(if_else(x > 0, "+", "-"), coma_ar(abs(x), dec))

titulos_i1 <- read_csv("output/tablas/03_titulos_graficos.csv", show_col_types = FALSE)
perfil     <- read_csv("output/tablas/09_perfiles.csv", show_col_types = FALSE)
ari        <- read_csv("output/tablas/09_comparacion_ari.csv", show_col_types = FALSE)
dentro     <- read_csv("output/tablas/09_dentro_grupo.csv", show_col_types = FALSE)
ejemplo    <- read_csv("output/tablas/09_ejemplo_recoleta.csv", show_col_types = FALSE)
prima      <- read_csv("output/tablas/08_prima_superficie_barrio.csv", show_col_types = FALSE)

a <- function(que, dec = 2) coma_ar(ari$ari[ari$comparacion == que], dec)
p <- function(g, col, dec = 1) coma_ar(perfil[[col]][perfil$grupo == g], dec)

# Los nombres de los grupos se escriben a mano ("viejos y baratos", etc.).
# Antes, se verifica que sigan describiendo a los grupos: si el kmeans cambiara,
# este script se frena en lugar de escribir algo falso.
stopifnot(
  which.max(perfil$pct_mas50) == 1,       # el 1 concentra el stock viejo
  which.min(perfil$precio_m2) == 1,       # y es el más barato por m²
  which.max(perfil$pct_estrenar) == 2,    # el 2 es el de los a estrenar
  which.min(perfil$m2) == 2,              # y el más chico
  which.max(perfil$m2) == 3,              # el 3 es el de los grandes
  which.max(perfil$precio_m2) == 3,       # y el más caro por m²
  which.min(perfil$dist_km) == 1          # el más barato es el más cercano al Obelisco
)

# Dentro de cada grupo: ¿el barrio sigue ordenando el precio por m²?
dentro_res <- dentro |>
  group_by(grupo) |>
  summarise(spearman = cor(pm2, prima_pct, method = "spearman"), barrios = n(),
            rango = max(pm2) / min(pm2), .groups = "drop")
# La síntesis presenta las brechas como una secuencia que baja a medida que se
# controla más. Si dejara de bajar, el script se frena en vez de escribirlo.
stopifnot(as.numeric(N9[["rango_total"]]) > as.numeric(N8[["brecha_p3"]]),
          as.numeric(N8[["brecha_p3"]]) > as.numeric(N8[["brecha_extremos"]]))

ej <- function(barrio, g, col)ejemplo[[col]][ejemplo$barrio_nom == barrio & ejemplo$grupo == g]
prima_de <- function(barrio) coma_ar(prima$prima_pct[prima$barrio_nom == barrio], 0)


# --- Las diapositivas --------------------------------------------------------
# Cada una es una lista: sección, cuerpo o anexo, título, bajada, imágenes y
# texto. El texto se escribe en párrafos cortos; en Canva se recorta si no entra.

diapo <- function(seccion, lugar = "Cuerpo", titulo, bajada = NULL, imagen = NULL, texto) {
  list(seccion = seccion, lugar = lugar, titulo = titulo, bajada = bajada,
       imagen = imagen, texto = texto)
}

cuerpo <- list(

  # ---------------------------------------------------------------- Sección 1
  diapo(
    "1. Retomar la primera entrega",
    titulo = "El precio por m² ordena la ciudad de sur a norte, no del centro a la periferia",
    bajada = "Síntesis de la instancia 1: la base, la pregunta y lo que encontramos.",
    imagen = "g3_norte_sur_distancia.png (de la instancia 1)",
    texto = c(
      paste0("**La base.** ", nn("n_crudo"), " avisos de departamentos en venta en la Ciudad ",
             "de Buenos Aires, publicados en MercadoLibre y relevados por la cátedra en noviembre ",
             "de 2024. La unidad de observación es el aviso publicado, no la vivienda. Sacamos las ",
             "republicaciones del mismo inmueble y quedaron ", nn("n_base"), "."),
      "**La pregunta.** ¿Qué pesa más en el precio de un departamento: la ubicación o la cantidad de metros cuadrados?",
      paste0("**Lo que encontramos.** El precio total sigue de cerca a la superficie. El precio por m², ",
             "en cambio, varía con la ubicación: ",
             str_to_lower(str_sub(titulos_i1$titulo[titulos_i1$id == "g2"], 1, 1)),
             str_sub(titulos_i1$titulo[titulos_i1$id == "g2"], 2), ". Y no es una cuestión de ",
             "distancia al centro: ", str_to_lower(str_sub(titulos_i1$titulo[titulos_i1$id == "g3"], 1, 1)),
             str_sub(titulos_i1$titulo[titulos_i1$id == "g3"], 2), ". Dentro de cada mitad, estar ",
             "cerca del Obelisco no es estar más caro: en las dos el máximo está entre ",
             # "3 a 6 km" pasa a "3 y 6 km" para que se lea "entre 3 y 6 km".
             str_replace(N3[["tramo_max_norte"]], " a ", " y "), ", y el tramo de hasta 3 km es ",
             "el más barato de cada mitad (US$ ", num_ar(as.numeric(N3[["min_cerca_norte"]])),
             " por m² en el norte, US$ ", num_ar(as.numeric(N3[["min_cerca_sur"]])), " en el sur).")
    )
  ),

  diapo(
    "1. Retomar la primera entrega",
    titulo = "Incorporamos los cuatro pedidos de la devolución y aclaramos para qué usamos la latitud",
    texto = c(
      paste0("**Descriptivas de todas las variables relevantes.** A la presentación anterior llegaron 7; ",
             "ahora están las ", nn("n_descriptivas"), " (anexo)."),
      paste0("**Avisos por barrio.** Tabla de frecuencias de los ", nn("n_barrios"),
             " barrios con su porcentaje (anexo). Los cinco con más avisos reúnen el ",
             nn("pct_top5", dec = 1), "%."),
      paste0("**Valores atípicos.** En la instancia 1 escribimos que no se podía decir nada porque la ",
             "base venía recortada. Estaba mal: la base viene recortada y aun así se puede analizar ",
             "lo que quedó. Lo hacemos en la sección 2."),
      paste0("**Variables de cercanía.** Las habíamos descartado sin mostrar por qué. Ahora lo ",
             "mostramos con una matriz de correlaciones y un PCA (diapositiva siguiente)."),
      paste0("**La latitud.** La devolución la marcó entre las variables que no hacía falta analizar. ",
             "No lo explicamos bien: es la variable que mide el eje norte-sur, el hallazgo central del ",
             "trabajo. La usamos en la regresión."),
      paste0("**geocod_score.** De acuerdo con la devolución: no la analizamos. Solo la mencionamos ",
             "como evidencia de que la base llega recortada (nunca baja de 80).")
    )
  ),

  diapo(
    "1. Retomar la primera entrega",
    titulo = "Las once variables de cercanía miden centralidad, y la centralidad no ordena el precio",
    bajada = paste0("Círculo de correlaciones de un PCA sobre las once variables. El precio por m² y ",
                    "la latitud se proyectan encima sin participar del cálculo."),
    imagen = "g13_pca_cercania.png",
    texto = c(
      paste0("El primer componente (", nn("pc1", dec = 1), "% de la varianza) es centralidad: la distancia ",
             "al Obelisco correlaciona ", nn("obelisco_pc1", dec = 2), " con él, y las paradas de colectivo, ",
             "las líneas y las escuelas cargan en el mismo eje. El segundo (", nn("pc2", dec = 1),
             "%) agrupa las tres de espacios verdes."),
      paste0("El precio por m² cae en (", nn("pm2_pc1", dec = 2), "; ", nn("pm2_pc2", dec = 2),
             ") y la latitud en (", nn("lat_pc1", dec = 2), "; ", nn("lat_pc2", dec = 2),
             "), pegados al origen: los dos componentes capturan menos del ", tope_pm2, "% de la variación ",
             "del precio por m² (", coma_ar(cos2_pm2, 1), "%) y menos del ", tope_lat, "% de la de la latitud (",
             coma_ar(cos2_lat, 1), "%). Es la suma de sus cargas al cuadrado en los dos componentes."),
      paste0("Por eso de este bloque llevamos a la regresión solo la distancia al Obelisco, como ",
             "representante, y no un índice construido con el PCA: su coeficiente se lee en kilómetros.")
    )
  ),

  # ---------------------------------------------------------------- Sección 2
  diapo(
    "2. Faltantes y outliers",
    titulo = "Los faltantes no son azar: dos son estructurales y uno está disfrazado",
    bajada = "Variables con faltantes, por qué faltan y qué hicimos con cada una.",
    imagen = "t01_faltantes.png",
    texto = c(
      paste0("Solo dos variables tienen faltantes. `inmobiliaria` falta en ", nn("na_inmo"), " avisos (",
             nn("pct_na_inmo", dec = 1), "%): exactamente los publicados por particulares. ",
             "`antiguedad_max` falta en ", nn("na_antig"), " (", nn("pct_na_antig", dec = 1),
             "%): exactamente la categoría abierta «Más de 50 años». Lo verificamos fila por fila."),
      paste0("**Estrategia: mantenerlos.** El faltante es informativo y su indicadora ya está en la base ",
             "(`publicado_por` y `antiguedad_grupo`). No perdemos ningún aviso. Si elimináramos las filas ",
             "incompletas quedarían ", nn("filas_completas"), " de ", nn("n_base"),
             ". La consecuencia: `inmobiliaria` no se puede usar como variable de nada."),
      # El rango se da sobre los barrios con al menos 100 avisos, como en los
      # modelos: con los 48, el mínimo pasa a ser Villa Soldati (42 avisos, la
      # mitad mal asignados según la decisión 9), un dato que no informa.
      paste0("**Dónde se concentran.** Entre los ", nn("part_n_barrios"), " barrios con al menos 100 avisos, ",
             "los particulares van del ", nn("part_max", dec = 0), "% de los avisos en ",
             txt("part_max_barrio"), " al ", nn("part_min", dec = 0), "% en ", txt("part_min_barrio"),
             ". Hay un gradiente con el precio del barrio (correlación ", nn("part_pearson", dec = 2),
             "), pero no es limpio (anexo)."),
      paste0("Descartamos `publicacion_reciente`, que nunca usamos: el ", nn("pct_pubrec", dec = 1),
             "% de los avisos dice «sin dato», un faltante con otro nombre.")
    )
  ),

  diapo(
    "2. Faltantes y outliers",
    # Sin "la escala importa más que la regla": el cuerpo muestra que las seis
    # combinaciones cortan la misma cola, y la variación de regla a regla es del
    # mismo orden que la de escala a escala.
    titulo = paste0("Con el rango intercuartílico, los atípicos del precio pasan de ", nn("iqr_orig"),
                    " en escala original a ", nn("iqr_log"), " en logaritmo"),
    bajada = "Boxplot del precio publicado en las dos escalas, y conteos de tres reglas en dos escalas.",
    imagen = "g09a_boxplot_precio_original.png y g09b_boxplot_precio_log.png, debajo t02_tres_reglas.png",
    texto = c(
      paste0("Aplicamos las tres reglas de la clase 13 (rango intercuartílico, z clásico y z robusto) en ",
             "escala original y en logaritmo. Sobre el precio marcan entre ",
             nn("z_orig"), " y ", nn("zrob_orig"), " avisos en escala original, y entre ",
             nn("z_log"), " y ", nn("iqr_log"), " en logaritmo."),
      paste0("**No coinciden en cuántos, pero no se contradicen.** Las seis combinaciones de regla y ",
             "escala marcan en total ", nn("marcados_alguna"), " avisos, y la más laxa sola cubre ",
             nn("marcados_max"), ". Las más estrictas marcan un subconjunto de las más laxas: todas ",
             "cortan la misma cola, en lugares distintos. Por eso las reglas dan candidatos, no un ",
             "veredicto: dónde cortar es una decisión."),
      paste0("**El logaritmo no es mejor por sí mismo.** La distancia al Obelisco va al revés que el ",
             "precio: con el rango intercuartílico marca ", nn("dist_iqr_orig"), " atípicos en escala ",
             "original y ", nn("dist_iqr_log"), " en logaritmo, todos a menos de ", nn("dist_log_lim_m"),
             " m del Obelisco. La distancia es casi simétrica (asimetría ", nn("dist_asim_orig", dec = 2),
             ") y el logaritmo le estira la cola izquierda (", nn("dist_asim_log", dec = 2), "). La escala ",
             "tiene que ajustarse a la forma de la variable: el logaritmo ordena una cola larga a la ",
             "derecha, y donde no la hay, inventa atípicos. Pasa algo parecido, en chico, con el precio: ",
             "en logaritmo aparecen ", nn("precio_log_bajos"), " atípicos por abajo (menos de US$ ",
             nn("precio_log_lim"), ") que en escala original no pueden existir."),
      paste0("El límite inferior solo informa cuando es alcanzable. En el precio es negativo, así que la ",
             "falta de atípicos bajos no dice nada. En el precio por m² es US$ ", nn("lim_inf_m2"),
             " y ningún aviso queda por debajo: el mínimo es US$ ", nn("min_m2"), ".")
    )
  ),

  diapo(
    "2. Faltantes y outliers",
    titulo = "Los extremos son casi todos genuinos: Puerto Madero existe",
    bajada = "Avisos que no son departamentos, y qué le pasa a la base con cada estrategia.",
    imagen = "t04_no_departamentos.png y t03_impacto_estrategias.png",
    texto = c(
      paste0("Revisamos los extremos uno por uno. ", nn("pm_top100"), " de los 100 avisos de mayor precio ",
             "por m² son de Puerto Madero: un submercado real, no un error."),
      paste0("Lo que sí encontramos son ", nn("no_depto"), " avisos que no son departamentos (",
             txt("no_depto_tipos"), "). Los identificamos porque el título empieza nombrando otra unidad. ",
             "Esos salen de los modelos."),
      paste0("**Estrategia para el resto: mantener, trabajar en logaritmo y medir la sensibilidad.** ",
             "Sacar todo lo que marca la regla de 1,5 RIC eliminaría ", nn("sacados_ric"),
             " avisos, bajaría la media de US$ ", nn("media_base"), " a US$ ", nn("media_sin_ric"),
             " y el P95 de US$ ", nn("p95_base"), " a US$ ", nn("p95_sin_ric"),
             ". No es limpiar: es borrar la parte alta del mercado."),
      paste0("**Por qué tampoco winsorizamos**, aunque la tabla la muestre como la opción que menos mueve: ",
             "conserva todos los avisos, pero reemplaza los ", n8("w_sobre_p99", 0), " precios por encima del ",
             "p99 (US$ ", n8("w_p99", 0), ") por ese techo, y ", n8("w_pm_sobre_p99", 0), " de ellos son de ",
             "Puerto Madero (", coma_ar(100 * as.numeric(N8[["w_pm_sobre_p99"]]) / as.numeric(N8[["w_pm_total"]]), 0),
             "% del barrio). Es cambiar valores genuinos por uno inventado. Y no hace falta: en logaritmo la ",
             "cola ya pesa poco. Lo probamos en el modelo completo: la prima de Puerto Madero pasa de +",
             coma_ar(as.numeric(N8[["w_prima_pm"]]), 1), "% a +", coma_ar(as.numeric(N8[["w_prima_pm_w"]]), 1),
             "% y la elasticidad de ", n8("b4"), " a ", n8("w_b4"), "."),
      paste0("La sensibilidad la medimos en cada método: con la distancia de Cook en la regresión y ",
             "con PAM, que es robusto a extremos, en el kmeans.")
    )
  ),

  # ---------------------------------------------------------------- Sección 3
  diapo(
    "3. Regresión lineal",
    titulo = "Usamos una regresión porque queremos mirar el tamaño y el barrio a la vez",
    texto = c(
      paste0("**La pregunta.** ¿Cuánto se asocia el precio con la superficie y cuánto con el barrio, ",
             "mirando cada uno con el otro fijo?"),
      paste0("**Por qué este método.** El precio es continuo y las variables que lo acompañan mezclan ",
             "continuas (superficie, distancia) y categóricas (barrio, antigüedad). Un ANOVA compararía ",
             "barrios sin descontar el tamaño; la regresión los mira juntos. Precio y superficie van en ",
             "logaritmo, por la asimetría de la sección 2 y porque así los coeficientes se leen como ",
             "variaciones porcentuales."),
      paste0("**El modelo, en cuatro pasos.** Superficie; más ambientes, baños y antigüedad; más el barrio ",
             "(41 categorías, con Balvanera como base); más la distancia al Obelisco y quién publica. ",
             n8("n_mod", 0), " avisos de los ", n8("n_barrios", 0), " barrios con al menos 100. ",
             "Errores estándar robustos (HC1)."),
      paste0("**Hipótesis nula.** Una vez que se tienen en cuenta la superficie y las características de ",
             "la vivienda, el barrio no se asocia con el precio: todos los barrios valen lo mismo que ",
             "Balvanera. **Alternativa:** al menos un barrio se aparta de Balvanera."),
      paste0("**Resultado.** F = ", n8("F_h0", 2), " con ", n8("gl1_h0", 0), " y ", n8("gl2_h0", 0),
             " grados de libertad, p < 2,2 × 10⁻¹⁶. Rechazamos la nula. Tamaño del efecto: el barrio suma ",
             # Cuatro decimales en los R² y dos en la diferencia, para que la resta cierre a la vista.
             coma_ar(100 * (round(as.numeric(N8[["r2_3"]]), 4) - round(as.numeric(N8[["r2_2"]]), 4)), 2),
             " puntos de R² (de ", n8("r2_2", 4), " a ", n8("r2_3", 4), ").")
    )
  ),

  diapo(
    "3. Regresión lineal",
    titulo = paste0("A igual barrio y características, un departamento 10% más grande se publica ",
                    coma_ar(as.numeric(N8[["pct_10mas"]]), 1), "% más caro"),
    bajada = "Cuatro pasos y una variante con la latitud en lugar del barrio. Variable dependiente: log(precio).",
    imagen = "t10_regresion_pasos.png",
    texto = c(
      paste0("La elasticidad del precio a la superficie es ", n8("b4"), ": casi proporcional y algo menor ",
             "que 1, así que el precio por m² baja levemente con el tamaño."),
      paste0("Con las 41 categorías de barrio el R² es ", n8("r2_4"), ". Reemplazándolas por una sola ",
             "variable, la latitud, es ", n8("r2_lat"), ". Con la latitud, **un kilómetro más al norte se ",
             "asocia con un precio ", coma_ar(as.numeric(N8[["lat_km_pct"]]), 1),
             "% mayor**, a igual distancia al centro."),
      paste0("El R² ajustado, que descuenta la cantidad de variables, es ", n8("r2aj_4", 4),
             ", apenas por debajo del R² de ", n8("r2_4", 4), ": las 41 categorías no inflan el ajuste."),
      paste0("Las primas de cada barrio, calculadas como (exp(β) − 1) × 100 y no como 100 × β, están ",
             "en el anexo.")
    )
  ),

  diapo(
    "3. Regresión lineal",
    titulo = "El coeficiente de la superficie se mueve al sumar controles: es sesgo por variable omitida",
    bajada = paste0("Qué mueve el coeficiente de log(superficie) en cada paso, y la prima de cada barrio ",
                    "contra la superficie promedio de sus avisos."),
    imagen = "t11_sesgo_omitida.png y g15_prima_superficie_barrio.png",
    texto = c(
      paste0("**Al sumar las características sube, de ", n8("b1"), " a ", n8("b2"), ".** Los ambientes lo ",
             "empujan para arriba (", con_signo(as.numeric(N8[["amb_aporte"]])), "): su coeficiente es ",
             "negativo y se mueven con la superficie (correlación ", n8("cor_amb_m2", 2), "), así que al ",
             "omitirlos ese efecto se le cargaba al tamaño. Los baños empujan para el otro lado (",
             con_signo(as.numeric(N8[["banos_aporte"]])), ") y compensan una parte."),
      paste0("**Al sumar el barrio baja, a ", n8("b3"), ".** Todo el cambio (",
             con_signo(as.numeric(N8[["barrio_aporte"]])), ") viene de que, a igual cantidad de ambientes, ",
             "los departamentos más amplios están en los barrios caros. El gráfico muestra la superficie ",
             "tal cual (correlación ", n8("cor_graf", 2), "); descontando ambientes, baños y antigüedad, ",
             "sube a ", n8("cor_cond", 2), "."),
      paste0("**La relación está en los barrios caros.** Por terciles de prima, la correlación es ",
             n8("cor_tercil_caro", 2), " en el tercio caro y ", n8("cor_tercil_barato", 2),
             " en el tercio barato. Tampoco es solo Puerto Madero: sin él, ", n8("cor_sin_pm", 2), "."),
      paste0("Una consecuencia: «a igual superficie, un ambiente más es más barato» vale sin el barrio (",
             n8("amb_2"), ") y desaparece cuando el barrio entra (", n8("amb_3"), ", p = ",
             n8("amb_p3", 2), ").")
    )
  ),

  diapo(
    "3. Regresión lineal",
    titulo = "La superficie acompaña al precio total y el barrio al precio por m²: no compiten",
    bajada = paste0("Cuánto aporta cada bloque al R² cuando se lo quita del modelo completo, en las dos ",
                    "lecturas del precio."),
    imagen = "t12_identidad_aporte.png",
    texto = c(
      paste0("**Las dos regresiones son la misma ecuación.** Con log(superficie) a la derecha, pasar de ",
             "log(precio) a log(precio/m²) solo le resta 1 a su coeficiente: los residuos y los demás ",
             "coeficientes son idénticos. Por eso el barrio reduce la suma de cuadrados en lo mismo en las ",
             "dos (", n8("scr_bar", 0), ") y lo único que cambia es el denominador."),
      paste0("Sobre el precio total, la superficie aporta ", pts8("dr2_sup_p"), " de R² y el barrio ",
             pts8("dr2_bar_p"), ". Sobre el precio por m², el barrio pasa a ", pts8("dr2_bar_m2"),
             " y la superficie a ", pts8("dr2_sup_m2", 2), "."),
      paste0("**Significativo no es lo mismo que importante.** La superficie sobre el precio por m² es ",
             "significativa (F = ", n8("F_sup_m2", 1), " con ", n8("F_sup_m2_gl1", 0), " y ",
             n8("F_sup_m2_gl2", 0), " gl, p < 0,001) y su aporte es prácticamente nulo. ",
             "Por eso respondemos la pregunta con el tamaño del efecto y no con el p-valor.")
    )
  ),

  diapo(
    "3. Regresión lineal",
    titulo = "Los supuestos no se cumplen del todo, y lo que hicimos no cambia las conclusiones",
    bajada = "Verificación de supuestos sobre el modelo completo.",
    imagen = "t13_supuestos.png",
    texto = c(
      paste0("**Forma funcional.** El RESET rechaza. Con ", n8("n_mod", 0), " avisos rechaza casi siempre, ",
             "así que probamos agregar log(m²)²: la elasticidad pasa a ir de ", n8("elast_35", 2),
             " en 35 m² a ", n8("elast_150", 2), " en 150 m², el F del barrio de ", n8("F_h0", 0), " a ",
             n8("F_h0_q", 0), " y el aporte de la superficie al precio por m² sigue cerca de cero (",
             pts8("dr2_sup_m2_q", 2), " de R²). El RESET sigue rechazando (", n8("reset_q", 0), "): lo que falta ",
             "probablemente sean variables que la base no tiene, como el estado o el piso."),
      paste0("**Varianza.** Breusch-Pagan rechaza: usamos errores robustos HC1 en todo."),
      paste0("**Influyentes.** ", n8("cook_n", 0), " avisos superan la distancia de Cook de 4/n. Sin ellos ",
             "la elasticidad pasa de ", n8("b4"), " a ", n8("b4_sin"), ", y las primas de barrio también se ",
             "mueven: la de Puerto Madero pasa de +", coma_ar(as.numeric(N8[["pm_con"]]), 1), "% a +",
             coma_ar(as.numeric(N8[["pm_sin"]]), 1), "%, y la brecha entre los barrios extremos de ",
             n8("brecha_con", 2), " a ", n8("brecha_sin", 2), " veces. Crece, no se achica: el resultado no ",
             "depende de los extremos. No los sacamos."),
      paste0("**Multicolinealidad.** Solo la distancia al Obelisco supera el umbral (", n8("gvif_max", 2),
             "), porque se superpone con el barrio: el barrio por sí solo explica el ",
             coma_ar(100 * as.numeric(N8[["r2_dist_barrio"]]), 1), "% de la distancia. El barrio, en cambio, ",
             "queda en ", n8("gvif_barrio", 2), ": una variable sola se deja explicar por 41 categorías, pero ",
             "41 categorías no se dejan explicar por una variable. La distancia es un control y no la ",
             "interpretamos."),
      paste0("**Limitaciones.** Son precios publicados, no de venta. La base llega recortada. Dejamos ",
             "afuera ", n8("n_chicos", 0), " avisos de ", n8("n_barrios_chicos", 0), " barrios con menos ",
             "de 100. Y son asociaciones entre avisos de un único momento, no causas.")
    )
  ),

  # ---------------------------------------------------------------- Sección 4
  diapo(
    "4. Kmeans",
    titulo = "Agrupamos los avisos por lo que son, sin decirle al algoritmo dónde están",
    bajada = "Los tres criterios para elegir la cantidad de grupos.",
    imagen = "g16_criterios_k.png",
    texto = c(
      paste0("**Por qué kmeans.** Queremos ver si en la base hay tipos de departamento y si esos tipos ",
             "coinciden con la geografía."),
      paste0("**Variables.** Log del precio por m², log de la superficie y tramo de antigüedad (0 a 3), ",
             "estandarizadas: kmeans usa distancia euclídea y sin estandarizar manda la de mayor escala. ",
             "La ubicación no entra, así el cruce con barrio y comuna es una prueba y no algo que pusimos ",
             "nosotros. Ambientes y baños quedan afuera porque se mueven con la superficie y la contarían ",
             "tres veces. 50 arranques."),
      paste0("**Cantidad de grupos.** Los criterios no coinciden. La silueta prefiere ", n9("k_sil", 0),
             " (", n9("silueta_k2"), " contra ", n9("silueta"), " con 3). ",
             if (N9[["gap_empate"]] == "1")
               paste0("El gap no distingue entre 2 y 3: la diferencia es del tamaño de su error estándar, ",
                      "y con dos muestras al azar distintas eligió una vez 2 y otra 3. ")
             else "",
             "El codo no tiene un quiebre marcado."),
      paste0("**Elegimos ", n9("K", 0), " por interpretabilidad:** los grupos se distinguen por antigüedad ",
             "y por tamaño a la vez. Con 2 la partición es prácticamente una sola variable, la antigüedad. ",
             "Es una decisión analítica, no un descubrimiento.")
    )
  ),

  diapo(
    "4. Kmeans",
    titulo = "Tres tipos de departamento: viejos y baratos, nuevos y chicos, grandes y caros",
    bajada = "Perfil de cada grupo en unidades originales. La ubicación no entró al clustering.",
    imagen = "t16_perfiles_grupos.png y g17_grupos_pca.png",
    texto = c(
      paste0("**Grupo 1 (", p(1, "pct"), "% de los avisos).** Los viejos: casi ninguno a estrenar (",
             p(1, "pct_estrenar"), "%) y el ", p(1, "pct_mas50"), "% con más de 50 años. ",
             "Precio por m² mediano de US$ ", num_ar(perfil$precio_m2[1]), ", el más bajo."),
      paste0("**Grupo 2 (", p(2, "pct"), "%).** A estrenar (", p(2, "pct_estrenar"), "%) y chicos: ",
             num_ar(perfil$m2[2]), " m² de mediana."),
      paste0("**Grupo 3 (", p(3, "pct"), "%).** Los grandes: ", num_ar(perfil$m2[3]), " m² de mediana y ",
             "el precio por m² más alto, US$ ", num_ar(perfil$precio_m2[3]), "."),
      paste0("Lo que no entró al clustering: el grupo 3 tiene el ", p(3, "pct_norte", 0), "% de sus avisos ",
             "en la mitad norte, contra el ", p(1, "pct_norte", 0), "% del grupo 1. Y algo que no ",
             "esperábamos: **el grupo más barato es el más cercano al Obelisco** (", p(1, "dist_km"),
             " km de mediana, contra ", p(2, "dist_km"), " y ", p(3, "dist_km"),
             "). El stock viejo está en el centro: en el anillo de hasta 3 km del Obelisco el ",
             coma_ar(as.numeric(N3[["pct_mas50_cerca"]]), 1), "% de los avisos tiene más de 50 años, ",
             "contra ", coma_ar(as.numeric(N3[["pct_mas50_resto_max"]]), 1), "% o menos en los demás. ",
             "Es el mismo anillo que en la diapositiva 1 aparece como el más barato de cada mitad.")
    )
  ),

  diapo(
    "4. Kmeans",
    titulo = "Los grupos son estables, pero no reproducen la geografía",
    bajada = "Coincidencia entre particiones, medida con el índice de Rand ajustado.",
    imagen = "t17_estabilidad_grupos.png",
    texto = c(
      paste0("**Estabilidad.** PAM, que usa avisos reales como centro y es más robusto a extremos, ",
             "coincide con kmeans en el ", n9("pct_igual_pam", 1), "% de los avisos (ARI ",
             a("kmeans contra PAM"), "). Sin los ", n9("n_extremos", 0), " avisos más extremos, ARI ",
             a("kmeans con y sin extremos"), "."),
      paste0("**Geografía.** Contra el barrio el ARI es ", a("Grupos contra barrio", 3), ", contra la comuna ",
             a("Grupos contra comuna", 3), " y contra la mitad norte-sur ",
             a("Grupos contra mitad norte-sur", 3),
             ". ", if (N9[["comunas_todos"]] == N9[["n_comunas"]])
               paste0("Las ", n9("n_comunas", 0), " comunas tienen")
             else paste0(n9("comunas_todos", 0), " de las ", n9("n_comunas", 0), " comunas tienen"),
             " avisos de los tres grupos. Los tipos de ",
             "departamento están repartidos por toda la ciudad. (El ARI contra el barrio no podría llegar ",
             "a 1 aunque la correspondencia fuera perfecta: 3 grupos no reproducen 42 barrios.)"),
      paste0("**Limitaciones.** La silueta media es ", n9("silueta", 2), ": estructura débil. Los grupos ",
             "son una descripción útil de la base, no una partición natural. Sin estandarizar la partición ",
             "cambia (ARI ", a("kmeans con y sin estandarizar"), "): manda la variable de mayor escala. Y ",
             "con 2 o 4 grupos se reordena bastante (ARI ", a("k = 3 contra k = 2"), " y ",
             a("k = 3 contra k = 4"), ").")
    )
  ),

  # ---------------------------------------------------------------- Sección 5
  diapo(
    "5. Síntesis y próximos pasos",
    titulo = "El barrio pesa en el precio por m² aun comparando el mismo tipo de departamento",
    bajada = paste0("Cómo se reparte cada barrio entre los tres grupos, con los barrios ordenados por la ",
                    "prima que les estima la regresión."),
    imagen = "g18_barrios_grupos.png",
    texto = c(
      paste0("**La hipótesis.** La superficie acompaña al precio total (", pts8("dr2_sup_p"), " de R²) y ",
             "el barrio al precio por m² (", pts8("dr2_bar_m2"), "). No compiten: responden preguntas ",
             "distintas. Los resultados apoyan la hipótesis y la reformulan."),
      # La evidencia principal es la secuencia de magnitudes. No es una
      # coincidencia entre métodos: cada escalón controla algo más, y el último
      # está comprimido por construcción (ver decisiones.md).
      paste0("**Cuánto de la diferencia entre barrios sobrevive al comparar comparables.** Entre el ",
             "barrio más caro y el más barato hay ", coma_ar(as.numeric(N9[["rango_total"]]), 1),
             " veces de precio por m² en crudo. A igual superficie, ambientes, baños y antigüedad, la ",
             "regresión deja ", coma_ar(as.numeric(N8[["brecha_p3"]]), 1), " (",
             txt("p3_max_barrio", N8), " contra ", txt("p3_min_barrio", N8), "). Sumando la distancia al ",
             "Obelisco y quién publica, ", coma_ar(as.numeric(N8[["brecha_extremos"]]), 1), " (",
             txt("prima_max_barrio", N8), " contra ", txt("prima_min_barrio", N8), "). Y dentro de cada ",
             "tipo de departamento del kmeans, entre ", coma_ar(min(dentro_res$rango), 1), " y ",
             coma_ar(max(dentro_res$rango), 1), ": un piso, no una estimación, porque los grupos se ",
             "arman con el precio por m² y adentro de cada uno ese precio ya viene recortado."),
      paste0("Mires como mires, la mayor parte de la diferencia entre barrios se sostiene. Una parte es ",
             "qué tipo de departamento se publica en cada uno; el resto, la mayor, se mantiene a igual tipo. ",
             "Y los tres tipos están en los ", n9("barrios_todos", 0), " barrios: no son tipos de barrio."),
      paste0("**El ejemplo.** Recoleta tiene una prima de ", prima_de("Recoleta"), "% sobre Balvanera y el ",
             coma_ar(ej("Recoleta", 1, "pct"), 0), "% de sus avisos en el grupo viejo y barato (Retiro, el ",
             coma_ar(ej("Retiro", 1, "pct"), 0), "%). Un ",
             "departamento de ese grupo se publica a US$ ", num_ar(ej("Recoleta", 1, "pm2")),
             " por m² en Recoleta y a US$ ", num_ar(ej("Balvanera", 1, "pm2")), " en Balvanera. ",
             "Un barrio caro no es un barrio de departamentos caros: conviven los dos tipos."),
      # Las dos correlaciones van como dato secundario: las dos cruzan cosas
      # construidas desde el precio, así que son esperables.
      # La misma idea con datos de la instancia 1 y otra partición (g6). No entra
      # en la versión corta: la 15 ya está llena.
      # "Crece en los tramos viejos" y no "crece con la antigüedad": entre los dos
      # tramos nuevos está quieta (1,33 y 1,32).
      paste0("**La misma idea en la instancia 1.** La brecha norte-sur es mayor en el stock viejo: el norte ",
             "es ", coma_ar(as.numeric(N3[["brecha_estrenar"]]), 2), " veces más caro en los a estrenar y ",
             coma_ar(as.numeric(N3[["brecha_hasta20"]]), 2), " en los de hasta 20 años, y ",
             coma_ar(as.numeric(N3[["brecha_20a50"]]), 2), " en los de 20 a 50 y ",
             coma_ar(as.numeric(N3[["brecha_mas50"]]), 2), " en los de más de 50. Comparando departamentos ",
             "de la misma antigüedad, la ubicación se asocia con diferencias más grandes en los viejos."),
      paste0("Dos correlaciones que no tomamos como evidencia, porque cruzan cosas construidas desde el ",
             "precio: el grupo caro pesa más en los barrios de prima alta (Spearman ",
             n9("cor_caro_prima", 2), "), y dentro de cada tipo el barrio ordena el precio por m² como ",
             "la prima (entre ", coma_ar(min(dentro_res$spearman), 2), " y ",
             coma_ar(max(dentro_res$spearman), 2), ")."),
      paste0("**Para la entrega final.** ¿Qué hay detrás de la parte de la prima que se sostiene a igual ",
             "tipo de departamento? Vamos a sumar lo que dicen los avisos en el texto (cochera, amenities, ",
             "estado) y a ver si el eje norte-sur se sostiene dentro de cada tipo.")
    )
  )
)

# --- Las versiones cortas ----------------------------------------------------
# La entrega es un PDF: lo que no entra en la diapositiva no se dice en ningún
# lado. Por eso cada diapositiva tiene una versión corta, la que va, y la
# completa queda abajo como respaldo para la defensa. Qué se cae se decide acá,
# sabiendo qué número sostiene qué afirmación:
#   - queda la afirmación central de la diapositiva y el número que la sostiene;
#   - queda todo lo que corrige una lectura equivocada que el gráfico podría
#     inducir (el ambiente negativo solo sin barrio, el 0,79 contra el 0,83,
#     el piso del kmeans);
#   - se va lo que ya muestra la tabla o el gráfico de la misma diapositiva, y
#     el detalle que solo sirve si alguien pregunta.

pct_g3 <- str_extract_all(titulos_i1$titulo[titulos_i1$id == "g3"], "\\d+%")[[1]]
# La brecha entre barrios de la instancia 1 (título de g2): la 15 abre con ella,
# así que la 1 tiene que haberla dicho.
brecha_g2 <- str_extract(titulos_i1$titulo[titulos_i1$id == "g2"], "\\d+,\\d+$")
stopifnot(!is.na(brecha_g2))
seis <- as.numeric(N[c("iqr_orig", "z_orig", "zrob_orig", "iqr_log", "z_log", "zrob_log")])

cortos <- list(
  # 1
  c(paste0("Base: ", nn("n_base"), " avisos de departamentos en venta en CABA (MercadoLibre, noviembre ",
           "de 2024); la unidad es el aviso. Pregunta: ¿qué pesa más en el precio, la ubicación o los m²?"),
    paste0("Hallazgo: entre el barrio más caro y el más barato el precio por m² se multiplica por ",
           brecha_g2, ". A igual distancia del Obelisco, el norte es entre ", pct_g3[1], " y ", pct_g3[2],
           " más caro, y el tramo más cercano al centro es el más barato de cada mitad.")),
  # 2
  c(paste0("Descriptivas: de 7 a ", nn("n_descriptivas"), " variables (anexo)."),
    "Avisos por barrio: tabla con porcentajes (anexo).",
    "Atípicos: antes dijimos que no se podía decir nada. Ahora los detectamos y decidimos (sección 2).",
    "Cercanía: mostramos con un PCA por qué no entran (diapositiva 3).",
    "Latitud: es la medida del eje norte-sur, el hallazgo central. Entra en la regresión.",
    "geocod_score: de acuerdo, no la analizamos."),
  # 3
  c(paste0("Las once variables arman un eje de centralidad (", nn("pc1", dec = 1), "% de la varianza; la ",
           "distancia al Obelisco correlaciona ", nn("obelisco_pc1", dec = 2), " con él)."),
    paste0("Los dos componentes capturan menos del ", tope_pm2, "% de la variación del precio por m² y ",
           "menos del ", tope_lat, "% de la de la latitud. Por eso de este bloque solo llevamos a la ",
           "regresión la distancia al Obelisco.")),
  # 4
  c(paste0("`inmobiliaria` falta exactamente en los avisos de particulares y `antiguedad_max` en la ",
           "categoría «Más de 50 años»: son estructurales. Los mantenemos y no perdemos ningún aviso."),
    paste0("`publicacion_reciente` dice «sin dato» en el ", nn("pct_pubrec", dec = 1), "% de los avisos: es ",
           "un faltante disfrazado. La descartamos; nunca la usamos.")),
  # 5
  c(paste0("Sobre el precio, las seis combinaciones de regla y escala marcan entre ", num_ar(min(seis)),
           " y ", num_ar(max(seis)), " avisos, y no se contradicen: la más laxa cubre ", nn("marcados_max"),
           " de los ", nn("marcados_alguna"), " que marca alguna. Cortan la misma cola en lugares distintos."),
    paste0("La distancia al Obelisco va al revés: ", nn("dist_iqr_orig"), " atípicos en escala original y ",
           nn("dist_iqr_log"), " en logaritmo, todos a menos de ", nn("dist_log_lim_m"), " m del centro. ",
           "El logaritmo ordena una cola larga a la derecha; donde no la hay, la inventa.")),
  # 6
  c(paste0(nn("pm_top100"), " de los 100 avisos de mayor precio por m² son de Puerto Madero: extremos ",
           "genuinos. Sí sacamos ", nn("no_depto"), " avisos que no son departamentos."),
    paste0("Al resto lo mantenemos y trabajamos en logaritmo. Eliminar lo que marca el rango ",
           "intercuartílico borraría ", nn("sacados_ric"), " avisos y bajaría el P95 de US$ ",
           nn("p95_base"), " a US$ ", nn("p95_sin_ric"), ". Winsorizar en el p99 reemplaza ",
           n8("w_sobre_p99", 0), " precios genuinos por un techo, y en logaritmo casi no cambia nada ",
           "(Puerto Madero pasa de +", coma_ar(as.numeric(N8[["w_prima_pm"]]), 1), "% a +",
           coma_ar(as.numeric(N8[["w_prima_pm_w"]]), 1), "%): no hace falta.")),
  # 7
  c(paste0("Regresión porque el precio es continuo y lo acompañan variables continuas y categóricas: ",
           "mira el tamaño y el barrio a la vez. Log(precio) en cuatro pasos: superficie; ambientes, baños ",
           "y antigüedad; barrio (base Balvanera); distancia al Obelisco y quién publica. ",
           n8("n_mod", 0), " avisos, errores HC1."),
    paste0("H0: a igual superficie y características, todos los barrios valen lo mismo que Balvanera. ",
           "H1: al menos uno se aparta."),
    paste0("F = ", n8("F_h0", 2), " (", n8("gl1_h0", 0), " y ", n8("gl2_h0", 0), " gl), p < 2,2 × 10⁻¹⁶: ",
           "rechazamos. El barrio suma ",
           coma_ar(100 * (round(as.numeric(N8[["r2_3"]]), 4) - round(as.numeric(N8[["r2_2"]]), 4)), 2),
           " puntos de R².")),
  # 8
  c(paste0("La elasticidad es ", n8("b4"), ": casi proporcional y algo menor que 1, así que el precio ",
           "por m² baja levemente con el tamaño."),
    paste0("Con la latitud en lugar de los 41 barrios, el R² baja de ", n8("r2_4"), " a ", n8("r2_lat"),
           ", y cada kilómetro al norte se asocia con un precio ", coma_ar(as.numeric(N8[["lat_km_pct"]]), 1),
           "% mayor. El R² ajustado (", n8("r2aj_4", 4), ") queda apenas debajo del R² (", n8("r2_4", 4),
           "): las 41 categorías no inflan el ajuste.")),
  # 9
  # Una oración por movimiento, cada una con su explicación. Las correlaciones
  # quedan en el pie del gráfico.
  c(paste0("Al sumar las características, el coeficiente de la superficie sube de ", n8("b1"), " a ",
           n8("b2"), ": los ambientes tienen coeficiente negativo y se mueven con la superficie, así que al ",
           "omitirlos ese efecto se le cargaba al tamaño."),
    paste0("Al sumar el barrio, baja a ", n8("b3"), ": a igual cantidad de ambientes, los departamentos más ",
           "amplios están en los barrios caros, sobre todo en el tercio más caro."),
    "El coeficiente negativo de ambientes desaparece cuando entra el barrio."),
  # 10
  c(paste0("Las dos regresiones son la misma ecuación: el barrio reduce la suma de cuadrados en lo mismo ",
           "y solo cambia el denominador."),
    paste0("Sobre el precio total, la superficie aporta ", pts8("dr2_sup_p"), " de R² y el barrio ",
           pts8("dr2_bar_p"), ". Sobre el precio por m², el barrio aporta ", pts8("dr2_bar_m2"),
           " y la superficie ", pts8("dr2_sup_m2", 2),
           ", aunque sea significativa (F = ", n8("F_sup_m2", 1), " con ", n8("F_sup_m2_gl1", 0), " y ",
           n8("F_sup_m2_gl2", 0), " gl): significativo no es importante.")),
  # 11
  c(paste0("RESET rechaza, también con log(m²)², pero las conclusiones no cambian: la elasticidad pasa a ir ",
           "de ", n8("elast_35", 2), " a ", n8("elast_150", 2), ". Breusch-Pagan rechaza: errores HC1. Sin ",
           "los ", n8("cook_n", 0), " avisos influyentes, la elasticidad pasa de ", n8("b4"), " a ",
           n8("b4_sin"), " y Puerto Madero de +", coma_ar(as.numeric(N8[["pm_con"]]), 1), "% a +",
           coma_ar(as.numeric(N8[["pm_sin"]]), 1), "%: la brecha entre barrios crece, no depende de los extremos."),
    paste0("Límites: son precios publicados, no de venta; la base llega recortada; son asociaciones, ",
           "no causas.")),
  # 12
  c(paste0("Agrupamos por precio por m², superficie y antigüedad, estandarizadas y sin la ubicación, ",
           "para que el cruce con barrio sea una prueba."),
    paste0("La silueta prefiere ", n9("k_sil", 0),
           if (N9[["gap_empate"]] == "1") ", el gap no distingue entre 2 y 3" else "",
           " y el codo no tiene quiebre. Elegimos 3: los grupos se distinguen por antigüedad y tamaño; ",
           "con 2, casi solo por antigüedad.")),
  # 13
  c(paste0("Viejos y baratos (", p(1, "pct"), "%, US$ ", num_ar(perfil$precio_m2[1]), " por m²), nuevos y ",
           "chicos (", p(2, "pct"), "%, ", num_ar(perfil$m2[2]), " m²) y grandes y caros (", p(3, "pct"),
           "%, ", num_ar(perfil$m2[3]), " m², US$ ", num_ar(perfil$precio_m2[3]), " por m²)."),
    paste0("El grupo viejo es el más cercano al Obelisco: a menos de 3 km, el ",
           coma_ar(as.numeric(N3[["pct_mas50_cerca"]]), 1), "% de los avisos tiene más de 50 años (",
           coma_ar(as.numeric(N3[["pct_mas50_resto_max"]]), 1), "% o menos en los demás anillos). Es el ",
           "anillo más barato de la diapositiva 1.")),
  # 14
  c(paste0("PAM coincide con kmeans en el ", n9("pct_igual_pam", 1), "% de los avisos (ARI ",
           a("kmeans contra PAM"), ")."),
    paste0("Contra barrio, comuna y mitad norte-sur, el ARI es ", a("Grupos contra barrio", 3), ", ",
           a("Grupos contra comuna", 3), " y ", a("Grupos contra mitad norte-sur", 3), ": las ",
           n9("n_comunas", 0), " comunas tienen los tres tipos."),
    paste0("Límites: silueta de ", n9("silueta", 2), ", estructura débil. Sin estandarizar, la partición ",
           "cambia (ARI ", a("kmeans con y sin estandarizar"), ").")),
  # 15
  c(paste0("La superficie acompaña al precio total (", pts8("dr2_sup_p"), " de R²) y el barrio al precio por ",
           "m² (", pts8("dr2_bar_m2"), "): no compiten."),
    paste0("Brecha entre barrios extremos: ", coma_ar(as.numeric(N9[["rango_total"]]), 1),
           " veces en crudo, ", coma_ar(as.numeric(N8[["brecha_p3"]]), 1), " a igual vivienda, ",
           coma_ar(as.numeric(N8[["brecha_extremos"]]), 1), " con distancia y quién publica, ",
           "y, como mínimo, ", coma_ar(min(dentro_res$rango), 1), " a ", coma_ar(max(dentro_res$rango), 1),
           " dentro de cada tipo. La mayor parte se sostiene."),
    paste0("Recoleta (prima de ", prima_de("Recoleta"), "%) tiene casi la mitad de sus avisos en el grupo ",
           "viejo: un barrio caro no es un barrio de departamentos caros. Queda abierto qué hay detrás de ",
           "esa prima."))
)
stopifnot(length(cortos) == length(cuerpo))
for (i in seq_along(cuerpo)) cuerpo[[i]]$corto <- cortos[[i]]


anexo <- list(
  c("g14_avisos_por_barrio.png y t07_avisos_por_barrio.png", "Avisos por barrio, con su porcentaje (pedido de la devolución)."),
  c("t05_descriptivas_vivienda.png", "Descriptivas de las variables de la vivienda (pedido de la devolución)."),
  c("t06_descriptivas_entorno.png", "Descriptivas de las variables de entorno."),
  c("t09_frecuencias_categoricas.png", "Frecuencias de las cuatro variables categóricas."),
  c("g12_correlaciones_cercania.png y t08_pca_cargas.png", "Correlaciones y cargas del PCA de cercanía."),
  c("g11_particulares_por_barrio.png", "Dónde se concentran los faltantes de inmobiliaria."),
  c("g10_tres_reglas.png", "Las seis combinaciones de regla y escala, un punto por aviso."),
  c("t14_primas_barrio.png", "Primas exactas de los 41 barrios sobre Balvanera."),
  c("t15_cook.png", "Coeficientes con y sin los avisos influyentes."),
  c("t18_comuna_grupos.png", "Cómo se reparte cada comuna entre los tres grupos."),
  c("g1, g2, g4 a g8 (instancia 1)", "El detalle de la primera entrega que sigue siendo útil.")
)


# --- Escribir el markdown ----------------------------------------------------

lineas <- c(
  "# Textos de la presentación, instancia 2",
  "",
  paste0("Generado por `script/10_textos.R`. Todos los números salen de los scripts 06 a 09: ",
         "si algo cambia, se vuelve a correr y este archivo se actualiza. ",
         length(cuerpo), " diapositivas de cuerpo sin contar carátula y cierre."),
  ""
)

seccion_actual <- ""
for (i in seq_along(cuerpo)) {
  d <- cuerpo[[i]]
  if (d$seccion != seccion_actual) {
    lineas <- c(lineas, paste0("## ", d$seccion), "")
    seccion_actual <- d$seccion
  }
  lineas <- c(lineas,
              paste0("### Diapositiva ", i, " (", str_to_lower(d$lugar), ")"), "",
              paste0("**Título:** ", d$titulo), "")
  if (!is.null(d$bajada)) lineas <- c(lineas, paste0("**Bajada:** ", d$bajada), "")
  if (!is.null(d$imagen)) lineas <- c(lineas, paste0("**Imagen:** ", d$imagen), "")
  palabras <- sum(str_count(d$corto, "\\S+"))
  lineas <- c(lineas,
              paste0("**Texto de la diapositiva** (", palabras, " palabras):"), "",
              unlist(map(d$corto, \(t) c(t, ""))),
              "<details><summary>Respaldo: versión completa, para la defensa</summary>", "",
              unlist(map(d$texto, \(t) c(t, ""))),
              "</details>", "")
}

lineas <- c(lineas, "## Anexo", "")
for (i in seq_along(anexo)) {
  lineas <- c(lineas, paste0("**A", i, ".** ", anexo[[i]][2], " Imagen: ", anexo[[i]][1]), "")
}

writeLines(lineas, "output/tablas/textos_instancia2.md", useBytes = FALSE)
cat("Textos escritos en output/tablas/textos_instancia2.md:", length(cuerpo), "diapositivas de cuerpo y",
    length(anexo), "del anexo.\n")
