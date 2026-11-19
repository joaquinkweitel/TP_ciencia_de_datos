# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 1: Análisis Exploratorio de Datos
# Script 05: armado de la presentación
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada: output/graficos/*.png y output/tablas/*.csv
# Salida : output/graficos/borrador_desde_R.pptx  (NO es la entrega)
#
# officer y flextable son paquetes de SALIDA, no de análisis: acá no se calcula
# nada. Todos los números vienen de los scripts 01 a 04, que usan solo
# tidyverse. Si alguno de esos números cambia, esta presentación se regenera
# sola sin tocar este archivo.
# =============================================================================

library(tidyverse)
library(officer)
library(flextable)
source("script/00_tema.R")

FECHA <- "Septiembre de 2026"


# =============================================================================
# 1. PLANTILLA 16:9
# =============================================================================
# La plantilla que trae officer es 4:3 y el paquete no expone ninguna función
# para cambiar el tamaño. Copiamos la plantilla, editamos el alto y el ancho
# dentro de presentation.xml y la volvemos a comprimir. Es exactamente lo que
# hace PowerPoint cuando uno cambia el tamaño de la diapositiva.

ANCHO <- 13.333
ALTO  <- 7.5

plantilla_169 <- function(destino) {
  origen <- system.file(package = "officer", "template", "template.pptx")
  tmp <- file.path(tempdir(), "plantilla169")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  zip::unzip(origen, exdir = tmp)

  xml_pres <- file.path(tmp, "ppt", "presentation.xml")
  doc <- xml2::read_xml(xml_pres)
  medida <- xml2::xml_find_first(doc, "//p:sldSz")
  xml2::xml_set_attr(medida, "cx", format(ANCHO * 914400, scientific = FALSE))
  xml2::xml_set_attr(medida, "cy", format(ALTO  * 914400, scientific = FALSE))
  xml2::write_xml(doc, xml_pres)

  if (file.exists(destino)) unlink(destino)
  zip::zip(normalizePath(destino, mustWork = FALSE),
           files = list.files(tmp, recursive = TRUE, all.files = TRUE, no.. = TRUE),
           root = tmp, mode = "mirror")
  invisible(destino)
}

ruta_plantilla <- file.path(tempdir(), "plantilla_169.pptx")
plantilla_169(ruta_plantilla)


# =============================================================================
# 2. PIEZAS DE UNA DIAPOSITIVA
# =============================================================================
# Todas las diapositivas se arman con estas funciones, para que el título, la
# bajada y el número queden siempre en la misma posición y al mismo tamaño.

MARGEN     <- 0.75
TOP_TITULO <- 0.38
TOP_CUERPO <- 1.80

# PowerPoint no deja definir el color de fondo de la diapositiva desde officer,
# así que lo resolvemos con una imagen del tamaño de la diapositiva que se
# coloca antes que todo lo demás.
fondo_png <- file.path(tempdir(), "fondo_slide.png")
ggsave(fondo_png,
       ggplot() + theme_void() +
         theme(plot.background = element_rect(fill = colores$fondo, color = NA)),
       width = ANCHO, height = ALTO, dpi = 96, device = ragg::agg_png)

texto_de <- function(txt, size, color, bold = FALSE, familia = fuente_cuerpo,
                     interlineado = 1.15, align = "left") {
  block_list(fpar(
    ftext(txt, fp_text(font.family = familia, font.size = size,
                       bold = bold, color = color)),
    fp_p = fp_par(text.align = align, padding = 0, line_spacing = interlineado)
  ))
}

# Varios párrafos dentro de un mismo cuadro, separados por una línea en blanco.
parrafos_de <- function(txts, size, color, interlineado = 1.2) {
  partes <- lapply(txts, function(t) {
    fpar(ftext(t, fp_text(font.family = fuente_cuerpo, font.size = size,
                          color = color)),
         fp_p = fp_par(text.align = "left", padding.bottom = 7,
                       line_spacing = interlineado))
  })
  do.call(block_list, partes)
}

nueva_slide <- function(d) {
  d <- add_slide(d, layout = "Blank", master = "Office Theme")
  ph_with(d, external_img(fondo_png, width = ANCHO, height = ALTO),
          location = ph_location(0, 0, ANCHO, ALTO))
}

# El título y la bajada van en un mismo cuadro, como dos párrafos. Si fueran
# dos cuadros con posición fija, un título de dos líneas se montaría encima de
# la bajada. Así la bajada baja sola cuando hace falta.
con_titulo <- function(d, titulo, bajada = NULL) {
  partes <- list(
    fpar(ftext(titulo, fp_text(font.family = fuente_titulo, font.size = 30,
                               bold = TRUE, color = colores$texto)),
         fp_p = fp_par(text.align = "left", padding = 0,
                       padding.bottom = 6, line_spacing = 1))
  )
  if (!is.null(bajada)) {
    partes <- c(partes, list(
      fpar(ftext(bajada, fp_text(font.family = fuente_cuerpo, font.size = 16,
                                 color = colores$secundario)),
           fp_p = fp_par(text.align = "left", padding = 0, line_spacing = 1.1))
    ))
  }
  ph_with(d, do.call(block_list, partes),
          location = ph_location(MARGEN, TOP_TITULO, ANCHO - 2 * MARGEN, 1.3))
}

con_numero <- function(d, n) {
  ph_with(d, texto_de(as.character(n), 10, colores$secundario, align = "right"),
          location = ph_location(ANCHO - MARGEN - 0.6, ALTO - 0.55, 0.6, 0.3))
}

con_grafico <- function(d, archivo, left, top, ancho, alto) {
  ph_with(d,
          external_img(file.path("output/graficos", archivo),
                       width = ancho, height = alto),
          location = ph_location(left, top, ancho, alto))
}

con_texto <- function(d, txts, left, top, ancho, alto, size = 16) {
  ph_with(d, parrafos_de(txts, size, colores$texto),
          location = ph_location(left, top, ancho, alto))
}

# --- Tablas ------------------------------------------------------------------

set_flextable_defaults(font.family = fuente_cuerpo, font.size = 12,
                       padding = 4, border.color = colores$grilla)

tabla_de <- function(df, anchos, encabezados = NULL, size = 12,
                     encabezado = TRUE, alto_fila = 4) {
  ft <- flextable(df)
  if (!is.null(encabezados)) ft <- set_header_labels(ft, values = encabezados)
  ft <- ft |>
    font(fontname = fuente_cuerpo, part = "all") |>
    fontsize(size = size, part = "all") |>
    color(color = colores$texto, part = "all") |>
    bg(bg = colores$fondo, part = "all") |>
    bg(i = seq(2, nrow(df), by = 2), bg = colores$fila_alt, part = "body") |>
    border_remove()

  if (encabezado) {
    ft <- ft |>
      bold(part = "header") |>
      hline_bottom(part = "header",
                   border = fp_border(color = colores$secundario, width = 1))
  } else {
    # La ficha de la base no necesita encabezado: sus dos columnas son
    # etiqueta y valor, y una fila de títulos vacía solo ocupa lugar.
    ft <- delete_part(ft, part = "header")
  }

  ft |>
    padding(padding.top = alto_fila, padding.bottom = alto_fila,
            padding.left = 6, padding.right = 6, part = "all") |>
    valign(valign = "center", part = "all") |>
    width(width = anchos) |>
    fix_border_issues()
}

con_tabla <- function(d, ft, left, top) {
  ph_with(d, ft, location = ph_location(left, top))
}


# =============================================================================
# 3. LOS DATOS QUE VAN A LAS DIAPOSITIVAS
# =============================================================================

# Las tablas ya vienen con los números formateados a la argentina desde el
# script 04. Hay que leerlas como TEXTO: si dejamos que readr adivine el tipo,
# interpreta "80,03" como ocho mil tres y "23.000" como veintitrés, porque
# asume el punto como separador decimal. Los números de la presentación
# quedarían mal sin que nada falle.
leer <- function(x) {
  read_csv(file.path("output/tablas", x),
           col_types = cols(.default = col_character()))
}

titulos      <- leer("03_titulos_graficos.csv")
ficha        <- leer("04_ficha_base.csv")
var_vivienda <- leer("04_variables_vivienda.csv")
var_ubicacion<- leer("04_variables_ubicacion.csv")
recorte      <- leer("04_evidencia_recorte.csv")

t_de <- function(id) titulos$titulo[titulos$id == id]
b_de <- function(id) titulos$bajada[titulos$id == id]

encabezado_vars <- list(
  variable = "Variable", descripcion = "Descripción", tipo = "Tipo",
  valores = "Valores que toma", nulos = "Valores nulos"
)
anchos_vars <- c(1.75, 3.95, 1.55, 2.60, 1.95)


# =============================================================================
# 4. LAS DIAPOSITIVAS
# =============================================================================

ppt <- read_pptx(ruta_plantilla)

# --- 1. Carátula -------------------------------------------------------------

ppt <- nueva_slide(ppt)
ppt <- ph_with(ppt,
  texto_de("¿Qué pesa más en el precio de una vivienda: la ubicación o los metros cuadrados?",
           40, colores$texto, bold = TRUE, familia = fuente_titulo,
           interlineado = 1.05),
  location = ph_location(MARGEN, 1.20, 11.3, 2.0))
ppt <- ph_with(ppt,
  texto_de("Análisis exploratorio de 39.305 avisos de venta de departamentos en la Ciudad de Buenos Aires",
           16, colores$secundario),
  location = ph_location(MARGEN, 3.30, 10.8, 0.5))
ppt <- ph_with(ppt,
  parrafos_de(c(
    "Trabajo Práctico, instancia 1",
    "Ciencia de Datos para Economía y Negocios",
    "Facultad de Ciencias Económicas, Universidad de Buenos Aires",
    "Docente: Nicolás Sidicaro"), 16, colores$texto),
  location = ph_location(MARGEN, 4.25, 10.8, 1.75))
ppt <- ph_with(ppt,
  parrafos_de(c("Joaquín Kweitel, registro 916425",
                "Sofía Colombo, registro 916997",
                FECHA), 16, colores$secundario),
  location = ph_location(MARGEN, 6.05, 10.8, 1.2))


# --- 2. La base --------------------------------------------------------------

ppt <- nueva_slide(ppt) |>
  con_titulo("Cada fila es un aviso publicado, no una vivienda",
             "Publicaciones de venta de departamentos en CABA, relevadas de MercadoLibre en noviembre de 2024")
ppt <- con_tabla(ppt,
  tabla_de(ficha, anchos = c(2.45, 9.35), size = 12, encabezado = FALSE),
  left = MARGEN, top = TOP_CUERPO)
ppt <- con_texto(ppt, c(
  "Chequeamos que una misma vivienda puede ocupar varias filas. En 683 conjuntos de avisos el título, el precio y la superficie coinciden y las direcciones no identifican unidades distintas: son republicaciones. De cada conjunto nos quedamos con un aviso y descartamos el resto, 863 copias en total, y así llegamos a 39.305 avisos.",
  "Conservamos los 104 conjuntos donde las direcciones sí señalan departamentos diferentes de un mismo edificio. Las medianas prácticamente no se mueven al sacarlas: la decisión es conceptual, no numérica."),
  left = MARGEN, top = 5.35, ancho = 11.0, alto = 1.6, size = 15)
ppt <- con_numero(ppt, 2)


# --- 3. Variables de la vivienda ---------------------------------------------

ppt <- nueva_slide(ppt) |>
  con_titulo("Las nueve variables que describen el departamento",
             "De las 45 que trae la base usamos 18. Dejamos afuera las distancias a comisarías, escuelas y colectivos.")
ppt <- con_tabla(ppt,
  tabla_de(var_vivienda, anchos = anchos_vars,
           encabezados = encabezado_vars, size = 10.5, alto_fila = 3),
  left = MARGEN, top = TOP_CUERPO + 0.15)
ppt <- con_texto(ppt, c(
  "Describimos la antigüedad con antiguedad_grupo y no con antiguedad_cat; la diapositiva siguiente muestra por qué."),
  left = MARGEN, top = 6.25, ancho = 11.0, alto = 0.7, size = 14)
ppt <- con_numero(ppt, 3)


# --- 4. Variables de ubicación -----------------------------------------------

ppt <- nueva_slide(ppt) |>
  con_titulo("Las nueve variables que ubican el aviso en la ciudad",
             "Una sola tiene valores nulos, y esos nulos no son datos faltantes.")
ppt <- con_tabla(ppt,
  tabla_de(var_ubicacion, anchos = anchos_vars,
           encabezados = encabezado_vars, size = 10.5, alto_fila = 3),
  left = MARGEN, top = TOP_CUERPO + 0.15)
ppt <- con_texto(ppt, c(
  "Los 28.745 nulos de inmobiliaria coinciden uno a uno con los avisos de particulares: comparamos los índices de las dos variables y son exactamente las mismas filas. No hay que imputarlos ni descartarlos, hay que leerlos como una categoría más."),
  left = MARGEN, top = 6.25, ancho = 11.0, alto = 0.8, size = 14)
ppt <- con_numero(ppt, 4)


# --- 5. Las categorías de antigüedad se pisan --------------------------------

ppt <- nueva_slide(ppt) |> con_titulo(t_de("g5"), b_de("g5"))
ppt <- con_grafico(ppt, "g5_antiguedad_solapada.png",
                   left = 2.04, top = TOP_CUERPO, ancho = 9.26, alto = 4.35)
ppt <- con_texto(ppt, c(
  "Cuatro categorías arrancan en el mismo año y varias se pisan entre sí. No clasifican la antigüedad: registran qué filtro de búsqueda usó quien publicó el aviso.",
  "Los 4.588 nulos de antiguedad_max caen todos en «Más de 50 años», que es abierta: también son estructurales."),
  left = MARGEN, top = 6.22, ancho = 11.0, alto = 0.95, size = 14)
ppt <- con_numero(ppt, 5)


# --- 6. El mapa --------------------------------------------------------------

ppt <- nueva_slide(ppt) |> con_titulo(t_de("g1"), b_de("g1"))
ppt <- con_grafico(ppt, "g1_mapa_precio_m2.png",
                   left = 0.9, top = TOP_CUERPO, ancho = 3.82, alto = 4.95)
ppt <- con_texto(ppt, c(
  "El corredor más caro no rodea al centro: sube por el norte, de Palermo a Belgrano, Núñez y Villa Urquiza.",
  "El sur y el sudoeste quedan parejos y abajo, sin un gradiente propio.",
  "La mancha oscura suelta sobre el margen este es Puerto Madero, que se comporta como un submercado aparte.",
  "Cada celda resume los avisos publicados en esa manzana, no las viviendas que hay ahí."),
  left = 5.2, top = 2.05, ancho = 7.3, alto = 4.5, size = 16)
ppt <- con_numero(ppt, 6)


# --- 7. El ranking de barrios ------------------------------------------------

ppt <- nueva_slide(ppt) |> con_titulo(t_de("g2"), b_de("g2"))
ppt <- con_grafico(ppt, "g2_ranking_barrios.png",
                   left = 2.28, top = 1.78, ancho = 8.77, alto = 4.65)
ppt <- con_texto(ppt, c(
  "Puerto Madero se despega con 5.676, contra 3.553 de Palermo. En el otro extremo, Villa Lugano, Nueva Pompeya y la Boca no llegan a 1.600. Villa Soldati aparece sexto con 2.972, pero 19 de sus 42 avisos declaran zona Núñez y tienen direcciones sin calle: el geocodificador los ubicó ahí igual."),
  left = MARGEN, top = 6.50, ancho = 11.0, alto = 0.8, size = 13)
ppt <- con_numero(ppt, 7)


# --- 8. No es la distancia al centro -----------------------------------------

ppt <- nueva_slide(ppt) |> con_titulo(t_de("g3"), b_de("g3"))
ppt <- con_grafico(ppt, "g3_norte_sur_distancia.png",
                   left = 2.04, top = TOP_CUERPO, ancho = 9.26, alto = 4.35)
ppt <- con_texto(ppt, c(
  "La distancia al centro casi no ordena los precios: su correlación con el precio por m² es 0,07. La latitud llega a 0,49.",
  "La brecha se mantiene en los cuatro tramos de distancia. El eje que organiza el precio en CABA no es centro y periferia, es norte y sur."),
  left = MARGEN, top = 6.22, ancho = 11.0, alto = 0.95, size = 14)
ppt <- con_numero(ppt, 8)


# --- 9. Superficie y precio --------------------------------------------------

ppt <- nueva_slide(ppt) |>
  con_titulo("La oferta se concentra en superficies chicas, y ahí el tamaño casi no mueve el precio por m²",
             "Distribución de la superficie cubierta y relación entre superficie y precio publicado.")
ppt <- con_grafico(ppt, "g8_superficie.png",
                   left = 0.95, top = TOP_CUERPO, ancho = 5.37, alto = 4.05)
ppt <- con_grafico(ppt, "g4_superficie_precio.png",
                   left = 6.95, top = TOP_CUERPO, ancho = 5.37, alto = 4.05)
ppt <- con_texto(ppt, c(
  "La mitad de los avisos está entre 38 y 78 m² y la mediana es 53. Es un rango estrecho, y eso ayuda a entender el contraste que sigue: el precio total acompaña al tamaño con una correlación de 0,80, pero el precio por m² casi no lo hace, con 0,02.",
  "Del precio por m², el 36,8% de la variación está entre barrios y el 63,2% queda dentro de cada barrio."),
  left = MARGEN, top = 5.95, ancho = 11.0, alto = 1.1, size = 14)
ppt <- con_numero(ppt, 9)


# --- 10. La antigüedad no explica la brecha ----------------------------------

ppt <- nueva_slide(ppt) |> con_titulo(t_de("g6"), b_de("g6"))
ppt <- con_grafico(ppt, "g6_antiguedad_mitad.png",
                   left = 2.04, top = TOP_CUERPO, ancho = 9.26, alto = 4.35)
ppt <- con_texto(ppt, c(
  "La antigüedad no explica la brecha: aparece en los cuatro tramos. Es más grande entre los edificios de más de 50 años, con 47%, que entre los a estrenar, con 33%.",
  "La brecha crece con la antigüedad. No podemos decir por qué con lo que tenemos, y es una de las preguntas que abrimos para la instancia 2."),
  left = MARGEN, top = 6.22, ancho = 11.0, alto = 0.95, size = 14)
ppt <- con_numero(ppt, 10)


# --- 11. La base llega recortada ---------------------------------------------

ppt <- nueva_slide(ppt) |>
  con_titulo("La base nos llega ya podada, y eso limita lo que podemos decir",
             "Extremos de cada variable en la base cruda, antes de cualquier transformación nuestra.")
ppt <- con_tabla(ppt,
  tabla_de(recorte, anchos = c(2.15, 1.20, 1.45, 1.70, 5.30), size = 12.5),
  left = MARGEN, top = TOP_CUERPO + 0.25)
ppt <- con_texto(ppt, c(
  "Los máximos caen en valores exactos y por encima no hay nada. No podemos escribir que la base no tiene valores atípicos: podemos escribir que los que había fueron sacados antes de llegarnos, y que no sabemos cuántos eran.",
  "Aun así, el 8,4% de los avisos supera el límite de 1,5 veces el rango intercuartílico en precio. Por debajo no hay ninguno, y no puede haberlos: ese límite cae por debajo de cero. La cola derecha sigue ahí, recortada."),
  left = MARGEN, top = 4.85, ancho = 11.0, alto = 1.8, size = 15)
ppt <- con_numero(ppt, 11)


# --- 12. Preguntas y cierre --------------------------------------------------

ppt <- nueva_slide(ppt) |>
  con_titulo("Dos preguntas que quedan abiertas",
             "Son las que nos gustaría trabajar en la instancia 2.")
ppt <- con_texto(ppt, c(
  "La brecha norte-sur se sostiene a igual distancia del centro y dentro de cada tramo de antigüedad. ¿Qué la sostiene entonces? Queremos ver si alguna de las variables de entorno que dejamos afuera la acompaña, o si hace falta información que la base no tiene.",
  "El 63,2% de la variación del precio por m² queda dentro de cada barrio, no entre barrios. ¿Cuánto de eso viene de características de la vivienda y cuánto de diferencias internas del propio barrio? La respuesta cambia qué tan lejos puede llegar un análisis que use el barrio como unidad."),
  left = MARGEN, top = TOP_CUERPO + 0.2, ancho = 11.3, alto = 3.2, size = 17)
ppt <- ph_with(ppt,
  texto_de("Gracias", 30, colores$texto, bold = TRUE, familia = fuente_titulo),
  location = ph_location(MARGEN, 5.60, 6.0, 0.6))
ppt <- ph_with(ppt,
  texto_de("Joaquín Kweitel y Sofía Colombo", 16, colores$secundario),
  location = ph_location(MARGEN, 6.30, 8.0, 0.4))
ppt <- con_numero(ppt, 12)


# =============================================================================
# 5. GUARDADO
# =============================================================================

# Escribe un BORRADOR, no la entrega. El archivo maestro es
# TP_instancia1_Kweitel_Colombo.pptx, está en la raíz del proyecto y se edita
# aparte. Este script no tiene que poder pisarlo ni por accidente.
salida <- "output/graficos/borrador_desde_R.pptx"
print(ppt, target = salida)

cat("Diapositivas:", length(ppt), "\n")
cat("Archivo:", salida, "-",
    round(file.size(salida) / 1024), "KB\n")
