# poner_nombre(): agrega a un gráfico o tabla su nombre descriptivo como
# título, sin bajada (la bajada la pone la diapositiva).
#
#   p             el gráfico o la tabla (hecha con tabla_png())
#   archivo       el nombre del archivo, para buscar el nombre de la figura
#   ancho         ancho de la imagen en pulgadas, para cortar el nombre en
#                 líneas que entren
#   size          tamaño de letra del nombre, en puntos
#
# Devuelve el gráfico con el nombre y, en el atributo "lineas_nombre", en
# cuántas líneas quedó, para que guardar() y guardar_tabla() agranden la
# imagen en vez de achicar el gráfico. La usan esas dos funciones cuando
# MOSTRAR_NOMBRES es TRUE (script/00_tema.R).

poner_nombre <- function(p, archivo, ancho, size = 15) {
  # Unos 9 caracteres por pulgada a 15 puntos en Georgia negrita; proporcional
  # para otros tamaños. Se descuenta el margen.
  limite <- floor(9 * 15 / size * (ancho - 0.6))
  nombre <- envolver(nombre_figura(archivo), limite)
  atributos <- attributes(p)[c("alto_sugerido")]
  p <- p +
    labs(title = nombre, subtitle = NULL) +
    theme(plot.title = element_text(family = fuente_titulo, face = "bold", size = size,
                                    color = colores$texto, hjust = 0, lineheight = 1.05,
                                    margin = margin(b = 10)),
          plot.title.position = "plot")
  if (!is.null(atributos$alto_sugerido)) attr(p, "alto_sugerido") <- atributos$alto_sugerido
  attr(p, "lineas_nombre") <- length(strsplit(nombre, "\n")[[1]])
  p
}
