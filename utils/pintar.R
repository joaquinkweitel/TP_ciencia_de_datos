# pintar(): envuelve un texto en HTML para pintarlo de un color (y en negrita)
# dentro de un título con ggtext::element_markdown().
#
# Es la función de la clase de visualización de Our World in Data
# (clase24_graficos_owid.R). La usa el gráfico comunicativo del script 11.

pintar <- function(texto, color, negrita = TRUE) {
  if (negrita) texto <- paste0("<b>", texto, "</b>")
  sprintf("<span style='color:%s'>%s</span>", color, texto)
}
