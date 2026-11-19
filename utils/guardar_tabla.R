# guardar_tabla(): guarda en output/tablas/ una tabla hecha con tabla_png(),
# con el alto calculado a partir de la cantidad de filas.
#
# Con MOSTRAR_NOMBRES en TRUE (script/00_tema.R) la tabla lleva arriba su
# nombre descriptivo (auxiliar/nombres_figuras.csv), y el alto crece lo que
# ocupa el nombre.

guardar_tabla <- function(tabla, nombre, ancho = 9, alto = NULL) {
  tabla <- ajustar_pie(tabla, ancho, chars_pulgada = 17)
  if (is.null(alto)) alto <- attr(tabla, "alto_sugerido")
  if (MOSTRAR_NOMBRES) {
    tabla <- poner_nombre(tabla, nombre, ancho, size = 13)
    alto <- alto + 0.28 * attr(tabla, "lineas_nombre") + 0.15
  }
  ggsave(file.path("output/tablas", nombre), tabla,
         width = ancho, height = alto, dpi = 300,
         bg = colores$fondo, device = ragg::agg_png)
  cat("tabla guardada:", nombre, sprintf("(%.1f x %.1f in)\n", ancho, alto))
}
