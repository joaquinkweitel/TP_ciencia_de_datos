# guardar_tabla(): guarda en output/tablas/ una tabla hecha con tabla_png(),
# con el alto calculado a partir de la cantidad de filas.

guardar_tabla <- function(tabla, nombre, ancho = 9, alto = NULL) {
  tabla <- ajustar_pie(tabla, ancho, chars_pulgada = 17)
  if (is.null(alto)) alto <- attr(tabla, "alto_sugerido")
  ggsave(file.path("output/tablas", nombre), tabla,
         width = ancho, height = alto, dpi = 300,
         bg = colores$fondo, device = ragg::agg_png)
  cat("tabla guardada:", nombre, sprintf("(%.1f x %.1f in)\n", ancho, alto))
}
