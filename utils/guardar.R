# guardar(): guarda un gráfico en output/graficos/ como PNG.
#
# Un solo lugar donde se fija el tamaño y la resolución de los gráficos de las
# instancias 1 y 2. Los dos JPG de la instancia 3 tienen su propio guardado en
# el script 11, porque la consigna pide otro formato y otro tamaño.
#
# Con MOSTRAR_NOMBRES en TRUE (script/00_tema.R) el gráfico lleva arriba su
# nombre descriptivo (auxiliar/nombres_figuras.csv). La imagen se agranda lo
# que ocupa el nombre, así el gráfico conserva su tamaño.

guardar <- function(grafico, nombre, ancho = 9, alto = 5) {
  grafico <- ajustar_pie(grafico, ancho, chars_pulgada = 15)
  if (MOSTRAR_NOMBRES) {
    grafico <- poner_nombre(grafico, nombre, ancho, size = 15)
    alto <- alto + 0.32 * attr(grafico, "lineas_nombre") + 0.15
  }
  ggsave(
    filename = file.path("output/graficos", nombre),
    plot     = grafico,
    width    = ancho, height = alto, dpi = 300,
    bg       = colores$fondo,
    device   = ragg::agg_png
  )
  cat("guardado:", nombre, "\n")
}
