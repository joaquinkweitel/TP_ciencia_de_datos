# leer_numeros(): lee un archivo de números clave guardado con
# guardar_numeros() y lo devuelve como vector con nombres (todo como texto).
#
#   N8 <- leer_numeros("08_numeros.csv"); N8[["brecha_p3"]]

leer_numeros <- function(archivo) {
  x <- readr::read_csv(file.path("output/tablas", archivo), show_col_types = FALSE,
                       col_types = "cc")
  setNames(x$valor, x$nombre)
}
