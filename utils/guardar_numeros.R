# guardar_numeros(): guarda en output/tablas/ los números clave de un script.
#
# Cada script guarda los números que van a los textos de la presentación, y el
# script 10 los lee de ahí (con leer_numeros()). Así ningún número de una
# diapositiva se escribe a mano: escribirlos a mano fue lo que en la instancia
# 1 nos hizo mezclar la base cruda con la de análisis.
#
#   guardar_numeros("08_numeros.csv", r2 = 0.88, brecha = 2.97)

guardar_numeros <- function(archivo, ...) {
  x <- list(...)
  stopifnot(!is.null(names(x)), all(names(x) != ""), all(lengths(x) == 1))
  readr::write_csv(tibble::tibble(nombre = names(x), valor = unname(unlist(x))),
                   file.path("output/tablas", archivo))
  cat("números guardados:", archivo, "\n")
}
