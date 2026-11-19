# nombre_figura(): el nombre descriptivo de una figura, según su archivo.
#
# Los nombres están en auxiliar/nombres_figuras.csv, un solo lugar para todas
# las figuras de la presentación. El nombre dice qué es la figura, no cuál es
# el hallazgo: el hallazgo lo afirma el título de la diapositiva. Si una figura
# no tiene nombre, se frena: ninguna figura sale sin él.

nombre_figura <- function(archivo) {
  nombres <- readr::read_csv("auxiliar/nombres_figuras.csv", show_col_types = FALSE)
  nombre <- nombres$nombre[nombres$archivo == archivo]
  if (length(nombre) != 1) {
    stop("La figura ", archivo, " no tiene nombre en auxiliar/nombres_figuras.csv")
  }
  nombre
}
