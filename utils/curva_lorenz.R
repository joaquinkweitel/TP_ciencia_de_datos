# curva_lorenz(): los puntos de la curva de Lorenz de un vector de cantidades.
#
# Ordena las unidades de menor a mayor y devuelve, para cada una, la
# proporción acumulada de unidades (x) y la proporción acumulada del total (y),
# empezando en el origen (0, 0). Si la cantidad estuviera repartida en partes
# iguales, la curva sería la diagonal. La usan gini() y el script 12.

curva_lorenz <- function(x) {
  x <- sort(x)
  tibble::tibble(prop_unidades  = c(0, seq_along(x) / length(x)),
                 prop_acumulada = c(0, cumsum(x) / sum(x)))
}
