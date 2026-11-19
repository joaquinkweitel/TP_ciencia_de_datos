# gini(): coeficiente de Gini de un vector de cantidades.
#
# El doble del área entre la diagonal y la curva de Lorenz, calculada con
# trapecios sobre curva_lorenz(). Va de 0 (todas las unidades iguales) a
# (N − 1) / N (una sola unidad tiene todo). Con unidades del mismo peso es
# igual a la diferencia media de Gini: sum(|xi − xj|) / (2 N² media).
# La usa el script 12.

gini <- function(x) {
  l <- curva_lorenz(x)
  1 - sum(diff(l$prop_unidades) * (head(l$prop_acumulada, -1) + l$prop_acumulada[-1]))
}
