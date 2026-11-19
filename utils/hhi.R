# hhi(): índice de Herfindahl-Hirschman de un vector de cantidades.
#
# Suma de las participaciones al cuadrado, en escala 0 a 1 (multiplicado por
# 10.000 da la escala de los organismos de competencia). Con N unidades va de
# 1/N (todas iguales) a 1 (una sola tiene todo). Su inversa, 1/HHI, es el
# "número equivalente": cuántas unidades del mismo tamaño darían la misma
# concentración. La usa el script 12.

hhi <- function(x) {
  s <- x / sum(x)
  sum(s^2)
}
