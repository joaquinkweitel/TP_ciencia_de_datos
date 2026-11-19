# coma_ar(): un número con decimales, a la argentina (coma decimal, punto de
# miles). Para porcentajes, coeficientes y correlaciones.
#
# Los formateadores sin decimales (num_ar, usd_ar, usd_mil) no están acá: son
# configuraciones de scales::label_number() y viven en script/00_tema.R.

coma_ar <- function(x, dec = 1) {
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}
