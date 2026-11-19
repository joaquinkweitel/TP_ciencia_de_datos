# fuente_disponible(): ¿la tipografía está instalada en el sistema?
#
# Segoe UI y Georgia vienen instaladas en Windows. Se verifica que estén antes
# de usarlas: si faltaran, los gráficos salen con la tipografía por defecto en
# lugar de fallar. La usa script/00_tema.R.

fuente_disponible <- function(nombre) {
  nombre %in% systemfonts::system_fonts()$family
}
