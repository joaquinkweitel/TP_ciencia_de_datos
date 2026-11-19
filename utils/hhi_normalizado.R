# hhi_normalizado(): el HHI llevado a una escala de 0 a 1 que no depende de
# cuántas unidades hay.
#
# El mínimo del HHI es 1/N, así que comparar el HHI de 48 barrios con el de 42
# mezcla la concentración con la cantidad de barrios. (HHI − 1/N) / (1 − 1/N)
# vale 0 si todas las unidades son iguales y 1 si una sola tiene todo, con
# cualquier N. Usa hhi(). La usa el script 12.

hhi_normalizado <- function(x) (hhi(x) - 1 / length(x)) / (1 - 1 / length(x))
