# ari(): índice de Rand ajustado entre dos particiones.
#
# mclust NO se carga con library(): tapa count() de dplyr y map() de purrr.
# Por eso la función llama a mclust::adjustedRandIndex() con el nombre
# completo. La usan los scripts 09 y 11.

ari <- function(a, b) mclust::adjustedRandIndex(a, b)
