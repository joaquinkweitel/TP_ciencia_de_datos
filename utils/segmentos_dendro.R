# segmentos_dendro(): las coordenadas de un dendrograma, para dibujarlo con
# ggplot (geom_segment) a partir de un objeto hclust.
#
# Se dibuja con ggplot y no con plot() para que use el mismo tema que el
# resto. Cada hoja va en la posición que le da el orden del dendrograma
# (h$order); cada unión es una horquilla de tres segmentos a su altura.
# Devuelve una tabla con pos, pos_fin, alt y alt_fin. La usa el script 11.

segmentos_dendro <- function(h) {
  n <- length(h$order)
  pos_hoja <- match(seq_len(n), h$order)
  x_union <- numeric(n - 1)
  pos <- function(j) if (j < 0) pos_hoja[-j] else x_union[j]
  alt <- function(j) if (j < 0) 0 else h$height[j]
  purrr::map_dfr(seq_len(n - 1), function(i) {
    a <- h$merge[i, 1]; b <- h$merge[i, 2]
    x_union[i] <<- (pos(a) + pos(b)) / 2
    tibble::tibble(pos = c(pos(a), pos(b), pos(a)), pos_fin = c(pos(a), pos(b), pos(b)),
                   alt = c(alt(a), alt(b), h$height[i]), alt_fin = rep(h$height[i], 3))
  })
}
