# ajustar_pie(): vuelve a envolver el pie de un gráfico si no entra en el ancho.
#
# Los pies se envuelven a 125 caracteres, que entran en una imagen de 9
# pulgadas pero no en una más angosta: ahí el texto se corta contra el borde.
# Antes de guardar, si alguna línea del pie es más larga de lo que entra en el
# ancho real, se vuelve a envolver. chars_pulgada sale de medir los pies que sí
# entraban (unos 15 caracteres por pulgada en los gráficos, 17 en las tablas,
# que usan letra más chica). La usan guardar() y guardar_tabla().

ajustar_pie <- function(p, ancho, chars_pulgada) {
  pie_actual <- p$labels$caption
  if (is.null(pie_actual)) return(p)
  limite <- floor(chars_pulgada * (ancho - 0.6))
  if (max(nchar(strsplit(pie_actual, "\n")[[1]])) <= limite) return(p)
  p + labs(caption = envolver(gsub("\n", " ", pie_actual), limite))
}
