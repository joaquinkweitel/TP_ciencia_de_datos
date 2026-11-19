# tabla_png(): dibuja una tabla con ggplot, para exportarla como imagen.
#
# La presentación se arma en Canva, así que las tablas también se entregan como
# PNG con la paleta del trabajo. Las dibujamos con ggplot en lugar de usar un
# paquete de tablas: así usan exactamente los mismos colores y la misma
# tipografía que los gráficos, y no hace falta un navegador para exportarlas.
#
#   df       data.frame ya formateado, con las columnas como texto
#   anchos   ancho relativo de cada columna (se normaliza solo)
#   alinear  "izq" o "der" para cada columna
#   destacar índices de filas que van en terracota (por ejemplo, los totales)
#
# Devuelve el gráfico con el atributo "alto_sugerido" (en pulgadas), que usa
# guardar_tabla().

tabla_png <- function(df, anchos = NULL, alinear = NULL, destacar = integer(0),
                      size = 3.2, size_enc = 3.3, alto_fila = 0.42,
                      pie_tabla = NULL) {
  # check.names = FALSE: sin esto R convierte "z clásico" en "z.clásico".
  df <- as.data.frame(lapply(df, as.character),
                      stringsAsFactors = FALSE, check.names = FALSE)
  n <- nrow(df); k <- ncol(df)

  if (is.null(anchos))  anchos  <- rep(1, k)
  if (is.null(alinear)) alinear <- c("izq", rep("der", k - 1))
  anchos <- anchos / sum(anchos)

  # Bordes izquierdo y derecho de cada columna, en el eje x de 0 a 1.
  der <- cumsum(anchos)
  izq <- der - anchos
  pad <- 0.008
  x_texto <- ifelse(alinear == "izq", izq + pad, der - pad)
  hjust   <- ifelse(alinear == "izq", 0, 1)

  # Una fila por celda, con su posición.
  celdas <- expand.grid(fila = seq_len(n), col = seq_len(k)) |>
    dplyr::mutate(
      texto = mapply(function(f, c) df[f, c], fila, col),
      x     = x_texto[col],
      h     = hjust[col],
      y     = -fila,
      color = ifelse(fila %in% destacar, colores$acento, colores$texto)
    )

  encabezado <- data.frame(texto = names(df), x = x_texto,
                           h = hjust, y = 0)

  # Fondo de filas alternas.
  franjas <- data.frame(fila = seq(2, n, by = 2))

  g <- ggplot() +
    { if (nrow(franjas) > 0)
        geom_rect(data = franjas,
                  aes(xmin = 0, xmax = 1, ymin = -fila - 0.5, ymax = -fila + 0.5),
                  fill = colores$fila_alt) } +
    geom_text(data = encabezado, aes(x, y, label = texto, hjust = h),
              size = size_enc, fontface = "bold", family = fuente_cuerpo,
              color = colores$texto, vjust = 0.5) +
    annotate("segment", x = 0, xend = 1, y = -0.45, yend = -0.45,
             color = colores$secundario, linewidth = 0.4) +
    geom_text(data = celdas, aes(x, y, label = texto, hjust = h, color = color),
              size = size, family = fuente_cuerpo, vjust = 0.5) +
    scale_color_identity() +
    scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
    scale_y_continuous(limits = c(-n - 0.6, 0.45), expand = expansion(0)) +
    labs(caption = if (is.null(pie_tabla)) NULL else pie(pie_tabla)) +
    theme_void(base_size = 14) +
    theme(
      plot.background  = element_rect(fill = colores$fondo, color = NA),
      panel.background = element_rect(fill = colores$fondo, color = NA),
      plot.caption     = element_text(family = fuente_cuerpo, size = 8.5,
                                      color = colores$secundario, hjust = 0),
      plot.caption.position = "plot",
      plot.margin      = margin(10, 12, 8, 12)
    )

  attr(g, "alto_sugerido") <- 0.55 + alto_fila * (n + 1) +
    if (is.null(pie_tabla)) 0 else 0.3
  g
}
