# tema_tp(): el tema de ggplot de todos los gráficos del trabajo.
#
# Usa la paleta (colores) y las tipografías (fuente_cuerpo, fuente_titulo)
# que define script/00_tema.R.

tema_tp <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = fuente_cuerpo) +
    theme(
      plot.background  = element_rect(fill = colores$fondo, color = NA),
      panel.background = element_rect(fill = colores$fondo, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = colores$grilla, linewidth = 0.3),
      plot.title       = element_text(family = fuente_titulo, face = "bold",
                                      size = base_size * 1.25,
                                      color = colores$texto, hjust = 0,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base_size * 0.95,
                                      color = colores$secundario, hjust = 0,
                                      margin = margin(b = 12)),
      plot.caption     = element_text(size = base_size * 0.7,
                                      color = colores$secundario, hjust = 0,
                                      margin = margin(t = 10)),
      axis.title       = element_text(color = colores$secundario),
      axis.text        = element_text(color = colores$secundario),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(color = colores$secundario),
      legend.justification = "left",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin      = margin(14, 18, 12, 14)
    )
}
