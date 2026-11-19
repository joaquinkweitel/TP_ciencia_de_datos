# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 1: Análisis Exploratorio de Datos
# Script 00: paleta, tipografía y tema de los gráficos
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Se define una sola vez y se usa en todos los gráficos, para que el conjunto
# se vea como una misma presentación y no como ocho gráficos sueltos.
# Este script no lee datos ni produce salidas: es configuración. Carga las
# funciones propias de utils/ (una por archivo) y define los objetos comunes:
# la paleta, las tipografías, el tema por defecto y el formato de los números.
# =============================================================================

library(ggplot2)
library(scales)

# Las funciones propias: tema_tp(), coma_ar(), envolver(), guardar(), etc.
invisible(lapply(list.files("utils", pattern = "\\.R$", full.names = TRUE), source))

colores <- list(
  fondo       = "#FAF8F5",
  texto       = "#1F2933",
  secundario  = "#6B7280",
  grilla      = "#E0D9D0",
  acento      = "#C0623D",
  acento2     = "#3F6C74",
  apagado     = "#B9B2AA",
  apagado_cl  = "#D3CDC6",   # para señalar categorías con pocos casos
  fila_alt    = "#EDE7E0"
)

# Escala continua y escala divergente, para los gráficos que las necesiten.
escala_continua  <- c("#EDE6DE", "#8C3F22")
escala_divergente <- c(colores$acento2, "#EDE7E0", colores$acento)

# Segoe UI y Georgia vienen instaladas en Windows. Si faltaran, los gráficos
# salen con la tipografía por defecto en lugar de fallar.
fuente_cuerpo <- if (fuente_disponible("Segoe UI")) "Segoe UI" else ""
fuente_titulo <- if (fuente_disponible("Georgia"))  "Georgia"  else ""

theme_set(tema_tp())


# --- Formato de números a la argentina -------------------------------------
# Punto para los miles, coma para los decimales. Los precios con prefijo US$.
# Estos tres son configuraciones de scales::label_number(), no funciones
# propias; la de decimales, coma_ar(), está en utils/coma_ar.R.

num_ar  <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
usd_ar  <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1,
                        prefix = "US$ ")
# Para ejes con valores grandes, en miles de dólares.
usd_mil <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1,
                        scale = 1e-3, prefix = "US$ ", suffix = " mil")


# --- Pie de fuente, igual en todos los gráficos ------------------------------

fuente_pie <- "Elaboración propia en base a avisos de MercadoLibre, noviembre de 2024"
