# envolver(): parte un texto largo en líneas de a lo sumo `ancho` caracteres.
#
# Los títulos dicen el hallazgo, así que son largos y se cortan contra el borde
# del gráfico. titulo(), bajada() y pie() (cada una en su archivo) la llaman
# con el ancho por defecto de cada elemento.

envolver <- function(texto, ancho) paste(strwrap(texto, width = ancho),
                                         collapse = "\n")
