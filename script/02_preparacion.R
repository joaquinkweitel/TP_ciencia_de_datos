# =============================================================================
# TP Ciencia de Datos para Economía y Negocios - FCE-UBA
# Instancia 1: Análisis Exploratorio de Datos
# Script 02: preparación de la base de análisis
#
# Joaquín Kweitel (916425) - Sofía Colombo (916997)
#
# Entrada : raw/base_ventas_caba.csv   (nunca se modifica)
# Salida  : input/base_analisis.csv
#           output/tablas/02_efecto_deduplicacion.csv
#
# Este script hace tres cosas y nada más:
#   1. Separa los avisos repetidos en republicaciones y unidades distintas.
#   2. Elimina las copias redundantes de las republicaciones.
#   3. Agrega las pocas variables derivadas que usamos en los gráficos.
# =============================================================================

library(tidyverse)

crudo <- read_csv("raw/base_ventas_caba.csv", show_col_types = FALSE)


# =============================================================================
# 1. AVISOS REPETIDOS: REPUBLICACIÓN O UNIDADES DISTINTAS
# =============================================================================

# El problema. Hay 787 conjuntos de avisos que comparten título, precio y
# superficie pero tienen id y url propios. Mirando las direcciones, no son
# todos lo mismo:
#
#   "Montañeses Y Av. Olazabal - Unidad 105"   \
#   "Montañeses Y Av. Olazabal - Unidad 103"    >  tres viviendas distintas
#   "Montañeses Y Av. Olazabal - Unidad 106"   /   del mismo edificio
#
#   "Viamonte 2070, Barrio Norte, Capital Federal"   \
#   "Viamonte 2070, Palermo, Capital Federal"         >  una sola vivienda,
#   "Viamonte 2070, Recoleta, Capital Federal"       /   seis etiquetas de barrio
#
#   "Bernardo De Irigoyen Al 1100, San Telmo"   \  una sola vivienda publicada
#   "Carlos Calvo Al 200, San Telmo"            /  con dos direcciones distintas
#
# El criterio. Dos avisos con el mismo título, precio y superficie son la misma
# vivienda salvo que sus direcciones señalen unidades diferentes. Buscamos en
# la dirección un identificador de unidad (piso, departamento, "Unidad NNN") y
# comparamos:
#
#   - el grupo tiene dos o más identificadores de unidad distintos -> son
#     viviendas distintas del mismo edificio, las conservamos todas;
#   - el grupo no los tiene, o tiene uno solo -> es la misma vivienda
#     republicada, conservamos un aviso y descartamos las copias.
#
# Límite del criterio, que conviene tener presente. Algunas direcciones escriben
# la unidad sin marca reconocible ("Vedia 4700 4C", "Av. Cramer 2200 11C"): esos
# grupos los colapsamos aunque sean unidades distintas. Y a la inversa, un mismo
# departamento escrito de dos formas ("| 7° 03" y "- Piso 7 - Unidad 03") lo
# leemos como dos unidades. Son pocos casos y los errores van en las dos
# direcciones. No afectan ningún resultado, como se ve en el punto 3.

normalizar <- function(x) {
  x |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_squish()
}

# Identificadores de unidad que aparecen efectivamente en la base:
#   "UNIDAD 105", "UNIDAD 5A", "PISO 4", "4° 02", "6° DTO B", "3o PISO", "12°"
patron_unidad <- paste(
  "UNIDAD\\s*[A-Z0-9]+",
  "PISO\\s*[A-Z0-9]+",
  "[0-9]+\\s*[º°]\\s*[A-Z0-9]*",
  "[º°]\\s*[A-Z0-9]+",
  "DTO\\.?\\s*[A-Z0-9]+",
  "DEPTO\\.?\\s*[A-Z0-9]+",
  sep = "|"
)

repetidos <- crudo |>
  mutate(
    # Nos quedamos con el tramo de la dirección anterior al sufijo de barrio.
    dir_pre = normalizar(str_split_i(direccion, ",", 1)),
    unidad  = str_extract(dir_pre, patron_unidad)
  ) |>
  group_by(titulo_aviso, precio_usd, metros_cubiertos) |>
  mutate(
    copias           = n(),
    unidades_dist    = n_distinct(unidad[!is.na(unidad)]),
    direcciones_dist = n_distinct(dir_pre),
    completas_dist   = n_distinct(direccion),
    barrios_dist     = n_distinct(barrio),
    tipo_repeticion = case_when(
      copias == 1          ~ "Aviso único",
      unidades_dist >= 2   ~ "Unidades distintas del mismo edificio",
      direcciones_dist > 1 ~ "Republicación: calle o altura distinta",
      completas_dist > 1   ~ "Republicación: misma calle, distinto barrio en el texto",
      TRUE                 ~ "Republicación: dirección idéntica"
    ),
    # Dentro de cada grupo de republicación conservamos el aviso de id más bajo.
    orden_en_grupo = rank(id, ties.method = "first")
  ) |>
  ungroup() |>
  mutate(
    copia_redundante = str_starts(tipo_repeticion, "Republicación") &
                       orden_en_grupo > 1
  )

# Resumen de la clasificación, a nivel grupo y a nivel aviso.
resumen_repeticion <- repetidos |>
  filter(copias > 1) |>
  group_by(titulo_aviso, precio_usd, metros_cubiertos) |>
  summarise(tipo = first(tipo_repeticion), copias = first(copias),
            barrios_dist = first(barrios_dist), .groups = "drop") |>
  group_by(tipo) |>
  summarise(grupos = n(), avisos = sum(copias),
            copias_redundantes = sum(copias) - n(),
            grupos_en_varios_barrios = sum(barrios_dist > 1),
            .groups = "drop") |>
  arrange(desc(avisos))

print(resumen_repeticion, width = Inf)
write_csv(resumen_repeticion, "output/tablas/02_clasificacion_repetidos.csv")


# =============================================================================
# 2. BASE DE ANÁLISIS
# =============================================================================

# Nombres de barrio en minúscula y con tildes, para que los gráficos se lean.
# En la base vienen en mayúscula y sin acentos.
nombres_barrio <- c(
  "AGRONOMIA" = "Agronomía", "ALMAGRO" = "Almagro", "BALVANERA" = "Balvanera",
  "BARRACAS" = "Barracas", "BELGRANO" = "Belgrano", "BOCA" = "Boca",
  "BOEDO" = "Boedo", "CABALLITO" = "Caballito", "CHACARITA" = "Chacarita",
  "COGHLAN" = "Coghlan", "COLEGIALES" = "Colegiales",
  "CONSTITUCION" = "Constitución", "FLORES" = "Flores", "FLORESTA" = "Floresta",
  "LINIERS" = "Liniers", "MATADEROS" = "Mataderos", "MONSERRAT" = "Monserrat",
  "MONTE CASTRO" = "Monte Castro", "NUEVA POMPEYA" = "Nueva Pompeya",
  "NUÑEZ" = "Núñez", "PALERMO" = "Palermo",
  "PARQUE AVELLANEDA" = "Parque Avellaneda",
  "PARQUE CHACABUCO" = "Parque Chacabuco", "PARQUE CHAS" = "Parque Chas",
  "PARQUE PATRICIOS" = "Parque Patricios", "PATERNAL" = "Paternal",
  "PUERTO MADERO" = "Puerto Madero", "RECOLETA" = "Recoleta",
  "RETIRO" = "Retiro", "SAAVEDRA" = "Saavedra",
  "SAN CRISTOBAL" = "San Cristóbal", "SAN NICOLAS" = "San Nicolás",
  "SAN TELMO" = "San Telmo", "VELEZ SARSFIELD" = "Vélez Sarsfield",
  "VERSALLES" = "Versalles", "VILLA CRESPO" = "Villa Crespo",
  "VILLA DEL PARQUE" = "Villa del Parque", "VILLA DEVOTO" = "Villa Devoto",
  "VILLA GRAL. MITRE" = "Villa Gral. Mitre", "VILLA LUGANO" = "Villa Lugano",
  "VILLA LURO" = "Villa Luro", "VILLA ORTUZAR" = "Villa Ortúzar",
  "VILLA PUEYRREDON" = "Villa Pueyrredón", "VILLA REAL" = "Villa Real",
  "VILLA RIACHUELO" = "Villa Riachuelo", "VILLA SANTA RITA" = "Villa Santa Rita",
  "VILLA SOLDATI" = "Villa Soldati", "VILLA URQUIZA" = "Villa Urquiza"
)

stopifnot(all(unique(crudo$barrio) %in% names(nombres_barrio)))

base <- repetidos |>
  filter(!copia_redundante) |>
  mutate(
    barrio_nom = unname(nombres_barrio[barrio]),

    # Eje norte-sur. Partimos la ciudad por la latitud mediana de los avisos:
    # el corte deja la misma cantidad de avisos de cada lado y no depende de
    # ningún criterio nuestro sobre dónde empieza el norte.
    mitad_ciudad = factor(
      if_else(lat > median(lat), "Norte", "Sur"),
      levels = c("Sur", "Norte")
    ),

    # Antigüedad: usamos los cuatro tramos que ya trae la base, sin las letras
    # que ordenan las categorías.
    antiguedad = factor(
      str_remove(antiguedad_grupo, "^[a-d]\\. "),
      levels = c("A estrenar", "Hasta 20 años", "20 a 50 años", "Más de 50 años")
    ),

    tipo_depto = factor(
      tipo_depto,
      levels = c("monoambiente", "1 dormitorio", "2 dormitorios",
                 "3 dormitorios", "4 dormitorios o más"),
      labels = c("Monoambiente", "1 dormitorio", "2 dormitorios",
                 "3 dormitorios", "4 dormitorios o más")
    ),

    publicado_por = factor(publicado_por,
                           levels = c("particular", "inmobiliaria"),
                           labels = c("Particular", "Inmobiliaria")),

    dist_centro_km = dist_centro_m / 1000,

    tramo_m2 = cut(
      metros_cubiertos,
      breaks = c(14, 35, 50, 70, 100, 150, 400),
      labels = c("Hasta 35 m²", "36 a 50 m²", "51 a 70 m²",
                 "71 a 100 m²", "101 a 150 m²", "Más de 150 m²")
    )
  ) |>
  select(-dir_pre, -unidad, -copias, -unidades_dist, -direcciones_dist,
         -completas_dist, -barrios_dist, -orden_en_grupo, -copia_redundante)

# Avisos por barrio: lo usamos para señalar en los gráficos los barrios cuya
# mediana se calcula sobre pocos casos.
base <- base |>
  group_by(barrio_nom) |>
  mutate(avisos_barrio = n()) |>
  ungroup()


# =============================================================================
# 3. ¿CAMBIA ALGO SACAR LAS COPIAS? (LA RESPUESTA ES NO, Y HAY QUE MOSTRARLO)
# =============================================================================

descriptivos <- function(datos, etiqueta) {
  datos |>
    summarise(
      version              = etiqueta,
      avisos               = n(),
      precio_medio         = round(mean(precio_usd)),
      precio_mediano       = median(precio_usd),
      precio_m2_medio      = round(mean(precio_m2_usd), 1),
      precio_m2_mediano    = round(median(precio_m2_usd), 1),
      m2_mediano           = median(metros_cubiertos),
      desvio_precio_m2     = round(sd(precio_m2_usd), 1),
      pct_palermo          = round(100 * mean(barrio == "PALERMO"), 2),
      pct_inmobiliaria     = round(100 * mean(publicado_por == "inmobiliaria" |
                                              publicado_por == "Inmobiliaria"), 2),
      pct_a_estrenar       = round(100 * mean(a_estrenar), 2)
    )
}

efecto <- bind_rows(
  descriptivos(crudo, "Base cruda"),
  descriptivos(base,  "Base de análisis"),
  descriptivos(crudo |> distinct(titulo_aviso, precio_usd, metros_cubiertos,
                                 .keep_all = TRUE),
               "Sin ningún repetido (referencia)")
)

print(efecto, width = Inf)
write_csv(efecto, "output/tablas/02_efecto_deduplicacion.csv")

cat("\nAvisos en la base cruda     :", nrow(crudo), "\n")
cat("Copias redundantes quitadas :", nrow(crudo) - nrow(base), "\n")
cat("Avisos en la base de análisis:", nrow(base), "\n")


# =============================================================================
# 4. GUARDADO
# =============================================================================

write_csv(base, "input/base_analisis.csv")
cat("\nGuardado en input/base_analisis.csv\n")
