# Precio de los departamentos en venta en la Ciudad de Buenos Aires

Trabajo práctico de Ciencia de Datos para Economía y Negocios, FCE-UBA,
segundo cuatrimestre de 2026. Docente: Nicolás Sidicaro.

Grupo 8: Joaquín Kweitel (916425) y Sofía Colombo (916997).


## 1. Objetivo e hipótesis de trabajo

Este trabajo estudia de qué depende el precio publicado de un departamento en venta en la Ciudad de Buenos Aires: cuánto se asocia con lo que es el departamento —superficie, ambientes, baños, antigüedad— y cuánto con dónde está.

Hipótesis. Dos departamentos de características equivalentes, con la misma superficie, los mismos ambientes, los mismos baños y la misma antigüedad, tienen precios sistemáticamente distintos según el barrio en el que se publican, y esa diferencia es grande en relación con lo que aporta el tamaño.

Los resultados la apoyan y la reformulan. La apoyan: a igual departamento, el precio pedido en el barrio más caro es 3,0 veces el del más barato. Y la reformulan, porque la pregunta de cuál de las dos variables pesa más estaba mal planteada: no compiten. La superficie está contenida en el precio total por construcción, y una vez que se divide el precio por los metros apenas aporta nada, 0,08 puntos de R². El barrio es el que da cuenta de esa variación: 26,7 puntos.


## 2. La base de datos y por qué la elegimos

Avisos de venta de departamentos de MercadoLibre Argentina, relevados en noviembre de 2024: 40.168 avisos en la base cruda y 39.305 después de quitar las republicaciones, distribuidos en los 48 barrios de la ciudad. Los modelos trabajan sobre 38.895 avisos: los de los 42 barrios con al menos 100 publicaciones, sin los 40 avisos que no son departamentos.

La elegimos porque reúne en una misma fila el precio, las características del departamento y la ubicación —barrio, comuna y coordenadas—, que es exactamente lo que hace falta para separar el peso de una cosa del peso de la otra. Y porque tiene avisos suficientes para comparar barrios entre sí con cierta solidez.

Lo que la base es y lo que no: representa la oferta publicada, no el stock de viviendas ni las operaciones cerradas; los precios son los pedidos, no los pactados; y es un corte de un solo mes, sin dimensión temporal.


## 3. Qué hicimos

El trabajo se hizo en tres entregas, y el código está numerado en ese orden.

**Instancia 1: análisis exploratorio (scripts 01 a 05).** Diagnóstico de la
base cruda: estructura, nulos, avisos repetidos, coherencia entre variables.
Separamos los avisos repetidos en republicaciones de un mismo departamento,
que se eliminan, y unidades distintas de un mismo edificio, que se conservan.
Con la base de análisis armamos los gráficos y las tablas descriptivas.

**Instancia 2: métodos estadísticos (scripts 06 a 10).**

- Valores faltantes y atípicos: tres reglas (IQR, z clásico, z robusto) en
  dos escalas, y revisión caso por caso de lo que marcan.
- Las correcciones que pidió la devolución de la instancia 1: descriptivas de
  todas las variables, avisos por barrio, y un PCA de las variables de
  cercanía para justificar por qué no las usamos.
- Regresión lineal del logaritmo del precio en cuatro pasos (superficie,
  características, barrio, entorno), con el sesgo por variable omitida de un
  paso a otro, errores robustos y diagnóstico de supuestos.
- Kmeans de los avisos sin variables de ubicación, para ver después si los
  tipos de departamento que encuentra se reparten distinto entre barrios.

**Instancia 3: visualización (script 11).** Dos gráficos:

- Uno exploratorio: el agrupamiento jerárquico de los 42 barrios según la
  composición de su oferta en los tres tipos del kmeans.
- Uno comunicativo: a igual departamento, en Puerto Madero se pide 3,0 veces
  lo que en Villa Lugano.

**Entrega final: herramienta de análisis económico (script 12).** Un índice
de concentración de los avisos publicados entre barrios: Herfindahl-Hirschman
con su número equivalente de barrios, curva de Lorenz y Gini. Mide la
concentración de la oferta publicada, no la del stock de viviendas, y
cuantifica por qué los modelos se restringen a los 42 barrios con al menos 100
avisos.

Todos los números de los títulos, los textos y las anotaciones salen de los
scripts: ninguno está escrito a mano.


## 4. Estructura de carpetas

```
TP_ciencia_de_datos/
├── raw/                    datos originales, tal como se descargaron
│   └── base_ventas_caba.csv
├── auxiliar/               archivos de apoyo
│   ├── diccionario_variables.csv
│   └── nombres_figuras.csv        (el nombre que lleva cada figura adentro)
├── input/                  base procesada, lista para el análisis
│   └── base_analisis.csv          (la arma el 02 y le agrega marcas el 06)
├── output/
│   ├── tablas/             tablas de resultados: CSV, tablas como imagen
│   │                       (t01 a t20), registros de diagnóstico (.txt),
│   │                       números clave (0X_numeros.csv) y textos
│   └── graficos/           gráficos: PNG de las instancias 1 y 2 (g1 a g18), la curva de Lorenz (g19),
│                           la prima de cada barrio contra su latitud (g20)
│                           y los dos JPG de la instancia 3, con su versión
│                           para diapositiva (*_diapositiva.jpg)
├── script/                 scripts de R, numerados por orden de ejecución
├── utils/                  funciones propias, una por archivo
├── TP_ciencia_de_datos.Rproj
└── README.md
```

Nada de `raw/` se modifica nunca. Todo lo que está en `input/` y `output/` lo
generan los scripts y se puede borrar y volver a generar.


## 5. Cómo reproducir el análisis

### Requisitos

R 4.5 o posterior. Los paquetes:

```r
install.packages(c(
  "tidyverse", "skimr", "FactoMineR", "cluster", "mclust",
  "lmtest", "sandwich", "car", "patchwork", "ggtext",
  "ragg", "systemfonts", "stringi",
  "officer", "flextable", "xml2", "zip"
))
```

`officer`, `flextable`, `xml2` y `zip` solo los usa el script 05, que arma un
borrador de la presentación de la instancia 1 y no calcula nada.

Los gráficos usan Segoe UI y Georgia, que vienen instaladas en Windows. Si
faltan, salen con la tipografía por defecto sin fallar, pero las imágenes no
quedan idénticas a las del repositorio.

### Orden de ejecución

Desde la raíz del proyecto (o abriendo `TP_ciencia_de_datos.Rproj`). Todas
las rutas son relativas: no hace falta editar nada. Cada script se puede
correr en una sesión nueva de R; no depende de objetos que haya dejado otro.

```bash
Rscript script/01_diagnostico.R
Rscript script/01b_diagnostico_profundo.R
Rscript script/02_preparacion.R
Rscript script/03_graficos.R
Rscript script/04_tablas.R
Rscript script/05_presentacion.R
Rscript script/06_faltantes_outliers.R
Rscript script/07_devolucion.R
Rscript script/08_regresion.R
Rscript script/09_kmeans.R
Rscript script/10_textos.R
Rscript script/11_instancia3.R
Rscript script/12_concentracion.R
Rscript script/13_numeros_presentacion.R
```

El orden importa en estos lugares:

- El **02** arma `input/base_analisis.csv` desde `raw/`, y el **06** le
  agrega las marcas de atípicos y la vuelve a guardar. Del 07 en adelante
  todos leen la base con esas marcas.
- El **09** usa la prima por barrio que guarda el **08**.
- El **10** lee los números que guardan el 03 y el 06 a 09.
- El **11** usa las tablas del **09** y vuelve a correr el **08** por dentro
  para tomar su muestra y su modelo.
- El **12** también vuelve a correr el **08** por dentro: toma de ahí los
  seis barrios con menos de 100 avisos, la muestra de 42 barrios y el modelo
  del paso 3.
- El **13** lee los números que guardan del 02 al 12, verifica que sean
  coherentes entre sí y con los de la sección 1 de este README, y los junta
  por diapositiva.

`script/00_tema.R` no se corre suelto: lo llaman los scripts que hacen
gráficos o tablas, y es el que carga las funciones de `utils/`.

La corrida completa tarda unos diez minutos, siete de ellos en el 09 (el
kmeans con 50 arranques, la estadística gap y la estabilidad por remuestreo).


## 6. Qué hace cada script

| Script | Qué hace | Salidas principales |
|---|---|---|
| `00_tema.R` | Configuración común: paleta, tipografías, tema de los gráficos y formato de números. Carga las funciones de `utils/`. No lee datos. | — |
| `01_diagnostico.R` | Diagnóstico de la base cruda: estructura, nulos, descriptivos, avisos repetidos, coherencia interna. No modifica datos. | `tablas/01_diagnostico.txt` |
| `01b_diagnostico_profundo.R` | Los cinco chequeos que quedaron abiertos en el 01. | `tablas/01b_diagnostico_profundo.txt` |
| `02_preparacion.R` | Separa republicaciones de unidades distintas, elimina las copias y agrega las variables derivadas. | `input/base_analisis.csv`, `tablas/02_*.csv` |
| `03_graficos.R` | Los ocho gráficos de la instancia 1. | `graficos/g1` a `g8`, `tablas/03_*.csv` |
| `04_tablas.R` | Las tablas descriptivas de la instancia 1, y la ficha de la base y las variables como imagen para la presentación final. | `tablas/04_*.csv`, `tablas/t19` y `t20` |
| `05_presentacion.R` | Borrador de la presentación de la instancia 1, armado desde R. No es la entrega: la presentación se editó después a mano. | `graficos/borrador_desde_R.pptx` |
| `06_faltantes_outliers.R` | Faltantes y atípicos: tres reglas en dos escalas, estrategias y su impacto. Agrega las marcas a la base de análisis. | `input/base_analisis.csv`, `graficos/g09a` a `g11`, `tablas/t01` a `t04` |
| `07_devolucion.R` | Las correcciones de la devolución de la instancia 1: descriptivas completas, avisos por barrio, correlaciones y PCA de cercanía. | `graficos/g12` a `g14`, `tablas/07_*`, `t05` a `t09` |
| `08_regresion.R` | Regresión en cuatro pasos y con latitud, sesgo por variable omitida, aporte de cada bloque, supuestos e influyentes. | `tablas/08_*`, `graficos/g15`, `tablas/t10` a `t15` |
| `09_kmeans.R` | Kmeans (k = 3) sin variables de ubicación, comparación con PAM, estabilidad y reparto de los grupos por barrio. | `tablas/09_*`, `graficos/g16` a `g18`, `tablas/t16` a `t18` |
| `10_textos.R` | Los textos de cada diapositiva de la instancia 2, con los números leídos de lo que guardaron los scripts anteriores. | `tablas/textos_instancia2.md` |
| `11_instancia3.R` | Los dos gráficos de la instancia 3: el jerárquico de barrios (exploratorio) y la brecha entre Puerto Madero y Villa Lugano (comunicativo). | `graficos/grupo_08_exploratorio.jpg`, `graficos/grupo_08_comunicativo.jpg`, `tablas/11_*` |
| `12_concentracion.R` | Herramienta de análisis económico: concentración de los avisos entre barrios (Herfindahl-Hirschman, número equivalente, Lorenz y Gini), la comparación entre 48 y 42 barrios, y el error estándar del coeficiente de cada barrio según sus avisos. | `graficos/g19_lorenz_barrios.png`, `tablas/12_*` |
| `13_numeros_presentacion.R` | Todos los números de la presentación final, verificados y con su fuente, agrupados por diapositiva. Calcula además la correlación entre la prima de cada barrio y su latitud, con su gráfico. | `tablas/13_numeros_diapositivas.csv`, `tablas/13_numeros.csv`, `graficos/g20_prima_latitud.png` |


## 7. Funciones propias (`utils/`)

Una función por archivo. Las carga `script/00_tema.R`.

| Archivo | Qué hace |
|---|---|
| `fuente_disponible.R` | Verifica si una tipografía está instalada. |
| `tema_tp.R` | El tema de ggplot de todos los gráficos. |
| `coma_ar.R` | Formatea un número con coma decimal y punto de miles. |
| `envolver.R` | Parte un texto largo en líneas. |
| `titulo.R` | Envuelve el título de un gráfico (58 caracteres por línea). |
| `bajada.R` | Envuelve la bajada de un gráfico (95 caracteres por línea). |
| `pie.R` | Envuelve la nota al pie de un gráfico (125 caracteres por línea). |
| `ajustar_pie.R` | Vuelve a envolver el pie si no entra en el ancho de la imagen. |
| `guardar.R` | Guarda un gráfico en `output/graficos/`. |
| `tabla_png.R` | Dibuja una tabla con ggplot, con la paleta del trabajo. |
| `guardar_tabla.R` | Guarda una tabla de `tabla_png()` en `output/tablas/`. |
| `guardar_numeros.R` | Guarda los números clave de un script en `output/tablas/`. |
| `leer_numeros.R` | Lee esos números. |
| `ari.R` | Índice de Rand ajustado entre dos particiones. |
| `pintar.R` | Pinta una palabra de un color dentro de un título con formato. |
| `segmentos_dendro.R` | Las coordenadas de un dendrograma, para dibujarlo con ggplot. |
| `hhi.R` | Índice de Herfindahl-Hirschman de un vector de cantidades. |
| `hhi_normalizado.R` | El HHI en una escala de 0 a 1 que no depende de cuántas unidades hay. |
| `curva_lorenz.R` | Los puntos de la curva de Lorenz. |
| `gini.R` | Coeficiente de Gini, a partir de la curva de Lorenz. |
| `nombre_figura.R` | El nombre descriptivo de una figura, desde `auxiliar/nombres_figuras.csv`. Se frena si no tiene. |
| `poner_nombre.R` | Pone ese nombre arriba de un gráfico o una tabla. Lo usan `guardar()` y `guardar_tabla()` cuando `MOSTRAR_NOMBRES` es `TRUE`. |
