# Textos de la presentación, instancia 2

Generado por `script/10_textos.R`. Todos los números salen de los scripts 06 a 09: si algo cambia, se vuelve a correr y este archivo se actualiza. 15 diapositivas de cuerpo sin contar carátula y cierre.

## 1. Retomar la primera entrega

### Diapositiva 1 (cuerpo)

**Título:** El precio por m² ordena la ciudad de sur a norte, no del centro a la periferia

**Bajada:** Síntesis de la instancia 1: la base, la pregunta y lo que encontramos.

**Imagen:** g3_norte_sur_distancia.png (de la instancia 1)

**Texto de la diapositiva** (76 palabras):

Base: 39.305 avisos de departamentos en venta en CABA (MercadoLibre, noviembre de 2024); la unidad es el aviso. Pregunta: ¿qué pesa más en el precio, la ubicación o los m²?

Hallazgo: entre el barrio más caro y el más barato el precio por m² se multiplica por 3,8. A igual distancia del Obelisco, el norte es entre 35% y 45% más caro, y el tramo más cercano al centro es el más barato de cada mitad.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**La base.** 40.168 avisos de departamentos en venta en la Ciudad de Buenos Aires, publicados en MercadoLibre y relevados por la cátedra en noviembre de 2024. La unidad de observación es el aviso publicado, no la vivienda. Sacamos las republicaciones del mismo inmueble y quedaron 39.305.

**La pregunta.** ¿Qué pesa más en el precio de un departamento: la ubicación o la cantidad de metros cuadrados?

**Lo que encontramos.** El precio total sigue de cerca a la superficie. El precio por m², en cambio, varía con la ubicación: entre el barrio más caro y el más barato el precio por m² se multiplica por 3,8. Y no es una cuestión de distancia al centro: a igual distancia del centro, el norte es entre un 35% y un 45% más caro. Dentro de cada mitad, estar cerca del Obelisco no es estar más caro: en las dos el máximo está entre 3 y 6 km, y el tramo de hasta 3 km es el más barato de cada mitad (US$ 2.875 por m² en el norte, US$ 2.089 en el sur).

</details>

### Diapositiva 2 (cuerpo)

**Título:** Incorporamos los cuatro pedidos de la devolución y aclaramos para qué usamos la latitud

**Texto de la diapositiva** (61 palabras):

Descriptivas: de 7 a 19 variables (anexo).

Avisos por barrio: tabla con porcentajes (anexo).

Atípicos: antes dijimos que no se podía decir nada. Ahora los detectamos y decidimos (sección 2).

Cercanía: mostramos con un PCA por qué no entran (diapositiva 3).

Latitud: es la medida del eje norte-sur, el hallazgo central. Entra en la regresión.

geocod_score: de acuerdo, no la analizamos.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Descriptivas de todas las variables relevantes.** A la presentación anterior llegaron 7; ahora están las 19 (anexo).

**Avisos por barrio.** Tabla de frecuencias de los 48 barrios con su porcentaje (anexo). Los cinco con más avisos reúnen el 42,3%.

**Valores atípicos.** En la instancia 1 escribimos que no se podía decir nada porque la base venía recortada. Estaba mal: la base viene recortada y aun así se puede analizar lo que quedó. Lo hacemos en la sección 2.

**Variables de cercanía.** Las habíamos descartado sin mostrar por qué. Ahora lo mostramos con una matriz de correlaciones y un PCA (diapositiva siguiente).

**La latitud.** La devolución la marcó entre las variables que no hacía falta analizar. No lo explicamos bien: es la variable que mide el eje norte-sur, el hallazgo central del trabajo. La usamos en la regresión.

**geocod_score.** De acuerdo con la devolución: no la analizamos. Solo la mencionamos como evidencia de que la base llega recortada (nunca baja de 80).

</details>

### Diapositiva 3 (cuerpo)

**Título:** Las once variables de cercanía miden centralidad, y la centralidad no ordena el precio

**Bajada:** Círculo de correlaciones de un PCA sobre las once variables. El precio por m² y la latitud se proyectan encima sin participar del cálculo.

**Imagen:** g13_pca_cercania.png

**Texto de la diapositiva** (57 palabras):

Las once variables arman un eje de centralidad (36,1% de la varianza; la distancia al Obelisco correlaciona 0,88 con él).

Los dos componentes capturan menos del 5% de la variación del precio por m² y menos del 2% de la de la latitud. Por eso de este bloque solo llevamos a la regresión la distancia al Obelisco.

<details><summary>Respaldo: versión completa, para la defensa</summary>

El primer componente (36,1% de la varianza) es centralidad: la distancia al Obelisco correlaciona 0,88 con él, y las paradas de colectivo, las líneas y las escuelas cargan en el mismo eje. El segundo (16,7%) agrupa las tres de espacios verdes.

El precio por m² cae en (0,12; 0,18) y la latitud en (0,12; -0,02), pegados al origen: los dos componentes capturan menos del 5% de la variación del precio por m² (4,9%) y menos del 2% de la de la latitud (1,5%). Es la suma de sus cargas al cuadrado en los dos componentes.

Por eso de este bloque llevamos a la regresión solo la distancia al Obelisco, como representante, y no un índice construido con el PCA: su coeficiente se lee en kilómetros.

</details>

## 2. Faltantes y outliers

### Diapositiva 4 (cuerpo)

**Título:** Los faltantes no son azar: dos son estructurales y uno está disfrazado

**Bajada:** Variables con faltantes, por qué faltan y qué hicimos con cada una.

**Imagen:** t01_faltantes.png

**Texto de la diapositiva** (45 palabras):

`inmobiliaria` falta exactamente en los avisos de particulares y `antiguedad_max` en la categoría «Más de 50 años»: son estructurales. Los mantenemos y no perdemos ningún aviso.

`publicacion_reciente` dice «sin dato» en el 89,4% de los avisos: es un faltante disfrazado. La descartamos; nunca la usamos.

<details><summary>Respaldo: versión completa, para la defensa</summary>

Solo dos variables tienen faltantes. `inmobiliaria` falta en 28.745 avisos (73,1%): exactamente los publicados por particulares. `antiguedad_max` falta en 4.588 (11,7%): exactamente la categoría abierta «Más de 50 años». Lo verificamos fila por fila.

**Estrategia: mantenerlos.** El faltante es informativo y su indicadora ya está en la base (`publicado_por` y `antiguedad_grupo`). No perdemos ningún aviso. Si elimináramos las filas incompletas quedarían 10.560 de 39.305. La consecuencia: `inmobiliaria` no se puede usar como variable de nada.

**Dónde se concentran.** Entre los 42 barrios con al menos 100 avisos, los particulares van del 92% de los avisos en Mataderos al 62% en Belgrano. Hay un gradiente con el precio del barrio (correlación -0,36), pero no es limpio (anexo).

Descartamos `publicacion_reciente`, que nunca usamos: el 89,4% de los avisos dice «sin dato», un faltante con otro nombre.

</details>

### Diapositiva 5 (cuerpo)

**Título:** Con el rango intercuartílico, los atípicos del precio pasan de 3.287 en escala original a 723 en logaritmo

**Bajada:** Boxplot del precio publicado en las dos escalas, y conteos de tres reglas en dos escalas.

**Imagen:** g09a_boxplot_precio_original.png y g09b_boxplot_precio_log.png, debajo t02_tres_reglas.png

**Texto de la diapositiva** (77 palabras):

Sobre el precio, las seis combinaciones de regla y escala marcan entre 291 y 3.884 avisos, y no se contradicen: la más laxa cubre 3.884 de los 3.889 que marca alguna. Cortan la misma cola en lugares distintos.

La distancia al Obelisco va al revés: 0 atípicos en escala original y 654 en logaritmo, todos a menos de 735 m del centro. El logaritmo ordena una cola larga a la derecha; donde no la hay, la inventa.

<details><summary>Respaldo: versión completa, para la defensa</summary>

Aplicamos las tres reglas de la clase 13 (rango intercuartílico, z clásico y z robusto) en escala original y en logaritmo. Sobre el precio marcan entre 842 y 3.884 avisos en escala original, y entre 291 y 723 en logaritmo.

**No coinciden en cuántos, pero no se contradicen.** Las seis combinaciones de regla y escala marcan en total 3.889 avisos, y la más laxa sola cubre 3.884. Las más estrictas marcan un subconjunto de las más laxas: todas cortan la misma cola, en lugares distintos. Por eso las reglas dan candidatos, no un veredicto: dónde cortar es una decisión.

**El logaritmo no es mejor por sí mismo.** La distancia al Obelisco va al revés que el precio: con el rango intercuartílico marca 0 atípicos en escala original y 654 en logaritmo, todos a menos de 735 m del Obelisco. La distancia es casi simétrica (asimetría 0,24) y el logaritmo le estira la cola izquierda (-1,01). La escala tiene que ajustarse a la forma de la variable: el logaritmo ordena una cola larga a la derecha, y donde no la hay, inventa atípicos. Pasa algo parecido, en chico, con el precio: en logaritmo aparecen 5 atípicos por abajo (menos de US$ 26.093) que en escala original no pueden existir.

El límite inferior solo informa cuando es alcanzable. En el precio es negativo, así que la falta de atípicos bajos no dice nada. En el precio por m² es US$ 138 y ningún aviso queda por debajo: el mínimo es US$ 1.011.

</details>

### Diapositiva 6 (cuerpo)

**Título:** Los extremos son casi todos genuinos: Puerto Madero existe

**Bajada:** Avisos que no son departamentos, y qué le pasa a la base con cada estrategia.

**Imagen:** t04_no_departamentos.png y t03_impacto_estrategias.png

**Texto de la diapositiva** (80 palabras):

47 de los 100 avisos de mayor precio por m² son de Puerto Madero: extremos genuinos. Sí sacamos 40 avisos que no son departamentos.

Al resto lo mantenemos y trabajamos en logaritmo. Eliminar lo que marca el rango intercuartílico borraría 3.287 avisos y bajaría el P95 de US$ 537.900 a US$ 329.000. Winsorizar en el p99 reemplaza 379 precios genuinos por un techo, y en logaritmo casi no cambia nada (Puerto Madero pasa de +129,7% a +124,0%): no hace falta.

<details><summary>Respaldo: versión completa, para la defensa</summary>

Revisamos los extremos uno por uno. 47 de los 100 avisos de mayor precio por m² son de Puerto Madero: un submercado real, no un error.

Lo que sí encontramos son 40 avisos que no son departamentos (24 oficinas, 8 locales, 4 terrenos, 3 cocheras, 1 depósito). Los identificamos porque el título empieza nombrando otra unidad. Esos salen de los modelos.

**Estrategia para el resto: mantener, trabajar en logaritmo y medir la sensibilidad.** Sacar todo lo que marca la regla de 1,5 RIC eliminaría 3.287 avisos, bajaría la media de US$ 198.218 a US$ 153.257 y el P95 de US$ 537.900 a US$ 329.000. No es limpiar: es borrar la parte alta del mercado.

**Por qué tampoco winsorizamos**, aunque la tabla la muestre como la opción que menos mueve: conserva todos los avisos, pero reemplaza los 379 precios por encima del p99 (US$ 990.000) por ese techo, y 66 de ellos son de Puerto Madero (14% del barrio). Es cambiar valores genuinos por uno inventado. Y no hace falta: en logaritmo la cola ya pesa poco. Lo probamos en el modelo completo: la prima de Puerto Madero pasa de +129,7% a +124,0% y la elasticidad de 0,954 a 0,936.

La sensibilidad la medimos en cada método: con la distancia de Cook en la regresión y con PAM, que es robusto a extremos, en el kmeans.

</details>

## 3. Regresión lineal

### Diapositiva 7 (cuerpo)

**Título:** Usamos una regresión porque queremos mirar el tamaño y el barrio a la vez

**Texto de la diapositiva** (84 palabras):

Regresión porque el precio es continuo y lo acompañan variables continuas y categóricas: mira el tamaño y el barrio a la vez. Log(precio) en cuatro pasos: superficie; ambientes, baños y antigüedad; barrio (base Balvanera); distancia al Obelisco y quién publica. 38.895 avisos, errores HC1.

H0: a igual superficie y características, todos los barrios valen lo mismo que Balvanera. H1: al menos uno se aparta.

F = 651,18 (41 y 38.847 gl), p < 2,2 × 10⁻¹⁶: rechazamos. El barrio suma 8,45 puntos de R².

<details><summary>Respaldo: versión completa, para la defensa</summary>

**La pregunta.** ¿Cuánto se asocia el precio con la superficie y cuánto con el barrio, mirando cada uno con el otro fijo?

**Por qué este método.** El precio es continuo y las variables que lo acompañan mezclan continuas (superficie, distancia) y categóricas (barrio, antigüedad). Un ANOVA compararía barrios sin descontar el tamaño; la regresión los mira juntos. Precio y superficie van en logaritmo, por la asimetría de la sección 2 y porque así los coeficientes se leen como variaciones porcentuales.

**El modelo, en cuatro pasos.** Superficie; más ambientes, baños y antigüedad; más el barrio (41 categorías, con Balvanera como base); más la distancia al Obelisco y quién publica. 38.895 avisos de los 42 barrios con al menos 100. Errores estándar robustos (HC1).

**Hipótesis nula.** Una vez que se tienen en cuenta la superficie y las características de la vivienda, el barrio no se asocia con el precio: todos los barrios valen lo mismo que Balvanera. **Alternativa:** al menos un barrio se aparta de Balvanera.

**Resultado.** F = 651,18 con 41 y 38.847 grados de libertad, p < 2,2 × 10⁻¹⁶. Rechazamos la nula. Tamaño del efecto: el barrio suma 8,45 puntos de R² (de 0,7919 a 0,8764).

</details>

### Diapositiva 8 (cuerpo)

**Título:** A igual barrio y características, un departamento 10% más grande se publica 9,5% más caro

**Bajada:** Cuatro pasos y una variante con la latitud en lugar del barrio. Variable dependiente: log(precio).

**Imagen:** t10_regresion_pasos.png

**Texto de la diapositiva** (67 palabras):

La elasticidad es 0,954: casi proporcional y algo menor que 1, así que el precio por m² baja levemente con el tamaño.

Con la latitud en lugar de los 41 barrios, el R² baja de 0,879 a 0,842, y cada kilómetro al norte se asocia con un precio 5,1% mayor. El R² ajustado (0,8786) queda apenas debajo del R² (0,8787): las 41 categorías no inflan el ajuste.

<details><summary>Respaldo: versión completa, para la defensa</summary>

La elasticidad del precio a la superficie es 0,954: casi proporcional y algo menor que 1, así que el precio por m² baja levemente con el tamaño.

Con las 41 categorías de barrio el R² es 0,879. Reemplazándolas por una sola variable, la latitud, es 0,842. Con la latitud, **un kilómetro más al norte se asocia con un precio 5,1% mayor**, a igual distancia al centro.

El R² ajustado, que descuenta la cantidad de variables, es 0,8786, apenas por debajo del R² de 0,8787: las 41 categorías no inflan el ajuste.

Las primas de cada barrio, calculadas como (exp(β) − 1) × 100 y no como 100 × β, están en el anexo.

</details>

### Diapositiva 9 (cuerpo)

**Título:** El coeficiente de la superficie se mueve al sumar controles: es sesgo por variable omitida

**Bajada:** Qué mueve el coeficiente de log(superficie) en cada paso, y la prima de cada barrio contra la superficie promedio de sus avisos.

**Imagen:** t11_sesgo_omitida.png y g15_prima_superficie_barrio.png

**Texto de la diapositiva** (74 palabras):

Al sumar las características, el coeficiente de la superficie sube de 1,038 a 1,124: los ambientes tienen coeficiente negativo y se mueven con la superficie, así que al omitirlos ese efecto se le cargaba al tamaño.

Al sumar el barrio, baja a 0,959: a igual cantidad de ambientes, los departamentos más amplios están en los barrios caros, sobre todo en el tercio más caro.

El coeficiente negativo de ambientes desaparece cuando entra el barrio.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Al sumar las características sube, de 1,038 a 1,124.** Los ambientes lo empujan para arriba (+0,133): su coeficiente es negativo y se mueven con la superficie (correlación 0,88), así que al omitirlos ese efecto se le cargaba al tamaño. Los baños empujan para el otro lado (-0,108) y compensan una parte.

**Al sumar el barrio baja, a 0,959.** Todo el cambio (-0,165) viene de que, a igual cantidad de ambientes, los departamentos más amplios están en los barrios caros. El gráfico muestra la superficie tal cual (correlación 0,79); descontando ambientes, baños y antigüedad, sube a 0,83.

**La relación está en los barrios caros.** Por terciles de prima, la correlación es 0,75 en el tercio caro y -0,12 en el tercio barato. Tampoco es solo Puerto Madero: sin él, 0,66.

Una consecuencia: «a igual superficie, un ambiente más es más barato» vale sin el barrio (-0,067) y desaparece cuando el barrio entra (-0,002, p = 0,48).

</details>

### Diapositiva 10 (cuerpo)

**Título:** La superficie acompaña al precio total y el barrio al precio por m²: no compiten

**Bajada:** Cuánto aporta cada bloque al R² cuando se lo quita del modelo completo, en las dos lecturas del precio.

**Imagen:** t12_identidad_aporte.png

**Texto de la diapositiva** (68 palabras):

Las dos regresiones son la misma ecuación: el barrio reduce la suma de cuadrados en lo mismo y solo cambia el denominador.

Sobre el precio total, la superficie aporta 10,2 puntos de R² y el barrio 8,1 puntos. Sobre el precio por m², el barrio aporta 26,7 puntos y la superficie 0,08 puntos, aunque sea significativa (F = 52,8 con 1 y 38.845 gl): significativo no es importante.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Las dos regresiones son la misma ecuación.** Con log(superficie) a la derecha, pasar de log(precio) a log(precio/m²) solo le resta 1 a su coeficiente: los residuos y los demás coeficientes son idénticos. Por eso el barrio reduce la suma de cuadrados en lo mismo en las dos (1.367) y lo único que cambia es el denominador.

Sobre el precio total, la superficie aporta 10,2 puntos de R² y el barrio 8,1 puntos. Sobre el precio por m², el barrio pasa a 26,7 puntos y la superficie a 0,08 puntos.

**Significativo no es lo mismo que importante.** La superficie sobre el precio por m² es significativa (F = 52,8 con 1 y 38.845 gl, p < 0,001) y su aporte es prácticamente nulo. Por eso respondemos la pregunta con el tamaño del efecto y no con el p-valor.

</details>

### Diapositiva 11 (cuerpo)

**Título:** Los supuestos no se cumplen del todo, y lo que hicimos no cambia las conclusiones

**Bajada:** Verificación de supuestos sobre el modelo completo.

**Imagen:** t13_supuestos.png

**Texto de la diapositiva** (67 palabras):

RESET rechaza, también con log(m²)², pero las conclusiones no cambian: la elasticidad pasa a ir de 0,90 a 1,03. Breusch-Pagan rechaza: errores HC1. Sin los 2.073 avisos influyentes, la elasticidad pasa de 0,954 a 0,938 y Puerto Madero de +129,7% a +138,1%: la brecha entre barrios crece, no depende de los extremos.

Límites: son precios publicados, no de venta; la base llega recortada; son asociaciones, no causas.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Forma funcional.** El RESET rechaza. Con 38.895 avisos rechaza casi siempre, así que probamos agregar log(m²)²: la elasticidad pasa a ir de 0,90 en 35 m² a 1,03 en 150 m², el F del barrio de 651 a 636 y el aporte de la superficie al precio por m² sigue cerca de cero (0,26 puntos de R²). El RESET sigue rechazando (206): lo que falta probablemente sean variables que la base no tiene, como el estado o el piso.

**Varianza.** Breusch-Pagan rechaza: usamos errores robustos HC1 en todo.

**Influyentes.** 2.073 avisos superan la distancia de Cook de 4/n. Sin ellos la elasticidad pasa de 0,954 a 0,938, y las primas de barrio también se mueven: la de Puerto Madero pasa de +129,7% a +138,1%, y la brecha entre los barrios extremos de 2,60 a 2,71 veces. Crece, no se achica: el resultado no depende de los extremos. No los sacamos.

**Multicolinealidad.** Solo la distancia al Obelisco supera el umbral (4,95), porque se superpone con el barrio: el barrio por sí solo explica el 95,9% de la distancia. El barrio, en cambio, queda en 1,04: una variable sola se deja explicar por 41 categorías, pero 41 categorías no se dejan explicar por una variable. La distancia es un control y no la interpretamos.

**Limitaciones.** Son precios publicados, no de venta. La base llega recortada. Dejamos afuera 370 avisos de 6 barrios con menos de 100. Y son asociaciones entre avisos de un único momento, no causas.

</details>

## 4. Kmeans

### Diapositiva 12 (cuerpo)

**Título:** Agrupamos los avisos por lo que son, sin decirle al algoritmo dónde están

**Bajada:** Los tres criterios para elegir la cantidad de grupos.

**Imagen:** g16_criterios_k.png

**Texto de la diapositiva** (56 palabras):

Agrupamos por precio por m², superficie y antigüedad, estandarizadas y sin la ubicación, para que el cruce con barrio sea una prueba.

La silueta prefiere 2, el gap no distingue entre 2 y 3 y el codo no tiene quiebre. Elegimos 3: los grupos se distinguen por antigüedad y tamaño; con 2, casi solo por antigüedad.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Por qué kmeans.** Queremos ver si en la base hay tipos de departamento y si esos tipos coinciden con la geografía.

**Variables.** Log del precio por m², log de la superficie y tramo de antigüedad (0 a 3), estandarizadas: kmeans usa distancia euclídea y sin estandarizar manda la de mayor escala. La ubicación no entra, así el cruce con barrio y comuna es una prueba y no algo que pusimos nosotros. Ambientes y baños quedan afuera porque se mueven con la superficie y la contarían tres veces. 50 arranques.

**Cantidad de grupos.** Los criterios no coinciden. La silueta prefiere 2 (0,36 contra 0,34 con 3). El gap no distingue entre 2 y 3: la diferencia es del tamaño de su error estándar, y con dos muestras al azar distintas eligió una vez 2 y otra 3. El codo no tiene un quiebre marcado.

**Elegimos 3 por interpretabilidad:** los grupos se distinguen por antigüedad y por tamaño a la vez. Con 2 la partición es prácticamente una sola variable, la antigüedad. Es una decisión analítica, no un descubrimiento.

</details>

### Diapositiva 13 (cuerpo)

**Título:** Tres tipos de departamento: viejos y baratos, nuevos y chicos, grandes y caros

**Bajada:** Perfil de cada grupo en unidades originales. La ubicación no entró al clustering.

**Imagen:** t16_perfiles_grupos.png y g17_grupos_pca.png

**Texto de la diapositiva** (65 palabras):

Viejos y baratos (35,4%, US$ 2.000 por m²), nuevos y chicos (46,2%, 42 m²) y grandes y caros (18,5%, 106 m², US$ 3.607 por m²).

El grupo viejo es el más cercano al Obelisco: a menos de 3 km, el 26,6% de los avisos tiene más de 50 años (10,3% o menos en los demás anillos). Es el anillo más barato de la diapositiva 1.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Grupo 1 (35,4% de los avisos).** Los viejos: casi ninguno a estrenar (0,5%) y el 30,7% con más de 50 años. Precio por m² mediano de US$ 2.000, el más bajo.

**Grupo 2 (46,2%).** A estrenar (66,7%) y chicos: 42 m² de mediana.

**Grupo 3 (18,5%).** Los grandes: 106 m² de mediana y el precio por m² más alto, US$ 3.607.

Lo que no entró al clustering: el grupo 3 tiene el 69% de sus avisos en la mitad norte, contra el 35% del grupo 1. Y algo que no esperábamos: **el grupo más barato es el más cercano al Obelisco** (4,6 km de mediana, contra 6,5 y 6,0). El stock viejo está en el centro: en el anillo de hasta 3 km del Obelisco el 26,6% de los avisos tiene más de 50 años, contra 10,3% o menos en los demás. Es el mismo anillo que en la diapositiva 1 aparece como el más barato de cada mitad.

</details>

### Diapositiva 14 (cuerpo)

**Título:** Los grupos son estables, pero no reproducen la geografía

**Bajada:** Coincidencia entre particiones, medida con el índice de Rand ajustado.

**Imagen:** t17_estabilidad_grupos.png

**Texto de la diapositiva** (45 palabras):

PAM coincide con kmeans en el 90,6% de los avisos (ARI 0,74).

Contra barrio, comuna y mitad norte-sur, el ARI es 0,007, 0,013 y 0,036: las 15 comunas tienen los tres tipos.

Límites: silueta de 0,34, estructura débil. Sin estandarizar, la partición cambia (ARI 0,44).

<details><summary>Respaldo: versión completa, para la defensa</summary>

**Estabilidad.** PAM, que usa avisos reales como centro y es más robusto a extremos, coincide con kmeans en el 90,6% de los avisos (ARI 0,74). Sin los 240 avisos más extremos, ARI 0,88.

**Geografía.** Contra el barrio el ARI es 0,007, contra la comuna 0,013 y contra la mitad norte-sur 0,036. Las 15 comunas tienen avisos de los tres grupos. Los tipos de departamento están repartidos por toda la ciudad. (El ARI contra el barrio no podría llegar a 1 aunque la correspondencia fuera perfecta: 3 grupos no reproducen 42 barrios.)

**Limitaciones.** La silueta media es 0,34: estructura débil. Los grupos son una descripción útil de la base, no una partición natural. Sin estandarizar la partición cambia (ARI 0,44): manda la variable de mayor escala. Y con 2 o 4 grupos se reordena bastante (ARI 0,62 y 0,65).

</details>

## 5. Síntesis y próximos pasos

### Diapositiva 15 (cuerpo)

**Título:** El barrio pesa en el precio por m² aun comparando el mismo tipo de departamento

**Bajada:** Cómo se reparte cada barrio entre los tres grupos, con los barrios ordenados por la prima que les estima la regresión.

**Imagen:** g18_barrios_grupos.png

**Texto de la diapositiva** (87 palabras):

La superficie acompaña al precio total (10,2 puntos de R²) y el barrio al precio por m² (26,7 puntos): no compiten.

Brecha entre barrios extremos: 3,8 veces en crudo, 3,0 a igual vivienda, 2,6 con distancia y quién publica, y, como mínimo, 2,0 a 2,7 dentro de cada tipo. La mayor parte se sostiene.

Recoleta (prima de 48%) tiene casi la mitad de sus avisos en el grupo viejo: un barrio caro no es un barrio de departamentos caros. Queda abierto qué hay detrás de esa prima.

<details><summary>Respaldo: versión completa, para la defensa</summary>

**La hipótesis.** La superficie acompaña al precio total (10,2 puntos de R²) y el barrio al precio por m² (26,7 puntos). No compiten: responden preguntas distintas. Los resultados apoyan la hipótesis y la reformulan.

**Cuánto de la diferencia entre barrios sobrevive al comparar comparables.** Entre el barrio más caro y el más barato hay 3,8 veces de precio por m² en crudo. A igual superficie, ambientes, baños y antigüedad, la regresión deja 3,0 (Puerto Madero contra Villa Lugano). Sumando la distancia al Obelisco y quién publica, 2,6 (Puerto Madero contra Boca). Y dentro de cada tipo de departamento del kmeans, entre 2,0 y 2,7: un piso, no una estimación, porque los grupos se arman con el precio por m² y adentro de cada uno ese precio ya viene recortado.

Mires como mires, la mayor parte de la diferencia entre barrios se sostiene. Una parte es qué tipo de departamento se publica en cada uno; el resto, la mayor, se mantiene a igual tipo. Y los tres tipos están en los 42 barrios: no son tipos de barrio.

**El ejemplo.** Recoleta tiene una prima de 48% sobre Balvanera y el 48% de sus avisos en el grupo viejo y barato (Retiro, el 49%). Un departamento de ese grupo se publica a US$ 2.442 por m² en Recoleta y a US$ 1.635 en Balvanera. Un barrio caro no es un barrio de departamentos caros: conviven los dos tipos.

**La misma idea en la instancia 1.** La brecha norte-sur es mayor en el stock viejo: el norte es 1,33 veces más caro en los a estrenar y 1,32 en los de hasta 20 años, y 1,40 en los de 20 a 50 y 1,47 en los de más de 50. Comparando departamentos de la misma antigüedad, la ubicación se asocia con diferencias más grandes en los viejos.

Dos correlaciones que no tomamos como evidencia, porque cruzan cosas construidas desde el precio: el grupo caro pesa más en los barrios de prima alta (Spearman 0,85), y dentro de cada tipo el barrio ordena el precio por m² como la prima (entre 0,81 y 0,94).

**Para la entrega final.** ¿Qué hay detrás de la parte de la prima que se sostiene a igual tipo de departamento? Vamos a sumar lo que dicen los avisos en el texto (cochera, amenities, estado) y a ver si el eje norte-sur se sostiene dentro de cada tipo.

</details>

## Anexo

**A1.** Avisos por barrio, con su porcentaje (pedido de la devolución). Imagen: g14_avisos_por_barrio.png y t07_avisos_por_barrio.png

**A2.** Descriptivas de las variables de la vivienda (pedido de la devolución). Imagen: t05_descriptivas_vivienda.png

**A3.** Descriptivas de las variables de entorno. Imagen: t06_descriptivas_entorno.png

**A4.** Frecuencias de las cuatro variables categóricas. Imagen: t09_frecuencias_categoricas.png

**A5.** Correlaciones y cargas del PCA de cercanía. Imagen: g12_correlaciones_cercania.png y t08_pca_cargas.png

**A6.** Dónde se concentran los faltantes de inmobiliaria. Imagen: g11_particulares_por_barrio.png

**A7.** Las seis combinaciones de regla y escala, un punto por aviso. Imagen: g10_tres_reglas.png

**A8.** Primas exactas de los 41 barrios sobre Balvanera. Imagen: t14_primas_barrio.png

**A9.** Coeficientes con y sin los avisos influyentes. Imagen: t15_cook.png

**A10.** Cómo se reparte cada comuna entre los tres grupos. Imagen: t18_comuna_grupos.png

**A11.** El detalle de la primera entrega que sigue siendo útil. Imagen: g1, g2, g4 a g8 (instancia 1)

