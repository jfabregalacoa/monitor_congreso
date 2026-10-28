# Urgencias del Ejecutivo en las votaciones de la Cámara, 2002-2026

Base para el proyecto de Amanda (Economía Política Formal 2026, DCCS-UDD), construida el 28-10-2026 en la rama `feature/urgencias-votaciones`. Scripts y orden de ejecución: [`src/urgencias/`](../../src/urgencias/README.md). Diccionario de datos: [`diccionario.csv`](diccionario.csv).

## 1. Resumen

| Qué | Resultado |
|:--|:--|
| Votaciones con urgencia reconstruida | **17.935** de 3.320 proyectos, en 6 períodos (2002-06 a 2022-26 extendida). Ninguna queda sin fecha, boletín o proyecto |
| Base larga diputado × votación | **2.476.655 filas**, sin duplicados, en dos archivos que se pueden apilar: [`panel_votos_2002_2014.csv.gz`](panel_votos_2002_2014.csv.gz) (859.806 filas) y [`panel_votos_2014_2026.csv.gz`](panel_votos_2014_2026.csv.gz) (1.616.849) |
| Votos de las matrices contra la Cámara | 0 discrepancias en 446.488 celdas (4.300 votaciones revisadas) |
| Proyectos con urgencia distinta entre sus votaciones (muestra para P5) | **635** (443 mensajes) |
| No podían repostular por la Ley 21.238 | 37 diputados en 2018-22 y 9 en 2022-26 |
| Independientes (oficialismo NA) | 5-7 % de las celdas en funciones hasta 2018, 10 % en 2018-22 y 25 % en 2022-26 |

Otros archivos: [`votaciones_urgencia_2002_2026.csv`](votaciones_urgencia_2002_2026.csv) (una fila por votación), [`urgency_spells.csv`](urgency_spells.csv) (historial de urgencias del Senado), [`militancias_2002_2026.csv`](militancias_2002_2026.csv), [`horizonte_ley21238.csv`](horizonte_ley21238.csv), [`ideologia_diputado_periodo.csv`](ideologia_diputado_periodo.csv) (intervalos de la ideología), [`coalicion_gobierno.csv`](coalicion_gobierno.csv) y [`suspensiones_desafuero.csv`](suspensiones_desafuero.csv), estas dos con fuente por fila.

## 2. Cómo se mide la urgencia

Para cada votación se buscan las urgencias del Ejecutivo vigentes **en la Cámara** ese día. Cada urgencia tiene fecha de ingreso, un plazo legal (30 días la simple, 15 la suma, 6 la discusión inmediata; Ley 18.918, art. 27) y a veces fecha de retiro. Como el Ejecutivo suele retirarlas tarde o nunca, hay dos versiones:

- **Estricta** (`urg_nivel`): rige desde el ingreso hasta el retiro o el fin del plazo legal, lo que ocurra primero. Es la lectura de la ley.
- **Laxa** (`urg_nivel_laxa`): rige hasta el retiro aunque el plazo haya vencido (sin retiro, hasta el fin del plazo). Sirve de prueba de robustez.

Con varias vigentes se toma la de mayor nivel (0 sin urgencia, 1 simple, 2 suma, 3 discusión inmediata). Las dos versiones coinciden en el 98,7 % de las votaciones; en el resto la laxa es mayor.

> **Ejemplo** (votación 19047, 15-09-2015). Una suma ingresó el 20-08 y se retiró el 19-09, pero su plazo venció el 04-09. La estricta la da por vencida y asigna 1, por una simple del 03-09; la laxa asigna 2.

**Cámara inferida.** Hasta 2004 el Senado casi nunca anota en qué cámara se puso la urgencia. En esos casos se usa la cámara donde estaba el proyecto ese día según su tramitación. La regla acierta el 99 % de las 24.515 urgencias que sí traen la cámara. Sin ella, 2002-06 tendría urgencia en el 21 % de las votaciones; con ella, en el 50 %. `urg_camara_inferida` marca las 574 votaciones afectadas (573 de 2002-06).

**Variación.** Un proyecto tiene variación si se votó dos o más veces y no todas sus votaciones tienen el mismo nivel; por ejemplo, votado en general sin urgencia y en particular con suma. Es la muestra para comparar votaciones dentro de un proyecto (P5). Hay **variación estricta** si difieren según `urg_nivel` y **laxa** si difieren según `urg_nivel_laxa`.

| Período | Proyectos con 2+ votaciones | Con variación estricta | Con variación laxa | Mensajes con variación estricta |
|:--|--:|--:|--:|--:|
| 2002-06 | 239 | 79 | 72 | 70 |
| 2006-10 | 253 | 89 | 87 | 75 |
| 2010-14 | 294 | 114 | 105 | 89 |
| 2014-18 | 281 | 102 | 96 | 81 |
| 2018-22 | 371 | 95 | 88 | 56 |
| 2022-26 | 333 | 80 | 79 | 38 |
| **2002-26** | 1.752 | **635** | 603 | 443 |

La última fila junta las votaciones de un proyecto aunque caigan en períodos distintos; 597 proyectos tienen variación en ambas versiones.

## 3. Votaciones por período

| Período | Votaciones | Proyectos | Con urgencia (estricta / laxa) | Mensajes / mociones con urgencia (estricta) | Ley de Presupuestos |
|:--|--:|--:|:--|:--|--:|
| 2002-06 | 1.950 | 479 | 50,5 % / 52,8 % | 58,8 % / 26,6 % | 119 |
| 2006-10 | 1.990 | 552 | 48,4 % / 49,4 % | 55,5 % / 25,6 % | 387 |
| 2010-14 | 3.082 | 558 | 56,7 % / 57,9 % | 61,7 % / 33,3 % | 675 |
| 2014-18 | 3.267 | 573 | 59,0 % / 59,3 % | 62,4 % / 45,8 % | 568 |
| 2018-22 | 3.520 | 703 | 51,2 % / 53,0 % | 62,4 % / 30,4 % | 687 |
| 2022-26 | 4.126 | 675 | 49,2 % / 49,2 % | 46,5 % / 56,3 % | 1.518 |

- **Ley de Presupuestos** (`ley_presupuestos`): 3.954 votaciones, una ley por año. No se rige por urgencias (Ley 18.918, art. 26), así que conviene excluirla. Los porcentajes de mensajes la incluyen como 0.
- **Comisión mixta:** tiene plazos más cortos (art. 28) que la base no modela; se puede separar con `tramite_constitucional`.
- **Fechas mal escritas en el Senado:** 35 años de dos dígitos se corrigieron y 41 retiros anteriores al ingreso se ignoraron. Quedan marcados en `f_ingreso_corregida` y `retiro_antes_ingreso`.

## 4. La base larga

Una fila por celda de cada matriz (diputado × votación).

**Voto.** `voto_original` es la celda tal cual. `voto` la homologa; `voto_si` vale 1 si es Afirmativo, 0 si es En contra y NA en el resto.

| voto | 2002-06 | 2006-10 | 2010-14 | 2014-18 | 2018-22 | 2022-26 |
|:--|--:|--:|--:|--:|--:|--:|
| Afirmativo | 131.303 | 136.278 | 223.549 | 253.968 | 334.405 | 362.645 |
| En contra | 31.492 | 35.956 | 61.323 | 51.810 | 110.504 | 138.554 |
| Abstención | 4.924 | 7.972 | 8.141 | 14.305 | 20.756 | 25.968 |
| Dispensado | 0 | 11 | 483 | 1.073 | 898 | 520 |
| Ausente o no vota | 58.450 | 58.580 | 76.284 | 67.598 | 78.970 | 107.137 |
| No en funciones | 1.968 | 5.973 | 9.306 | 3.384 | 28.227 | 8.390 |
| No en funciones (suspendido) | 7.813 | 0 | 0 | 3.169 | 0 | 4.568 |

- **Ausente o no vota:** en funciones y sin voto. La Cámara no lo registra igual siempre: hasta el 09-04-2025 el ausente no aparece (NA en la matriz) y desde entonces aparece como "No Vota". No distingue ausencia de presencia sin votar. No hay pareos en las fuentes.
- **No en funciones:** antes de asumir o después de dejar el cargo. Las salidas vienen de la Cámara. Un reemplazante entra en su primera votación (aproximación), y se considera reemplazante a quien empieza a votar más de 180 días después del inicio del período. En 2002-06 la Cámara no registra salidas; la de Ramón Pérez Opazo (falleció el 12-06-2005) se agregó a mano.
- **Suspendido:** desafuero firme (Constitución, art. 61). Son 11 casos con fuente: cinco del caso Coimas y Mario Escobar en 2002-06; Rosauro Martínez, María Luisa Cordero, Mauricio Ojeda, Francisco Pulgar y Catalina Pérez en 2014-26. Se marca la racha de ausencia que sigue a la resolución. En el caso de Escobar no se encontró la fecha de la confirmación de la Corte Suprema.
- **Partido** (`partido`): el que el diputado tenía el día de la votación, según las militancias con fecha de la Cámara. Desde 2014 se ven los cambios dentro del período (8, 37 y 55 diputados por período); en 2002-14 la Cámara registra un solo partido por período. Si hay dos partidos el mismo día se usa el más reciente y se marca `oficialista_dudoso` (3 diputados).
- **Ideología:** `ideol_dyn` (dinámica, emIRT), `ideol_wnom` (W-NOMINATE) e `ideol_bay` (bayesiana), del `long_format` publicado; ninguna falta en las celdas en funciones. Sus intervalos están en [`ideologia_diputado_periodo.csv`](ideologia_diputado_periodo.csv).

## 5. Oficialismo

`oficialista` vale 1 si el partido del diputado formaba parte de la coalición de gobierno o tenía ministros ese día, según [`coalicion_gobierno.csv`](coalicion_gobierno.csv) (BCN y prensa, con fecha y fuente por fila).

| Gobierno | Oficialistas | Casos dudosos (`oficialista_dudoso = 1`) |
|:--|:--|:--|
| Lagos (2002-06) | DC, PS, PPD, PR (Concertación) | — |
| Bachelet I (2006-10) | DC, PS, PPD, PR (Concertación) | PRI, no oficialista |
| Piñera I (2010-14) | RN, UDI (Coalición por el Cambio) | PRI, no oficialista (Zaldívar fue embajador) |
| Bachelet II (2014-18) | PS, PPD, DC, PR, PC, IC, MAS (Nueva Mayoría) | RD y PL, no oficialistas |
| Piñera II (2018-22) | RN, UDI, Evópoli, PRI hasta feb-2022 (Chile Vamos) | Republicanos, no oficialistas |
| Boric (2022-26) | Apruebo Dignidad (FA, RD, CS, Comunes, PC, FRVS hasta el 19-08-2025, AH) y Socialismo Democrático (PS, PPD, PR, PL) desde el 11-03-2022 | AH, oficialista; DC y PEV, no oficialistas; FRVS desde el 20-08-2025, NA |

Los independientes quedan NA en todos los gobiernos: su cercanía al gobierno depende de la bancada o del pacto por el que fueron electos, que no están en estos datos.

| Celdas en funciones | 2002-06 | 2006-10 | 2010-14 | 2014-18 | 2018-22 | 2022-26 |
|:--|--:|--:|--:|--:|--:|--:|
| Oficialista | 49,4 % | 46,7 % | 46,0 % | 53,4 % | 43,7 % | 33,2 % |
| No oficialista | 45,1 % | 45,8 % | 49,2 % | 39,7 % | 45,8 % | 42,1 % |
| NA (independiente) | 5,5 % | 7,5 % | 4,8 % | 6,9 % | 10,4 % | 24,6 % |

## 6. Horizonte por la Ley 21.238

La ley (D.O. 08-07-2020) limita la reelección a tres períodos consecutivos; un período cuenta si se ejerció más de la mitad, y cuentan también los anteriores a 2020.

- `no_puede_repostular = 1` si el diputado ejerció más de la mitad del período actual y de los dos anteriores. Hay 37 casos en 2018-22: los 33 de la [lista de prensa](https://www.radioagricultura.cl/noticias/politica/los-33-diputados-que-no-iran-por-un-nuevo-periodo-en-la-camara-por-aplicacion-de-ley-de-limite-de-reeleccion_20211117/) más Melero, Gutiérrez, Sabat y Kort, que dejaron la Cámara antes de la elección. En 2022-26 hay 9.
- `horizonte_corto = 1` desde la publicación de la ley, en las votaciones de esos diputados.
- `habria_estado_afectado` aplica la misma regla antes de la ley (44 en 2010-14 y 44 en 2014-18); en 2002-10 queda NA porque faltan los períodos anteriores a 2002.

## 7. Decisiones que conviene tener presentes

1. **Solo cuentan las urgencias puestas en la Cámara.** Una urgencia en el Senado no presiona a los diputados.
2. **La urgencia de antes de 2005 es en parte inferida.** Para comprobar que los resultados no dependen de ello, se puede repetir el análisis sin las votaciones con `urg_camara_inferida = 1`.
3. **Oficialismo según la pertenencia a la coalición, no según cómo votó el partido.** Votar con el gobierno es justamente lo que se quiere explicar. Los casos discutibles llevan `oficialista_dudoso = 1` y los independientes quedan NA; en 2022-26 son un cuarto de las celdas, así que ese período depende más de cómo se traten.
4. **Partido a la fecha y no fijo por período.** Un diputado que deja un partido oficialista cuenta como independiente desde ese día. En 2002-14 no se ven esos cambios.
5. **"Ausente o no vota" significa lo mismo en todo el período,** pese al cambio de registro de abril de 2025, y no incluye a quienes no podían votar (no asumidos, ya salidos o suspendidos).
6. **Tipo de votación.** `tipo_votacion_3` distingue votaciones en general (idea de legislar), en particular (artículo por artículo) y otras; sirve para comparar dentro de un proyecto.

## 8. Fuentes

- **Cámara** (`opendata.camara.cl`): detalle de 4.300 votaciones, votaciones de 3.320 proyectos y diputados con militancias de 8 períodos. No se usó `www.camara.cl`.
- **Senado** (`tramitacion.senado.cl/wspublico`): historial de urgencias y tramitación de 3.320 proyectos.
- **Repositorio:** matrices de Harvard Dataverse, ideología `long_format` y JSON `details__periodo_*`.
- **BCN LeyChile:** [Ley 18.918](https://www.bcn.cl/leychile/navegar?idNorma=30289) (arts. 26-28), [Constitución](https://www.bcn.cl/leychile/navegar?idNorma=242302) (arts. 51, 61 y 74) y [Ley 21.238](https://www.bcn.cl/leychile/navegar?idNorma=1147301).

Todo se descargó el 28-10-2026, sin errores. Al rehacer todo el flujo desde la caché se obtienen archivos idénticos byte a byte.

## 9. Propuesta para la "estaca" de cada votación (sin implementar)

1. **Tipo de votación y trámite:** ya disponibles. La votación en general pesa más que la de un artículo.
2. **Margen** |Sí − No| / (Sí + No), con `total_si` y `total_no` de `votaciones_urgencia_2002_2026.csv`. La mediana baja de 0,98 en 2002-10 a 0,46 en 2022-26. Es posterior a la votación y depende de la propia disciplina.
3. **Punto de corte del modelo ideológico:** reestimar `emIRT::dynIRT` guardando los parámetros de cada votación y medir cuántos oficialistas quedan al otro lado del corte. Es la medida más directa, pero requiere reestimar.
