# Urgencias del Ejecutivo en las votaciones de la Cámara, 2014-2026

Informe de construcción de la base para el proyecto de Amanda (Economía Política Formal 2026, DCCS-UDD).

- Preparado el 28-10-2026 en la rama `feature/urgencias-votaciones` del repositorio Monitor Congreso.
- Los scripts están en [`camara-cl-api/src/urgencias/`](../../src/urgencias/) y su orden de ejecución en el [README](../../src/urgencias/README.md).
- El diccionario de datos está en [`diccionario.csv`](diccionario.csv).

## 1. Resumen

| Qué | Resultado |
|:--|:--|
| Votaciones con urgencia reconstruida | **10.913** (2014-18: 3.267; 2018-22: 3.520; 2022-26: 4.126, de ellas 733 de la matriz extendida). Ninguna queda sin fecha, boletín o proyecto en el Senado |
| Proyectos | 1.853 (573 con votaciones en 2014-18, 1.313 en 2018-26, 33 en ambos) |
| Historial de urgencias (Senado) | 17.921 urgencias en 1.172 proyectos; 6.069 ingresadas a la Cámara son las que se usan |
| Base larga diputado × votación | **1.616.849 filas** (las que implican las matrices), sin duplicados de (vote_id, diputado_id); 34,5 MiB en `.csv.gz` |
| Votos de las matrices contra el detalle de la Cámara | 0 discrepancias en 432.620 celdas con voto (4.150 votaciones revisadas) |
| Proyectos con urgencia distinta entre sus votaciones (muestra para P5) | 102 en 2014-18, 95 en 2018-22, 80 en 2022-26; 318 si se junta todo el período (189 mensajes) |
| No podían repostular por la Ley 21.238 | 37 diputados en 2018-22 (los 33 de la prensa más 4 que dejaron la Cámara antes de la elección) y 9 en 2022-26 |
| Oficialismo sin codificar (independientes) | 6,9 % de las celdas en funciones en 2014-18, 10,4 % en 2018-22 y 24,6 % en 2022-26 |

## 2. Cómo se mide la urgencia en cada votación

Para cada votación se busca qué urgencias del Ejecutivo estaban vigentes **en la Cámara** ese día. Solo cuentan las que el Senado registra como ingresadas a la Cámara de Diputados. Cada urgencia tiene una fecha de ingreso, un plazo legal (30 días la simple, 15 la suma y 6 la discusión inmediata) y, a veces, una fecha de retiro. El Ejecutivo suele retirar una urgencia después de vencido su plazo, o no retirarla nunca. Por eso hay dos versiones:

- **Estricta** (`urg_nivel`, igual a `lvl_A` del curso): la urgencia rige desde su ingreso hasta lo que ocurra primero, el retiro o el fin del plazo legal. Es la lectura de la ley: vencido el plazo, la urgencia ya no obliga.
- **Laxa** (`urg_nivel_laxa`, igual a `lvl_B`): la urgencia rige desde su ingreso hasta su retiro, aunque el plazo ya haya vencido. Si no hay retiro, rige hasta el fin del plazo legal. Refleja que la urgencia sigue figurando en la tramitación mientras el Ejecutivo no la retire.

Si hay varias vigentes el mismo día, se toma la de mayor nivel: 0 sin urgencia, 1 simple, 2 suma, 3 discusión inmediata.

**Ejemplo (votación 19047, boletín 9279, 15-09-2015).** Una urgencia suma ingresó el 20-08-2015 y se retiró el 19-09, después de que venciera su plazo de 15 días (04-09). El día de la votación, la versión estricta la da por vencida y asigna nivel 1, por una simple ingresada el 03-09. La laxa la sigue contando y asigna nivel 2.

Las dos versiones coinciden en el 99 % de las votaciones. En las 110 restantes la laxa es mayor, nunca menor.

**Variación de urgencia.** Un proyecto tiene variación si se votó al menos dos veces y sus votaciones no tienen todas el mismo nivel de urgencia. Es la muestra que sirve para comparar votaciones dentro de un mismo proyecto (P5). Por ejemplo, un proyecto votado en general sin urgencia y después en particular con suma tiene variación.

- **Variación estricta:** los niveles difieren según `urg_nivel`. Son 318 proyectos en 2014-26 (189 mensajes).
- **Variación laxa:** los niveles difieren según `urg_nivel_laxa`. Son 304 proyectos (180 mensajes).
- 302 proyectos tienen variación en las dos versiones. El detalle por período está en la sección 10.2.

## 3. Qué se hizo

| Paso | Script | Qué hace |
|:--|:--|:--|
| 1 | `01_exportar_votaciones.R` | Exporta los metadatos de 2018-26 desde `details__periodo_2018_22.json` y `details__periodo_2022_26.json` (6.913 votaciones). Toma los IDs de las matrices: 3.267 de 2014-18 y 733 nuevas de 2022-26. Sortea la muestra de validación (150) |
| 2 | `02_descargar.py` | Descarga con caché el detalle de 4.150 votaciones, el historial de 1.853 proyectos en el Senado, las votaciones de cada proyecto en la Cámara (1.853) y los diputados con sus militancias (7 períodos) |
| 3 | `03_urgencia_en_la_votacion.R` | Calcula la urgencia vigente en cada votación, el tipo de votación y los controles de fechas |
| 4 | `04_validar_votos.R` | Compara celda por celda las matrices con el detalle de la Cámara |
| 5 | `05_partido_y_coalicion.R` | Arma las militancias con fecha por período, las contrasta con `party_1..party_4` y verifica que la tabla de coalición cubra todos los partidos |
| 6 | `06_horizonte_ley21238.R` | Calcula el tiempo en el cargo por período y el límite a la reelección |
| 7 | `07_panel_votos.R` | Arma la base larga |
| 8 | `08_controles.R` | Corre los controles de calidad |

Al terminar se corrió el flujo completo desde cero sobre la caché. Hizo 0 peticiones nuevas y produjo resultados idénticos byte a byte.

## 4. Fuentes y fechas de descarga

| Fuente | Qué se pidió | Peticiones | Descargado |
|:--|:--|--:|:--|
| Cámara, `WSLegislativo.asmx/retornarVotacionDetalle?prmVotacionId=` | Fecha, boletín, quórum, resultado y voto de cada diputado | 4.150 | 28-10-2026, 08:42-08:47 |
| Cámara, `WSLegislativo.asmx/retornarVotacionesXProyectoLey?prmNumeroBoletin=` | Tipo de votación (general/particular/única), artículo, trámite y totales | 1.853 | 28-10-2026, 08:47-08:49 |
| Cámara, `WSDiputado.asmx/retornarDiputadosXPeriodo?prmPeriodoId=` (5 a 11) | Diputados, fecha de salida y militancias con fechas | 7 | 28-10-2026, 08:42 |
| Senado, `tramitacion.senado.cl/wspublico/tramitacion.php?boletin=` | Historial de urgencias (`<urgencias>`), iniciativa y estado | 1.853 | 28-10-2026, 08:42-08:48 |
| BCN LeyChile | Ley 18.918, Constitución y Ley 21.238 (enlaces en la sección 5) | 4 | 28-10-2026, 08:38-08:40 |
| Repositorio | Matrices de Harvard Dataverse (2006-10 a 2022-26 extendida), `long_format` de ideología, JSON `details__periodo_*` de la rama API | — | Versión de esta rama |
| Repositorio, historial | Ideología dinámica con la matriz extendida (`git show 6c91e1a:pape/...extended.csv`) | — | 28-10-2026 |

- Las descargas corrieron con 6 conexiones por servidor y 0,05 s de pausa. No hubo ningún error.
- `www.camara.cl` no se usó.
- Las fuentes de prensa usadas para la coalición y las suspensiones están en las columnas `fuente` y `fuente_url` de [`coalicion_gobierno.csv`](coalicion_gobierno.csv) y [`suspensiones_desafuero.csv`](suspensiones_desafuero.csv).

## 5. Normas verificadas en BCN

- **Plazos de las urgencias.** La [Ley 18.918](https://www.bcn.cl/leychile/navegar?idNorma=30289), art. 27, fija que la discusión y votación "deberán quedar terminadas en el plazo de treinta días" (simple); "quince días" (suma) y "seis días" (discusión inmediata). El texto es el original, sin modificaciones desde 1990. La [Constitución](https://www.bcn.cl/leychile/navegar?idNorma=242302), art. 74, fija el plazo máximo de 30 días y remite la calificación a la ley orgánica. Por eso se usan 30, 15 y 6 días.
- **Matices del art. 27 que la base no modela:**
  - El plazo corre desde la cuenta del oficio "en la sesión más próxima"; la base usa la fecha de ingreso del Senado.
  - El retiro rige "en el acto mismo" de su recepción.
  - El art. 28 fija plazos más cortos en la comisión mixta: 10, 5 y 2 días por cámara. Las votaciones con `tramite_constitucional = "Comisión Mixta"` pueden tratarse aparte.
  - Los arts. 26 a 29 no se aplican a la Ley de Presupuestos (art. 26, inciso tercero). Por eso esas votaciones van marcadas en `ley_presupuestos`.
- **Límite a la reelección.** La [Ley 21.238](https://www.bcn.cl/leychile/navegar?idNorma=1147301) (D.O. 08-07-2020) reemplazó el inciso segundo del art. 51 de la Constitución: "Los diputados podrán ser reelegidos sucesivamente en el cargo hasta por dos períodos [...] se entenderá que los diputados y senadores han ejercido su cargo durante un período cuando han cumplido más de la mitad de su mandato". **La ley no tiene disposiciones transitorias.**
- **Suspensión por desafuero.** Constitución, art. 61: "Desde el momento en que se declare, por resolución firme, haber lugar a formación de causa, queda el diputado o senador imputado suspendido de su cargo".

## 6. 2014-2018 y las 733 votaciones nuevas de 2022-26

- **2014-18:** 3.267 votaciones (13-03-2014 a 06-03-2018), todas "Proyecto de Ley" y con boletín, en 573 proyectos. El 59,0 % tiene urgencia vigente en la versión estricta (59,3 % en la laxa).
- **733 nuevas de 2022-26** (04-06-2025 a 05-03-2026): todas tienen fecha, boletín, proyecto en el Senado, urgencia y tipo de votación. Por nivel: 215 sin urgencia, 9 simple, 157 suma y 352 discusión inmediata; en total, 70,7 % con urgencia.
- **Ley de Presupuestos:** 1.792 votaciones de 11 proyectos (580 en 2014-18, 585 en 2018-22, 627 en 2022-26). Solo 17 tienen alguna urgencia registrada. Conviene excluirlas o controlarlas, porque no se rigen por urgencias.
- **Fechas mal escritas:** 2 ingresos con año de dos dígitos (boletines 6639 y 6252: "05/01/0010" pasa a 2010) y 11 retiros anteriores al ingreso. Ninguno se borró: quedan marcados en `f_ingreso_corregida` y `retiro_antes_ingreso`.

## 7. Base larga diputado × votación

Archivo: [`panel_votos_2014_2026.csv.gz`](panel_votos_2014_2026.csv.gz). Una fila por celda de `matriz__periodo_2014_18.csv` (121 × 3.267), `matriz__periodo_2018_22.csv` (163 × 3.520) y `matriz__periodo_2022_26_extended.csv` (157 × 4.126).

### Voto

**Validación.** Se compararon las matrices con el detalle de la Cámara en 2014-18 completo, las 733 nuevas y una muestra de 150 votaciones de 2018-26 (75 y 75). En las 432.620 celdas con voto, la matriz y la Cámara coinciden en el 100 %. Al cruzar ambas fuentes se ve lo siguiente:

- **NA** en la matriz equivale a que el diputado no figura en el registro de la Cámara.
- **"No Vota"** equivale a "No Vota" en la Cámara. La Cámara cambió su registro el **09-04-2025**: desde entonces lista a los 155 diputados en ejercicio y marca "No Vota" a quien no votó. Antes, el ausente simplemente no aparecía.
- **Celda vacía** (solo en las 733 nuevas, 2 por votación) equivale a no figurar en el registro: son los 2 diputados que ya habían dejado el cargo.
- **No hay pareos** en ninguna de las dos fuentes: la categoría no se puede construir con estos datos.

**Homologación (`voto`).** `voto_original` conserva la celda tal cual. `voto` la lleva a estas categorías:

- Afirmativo, En contra, Abstención, Dispensado.
- **Ausente o no vota:** en funciones y sin voto. Corresponde a NA antes de abril de 2025 y a "No Vota" después; no distingue ausencia de presencia sin votar.
- **No en funciones:** antes de asumir o después de dejar el cargo.
- **No en funciones (suspendido):** desafuero firme.

`voto_si` vale 1 para Afirmativo, 0 para En contra y NA en el resto.

| voto | 2014-18 | 2018-22 | 2022-26 |
|:--|--:|--:|--:|
| Afirmativo | 253.968 | 334.405 | 362.645 |
| En contra | 51.810 | 110.504 | 138.554 |
| Abstención | 14.305 | 20.756 | 25.968 |
| Dispensado | 1.073 | 898 | 520 |
| Ausente o no vota | 67.598 | 78.970 | 107.026 |
| No en funciones | 3.384 | 28.227 | 8.501 |
| No en funciones (suspendido) | 3.169 | 0 | 4.568 |

### En funciones

- **Salidas:** `retornarDiputadosXPeriodo` trae en el campo `FechaInicio` la fecha en que el diputado **dejó** el cargo. En los titulares coincide con el fin del período. Esto se dedujo de los datos y calza con hechos conocidos: Bellolio y Desbordes, 28-07-2020 (asumen como ministros); Insunza, 12-05-2015.
- **Entradas de reemplazantes:** se toman de su primera votación registrada (aproximación de días). Hay 1 salida y 1 reemplazo en 2014-18, 8 y 8 en 2018-22, y 2 y 2 en 2022-26, que calzan exactamente con las filas de cada matriz.
- **Suspensiones:** [`suspensiones_desafuero.csv`](suspensiones_desafuero.csv) lista 5 casos con su fuente. Rosauro Martínez desde el 17-06-2014; María Luisa Cordero del 04-09-2023 al 04-03-2024; Mauricio Ojeda desde el 30-09-2024; Francisco Pulgar desde el 11-12-2024; Catalina Pérez desde el 07-04-2025. La suspensión se marca en la racha de ausencia (20 o más votaciones seguidas) que sigue a la resolución firme. Así se respeta el tiempo que tarda la comunicación a la Cámara.
- Ningún voto queda fuera de un período en funciones.

### Ideología

- El panel trae `ideol_dyn` (`dim_ideology`, emIRT::dynIRT publicada), `ideol_wnom` (W-NOMINATE) e `ideol_bay` (pscl::ideal), todas de `ideologia_congreso_chile_2002_2026_long_format.csv`, redondeadas a 4 decimales.
- Los intervalos de esas estimaciones están en [`ideologia_diputado_periodo.csv`](ideologia_diputado_periodo.csv), que se une por (periodo, diputado_id). Se sacaron del panel para que pesara menos de 50 MB; solo hacen falta si se quiere usar la incertidumbre de la ideología.
- La versión dinámica estimada con la matriz extendida (`ideol_dyn_ext`) sale del commit `6c91e1a` y se escribe solo en `no_versionado/ideologia_dyn_ext_2014_2026.csv`, que git ignora. Se une por (periodo, diputado_id).
- En 2014-26, la versión dinámica publicada, la de una ancla y la de tres anclas son numéricamente idénticas. La extendida coincide con ellas en 2014-22 y difiere solo en 2022-26 (correlación 0,997).
- Pablo Prieto Lorca figura en la matriz 2018-22 con el Id 1064 y en `long_format` con el Id 209 (su Id de los años noventa). Se unió con una tabla de equivalencia explícita en el paso 7.

### Partido a la fecha

- Se toma de las militancias con fecha de la Cámara (WSDiputado), la misma fuente que usó `B_Add_Variables.R` para `party_1..party_4`.
- **Todos los cambios dentro del período tienen fecha:** 8 diputados en 2014-18, 37 en 2018-22 y 55 en 2022-26 (6, 21 y 38 de ellos pasan por independiente). Ningún diputado-período queda sin militancia fechada.
- Diferencias con `party_1..party_4`: en 59 diputados-período `long_format` agrega partidos de períodos vecinos (por ejemplo, Godoy figura con "AMPL, RN" en 2014-18, pero militó en RN en 2006-14), y en 10 casos los partidos son distintos. El detalle está en [`controles/militancia_vs_long_format.csv`](controles/militancia_vs_long_format.csv).
- Quedan 198 celdas en funciones sin militancia registrada a la fecha, todas de Gaspar Rivas en 2014-18, y 1 militancia con fecha de término anterior a la de inicio.
- Cuando hay dos partidos a la misma fecha se usa el registro más reciente y se marca `oficialista_dudoso = 1`. Son 3 diputados: Álvaro Carter (UDI e IND, 1.410 celdas) y Camila Rojas (IGUAL e IND, 2 celdas) en 2018-22, y Félix Bugueño (FRVS e IND, 860 celdas) en 2022-26.

## 8. Oficialismo y oposición

La tabla [`coalicion_gobierno.csv`](coalicion_gobierno.csv) tiene una fila por partido y gobierno, con fechas, nota y fuente. Cubre todos los partidos presentes en las militancias. Así quedó la codificación:

- **Bachelet II (11-03-2014 a 10-03-2018):** Nueva Mayoría = PS, PPD, DC, PR (PRSD), PC, IC y MAS (BCN). La DC, la IC y el MAS compitieron en 2017 en un pacto parlamentario aparte, pero BCN no registra un retiro del gobierno. RD y PL no integraron la coalición (dudoso: relación con el gobierno no verificada).
- **Piñera II (11-03-2018 a 10-03-2022):** Chile Vamos = RN, UDI, Evópoli y PRI; el PRI sale en febrero de 2022 por su disolución (BCN). Los Republicanos quedan como no oficialistas y dudosos: no integraron Chile Vamos ni el gabinete, pero pactaron con la coalición en 2021.
- **Boric (11-03-2022 a 10-03-2026):**
  - Oficialistas: RD, CS, Comunes, el partido FA (fusión de los tres), PC, FRVS y Acción Humanista (Apruebo Dignidad), más PS, PPD, PR y PL (Socialismo Democrático).
  - Socialismo Democrático tuvo ministros desde el gabinete inicial (11-03-2022), así que se codifica desde esa fecha; su peso creció tras el plebiscito de 2022.
  - FRVS: oficialista hasta el 19-08-2025. Desde el 20-08-2025 (Boric remueve a su ministro tras el quiebre del pacto parlamentario) queda NA y dudoso.
  - Acción Humanista: oficialista y dudoso, porque en 2026 declaró no ser parte del gobierno, sin fecha de alejamiento verificada.
  - DC y PEV: no oficialistas y dudosos (DC: oposición colaborativa, sin ministros ni subsecretarios).
- **Independientes:** NA y dudoso en los tres gobiernos, por regla ("no adivinar"). Su pertenencia depende de la bancada o del pacto por el que fueron electos, que no están en estos datos.

Celdas en funciones según la codificación del oficialismo:

| | 2014-18 | 2018-22 | 2022-26 |
|:--|--:|--:|--:|
| Oficialista (1) | 53,4 % | 43,7 % | 33,2 % (1,1 % dudoso) |
| No oficialista (0) | 39,7 % (1,7 % dudoso) | 45,8 % (0,5 % dudoso) | 42,1 % (3,5 % dudoso) |
| NA: independiente | 6,9 % | 10,4 % | 24,6 % |
| NA: otros motivos | 0,1 % (sin militancia) | 0 | 0,1 % (FRVS desde agosto de 2025) |

## 9. Horizonte por la Ley 21.238

Cómo cuenta los períodos:

1. Un diputado puede ser reelegido sucesivamente dos veces: máximo tres períodos consecutivos.
2. Un período cuenta si ejerció más de la mitad del mandato (más de 2 de los 4 años).
3. La ley no tiene transitorias, así que cuentan también los períodos anteriores a 2020 (así se aplicó en 2021).

Codificación:

- `no_puede_repostular = 1` si el diputado ejerció más de la mitad del período actual y de los dos anteriores.
- En 2014-18 vale 0, porque la ley no existía; `habria_estado_afectado` lo calcula igual: 44 casos.
- `horizonte_corto` (en el panel) aplica desde la publicación (08-07-2020). En 2018-22 cubre 56.407 celdas en funciones y en 2022-26, 37.134.

Lista para revisar a mano:

- **2018-22 (37):**
  - Coinciden con la lista de prensa de 33 ([Radio Agricultura, 17-11-2021](https://www.radioagricultura.cl/noticias/politica/los-33-diputados-que-no-iran-por-un-nuevo-periodo-en-la-camara-por-aplicacion-de-ley-de-limite-de-reeleccion_20211117/)): Alejandra Sepúlveda, Alejandro Santana, Carlos Abel Jarpa, Celso Morales, Cristina Girardi, Enrique Van Rysselberghe, Fernando Meza, Fidel Espinoza, Gabriel Silber, Guillermo Teillier, Ignacio Urrutia, Iván Norambuena, Javier Hernández, Javier Macaya, Jorge Sabag, José Miguel Ortiz, José Pérez, Juan Luis Castro, Leopoldo Pérez, Manuel Monsalve, Marcelo Schilling, Mario Venegas, María José Hoffmann, Matías Walker, Pablo Lorenzini, Pedro Pablo Álvarez-Salamanca, Pepe Auth, Ramón Barros, René Manuel García, René Saffirio, Rodrigo González, Tucapel Jiménez y Víctor Torres.
  - Además, 4 que dejaron la Cámara antes de la elección tras ejercer más de la mitad del período. Por eso no están en la lista de prensa, que contó solo a los diputados en ejercicio: Patricio Melero (77 % del período), Hugo Gutiérrez (72 %), Marcela Sabat (60 %) e Issa Kort (82 %).
- **2022-26 (9):** Bernardo Berger, Daniella Cicardini, Joaquín Lavín León, Jorge Rathgeb, Juan Antonio Coloma Álamos, Karol Cariola, Leonardo Soto, Renzo Trisotti y Vlado Mirosevic. Los 9 ejercieron completos 2014-18, 2018-22 y 2022-26.
- **Casos límite:** Bellolio y Desbordes ejercieron el 59,6 % de 2018-22 (cuenta como período), pero no acumulan tres períodos consecutivos.

## 10. Controles de calidad

### 10.1 `urg_nivel` (estricta) por período e iniciativa

| Período | Iniciativa | 0 | 1 simple | 2 suma | 3 disc. inmediata | % con urgencia |
|:--|:--|--:|--:|--:|--:|--:|
| 2014-18 | Mensaje | 980 | 75 | 808 | 740 | 62,4 |
| 2014-18 | Moción | 360 | 57 | 185 | 62 | 45,8 |
| 2018-22 | Mensaje | 860 | 117 | 602 | 711 | 62,4 |
| 2018-22 | Moción | 856 | 95 | 146 | 133 | 30,4 |
| 2022-26 | Mensaje | 1.603 | 17 | 454 | 921 | 46,5 |
| 2022-26 | Moción | 494 | 68 | 329 | 240 | 56,3 |

Los ceros de los mensajes incluyen las 1.792 votaciones de la Ley de Presupuestos (627 en 2022-26).

### 10.2 Proyectos con variación de urgencia entre sus votaciones (muestra efectiva)

La definición de variación estricta y laxa está en la sección 2.

| Período | Proyectos con 2 o más votaciones | Con variación (estricta) | Con variación (laxa) | Mensajes con 2 o más | Mensajes con variación (estricta) | Votaciones en esos proyectos |
|:--|--:|--:|--:|--:|--:|--:|
| 2014-18 | 281 | 102 | 96 | 168 | 81 | 1.390 |
| 2018-22 | 371 | 95 | 88 | 152 | 56 | 943 |
| 2022-26 | 333 | 80 | 79 | 140 | 38 | 732 |
| 2014-26 (proyecto entre períodos) | 979 | 318 | 304 | 448 | 189 | 3.625 |

La última fila junta las votaciones de un mismo proyecto aunque caigan en períodos distintos.

### 10.3 Ausencias largas

[`controles/control_ausencias_largas.csv`](controles/control_ausencias_largas.csv) lista 241 rachas de 100 o más votaciones seguidas sin voto estando en funciones. La mayoría son maratones de la Ley de Presupuestos, la pandemia (marzo-abril de 2020) o campañas (fines de 2025). De ahí salieron las 5 suspensiones verificadas. Las demás ausencias largas (por ejemplo, Gaspar Rivas en 2016-17 o la segunda de María Luisa Cordero, de noviembre de 2024 a enero de 2025) quedan como "Ausente o no vota", porque su causa no está verificada.

## 11. Decisiones tomadas

Cada punto dice qué problema había, qué se eligió y qué implica para el análisis.

1. **Solo cuentan las urgencias puestas en la Cámara.** Un proyecto pasa por las dos cámaras y el Ejecutivo pone urgencias en cada una por separado. Una urgencia en el Senado no presiona a los diputados, así que solo se usan las que el Senado registra como ingresadas a la Cámara de Diputados. La vigencia tiene dos versiones (sección 2): la estricta sigue la ley y la laxa sirve para comprobar que los resultados no dependen de esa elección. `urg_nivel` y `urg_nivel_laxa` son iguales a `lvl_A` y `lvl_B` del curso, salvo que quedan NA si falta la fecha, el boletín o el proyecto; hoy no hay ningún caso.
2. **Varias urgencias vigentes el mismo día.** A veces el Ejecutivo pone una urgencia nueva antes de que la anterior venza o se retire, y quedan dos o más vigentes. Se usa la de mayor nivel, porque es el plazo más exigente que enfrenta la Cámara ese día. A igual nivel se usa la más reciente, que es la que define `dias_desde_ingreso_urgencia` y `dias_al_vencimiento`.
3. **Fechas mal escritas en el Senado.** Unos pocos registros traen el año con dos dígitos ("0010" en vez de 2010) o un retiro anterior al ingreso, que es imposible. El año se corrige y el retiro imposible se ignora, así que esa urgencia rige hasta su plazo legal. No se borró ningún registro: quedan marcados en `f_ingreso_corregida` y `retiro_antes_ingreso`.
4. **Tipo de votación.** En cada proyecto se vota primero "en general" (si se aprueba la idea de legislar) y después "en particular" (artículo por artículo). El dato viene de la Cámara. `tipo_votacion_3` lo simplifica en tres grupos: general (incluye "General y Particular", cuando se vota todo de una vez), particular y otro ("Única" y "Admisibilidad"). Sirve para comparar votaciones de un mismo proyecto (P5).
5. **Voto homologado.** La Cámara no registró siempre de la misma forma a quien no votó: antes de abril de 2025 simplemente no aparecía, y después aparece como "No Vota". `voto` lo unifica, para que "Ausente o no vota" signifique lo mismo en todo el período. Además separa a quienes no podían votar porque no estaban en el cargo: aún no asumían, ya se habían ido o estaban suspendidos por desafuero. `voto_si` vale 1 o 0 solo cuando el diputado votó a favor o en contra. El detalle está en la sección 7.
6. **Partido a la fecha de cada votación.** Muchos diputados cambian de partido o renuncian a mitad de período. En vez del partido fijo de `party_1..party_4`, se usa el que tenía el día de cada votación, según las militancias con fecha de la Cámara. Así, un diputado que deja un partido oficialista cuenta como independiente desde ese día. Si la Cámara registra dos partidos el mismo día, se usa el registro más reciente y se marca como dudoso.
7. **Oficialismo.** Un partido es oficialista si formaba parte de la coalición de gobierno o tenía ministros, según BCN y la prensa citada en `coalicion_gobierno.csv`. No se usa el comportamiento en las votaciones para decidirlo, porque eso es justamente lo que se quiere explicar. Los independientes quedan NA: su cercanía al gobierno depende de su bancada o de su pacto electoral, que no están en estos datos. En 2022-26 son casi un cuarto de las celdas. Los casos discutibles llevan `oficialista_dudoso = 1`, para poder repetir el análisis sin ellos.
8. **Horizonte corto.** La Ley 21.238 impide repostular a quien lleva tres períodos seguidos. Un período cuenta si el diputado ejerció más de la mitad, aunque después haya dejado el cargo. `horizonte_corto` vale 1 desde que la ley se publicó (08-07-2020), porque desde ese día el diputado sabía que no podría repostular.
9. **Ideología.** El panel trae tres estimaciones publicadas (dinámica, W-NOMINATE y bayesiana), para poder comprobar que el resultado no depende del método. La versión estimada con la matriz extendida queda fuera de git (sección 7).
10. **Tamaño del panel.** GitHub recomienda archivos de menos de 50 MB. Con 4 decimales en la ideología y sus intervalos en un archivo aparte, el panel pesa 34,5 MiB.

## 12. Propuesta para la "estaca" de cada votación (sin implementar)

En orden de dificultad:

1. **Tipo de votación y trámite (disponible ya).** Ya están `tipo_votacion_3` (general frente a particular), `articulo` y `tramite_constitucional`. La votación en general (idea de legislar) mueve más la política que la de un artículo. Es una medida gruesa, pero no depende del resultado.
2. **Margen de la votación (inmediata).** Se calcula como |Sí − No| / (Sí + No) con `total_si` y `total_no`, que ya están en `votaciones_urgencia_2014_2026.csv`. La mediana es 0,95 en 2014-18, 0,63 en 2018-22 y 0,46 en 2022-26; entre 19 % y 38 % de las votaciones son unánimes. Su problema es que es posterior a la votación y depende de la propia disciplina, así que no sirve como medida previa de la estaca.
3. **Punto de corte del modelo ideológico (factible; requiere reestimar).** Volver a correr `emIRT::dynIRT` (instalado), guardando los parámetros de cada votación (α, β). El punto de corte es −α/β en la escala ideológica. La estaca para el oficialismo sería la distancia entre ese corte y la posición mediana oficialista, o la proporción de oficialistas al otro lado del corte. Hay que alinear el signo por período (el script publicado invierte los períodos 1, 2, 4 y 6) y decidir si se usa la matriz histórica o la extendida.

Recomiendo la 1 y la 2 como controles de inmediato, y la 3 como medida principal si les parece. No implemento ninguna sin su visto bueno.
