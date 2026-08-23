# Auditoría de actualización 2022–2026

- Fecha de actualización: 2026-08-22
- Rama: `update-2022-26-rollcalls-ideology`
- Origen local: `D:/varios/monitor_congreso/data`
- Alcance temporal: 2022-03-11 (inclusive) a 2026-03-11 (exclusivo)
- Corte observado de la fuente 2022–2026: 2026-03-05 13:39:15
- Primer registro del archivo separado 2026–2030: 2026-03-18 12:47:30

## Flujo y archivos identificados

La réplica histórica de Harvard Dataverse está en `Harvard Dataverse/`. La matriz histórica usada para el último período es `Harvard Dataverse/Roll calls/matriz__periodo_2022_26.csv`. Las estimaciones históricas publicadas están en `Harvard Dataverse/Ideology estimates/ideologia_congreso_chile_2002_2026_{long,wide}_format.csv`.

El artículo usa la estimación dinámica anclada de `pape/polarizacion/code/dynIRT_with_anchors.R`, cuya salida histórica es `pape/polarizacion/data/ideologia_diputados_largo_emIRT_con_anclas.csv`. `code/A_Estimacion_emIRT_dinamica.R` y `Harvard Dataverse/A_Ideology_Estimation.R` son antecedentes metodológicos; para mantener el ancla y el formato del artículo se ejecutó el script anclado.

Los insumos locales relevantes son `data/matriz__periodo_2022_26.csv` y `data/Votaciones_periodo_2022_26.csv`. Los archivos `data/matriz__periodo_2026_30.csv` y `data/Votaciones_periodo_2026_30.csv` se usaron únicamente como controles negativos de exclusión.

Ambigüedades resueltas:

- La solicitud dice una vez “PQR”, pero la carpeta existente es `PRQ`; se creó `PA` al mismo nivel de `PRQ`.
- La matriz local normaliza vacíos históricos a `NA` y difiere en una celda de metadatos. Para impedir cambios históricos, la matriz extendida se construyó desde la versión Harvard y se anexaron exclusivamente las columnas nuevas.
- Hay una votación histórica (`36733`) en la tabla del período sin detalle nominal; ya faltaba en la matriz histórica y no es una omisión nueva.

## Comparación de matrices

| Verificación | Histórica | Extendida |
|---|---:|---:|
| Legisladores/filas | 157 | 157 |
| Votaciones/columnas de voto | 3.393 | 4.126 |
| Votaciones agregadas | — | 733 |
| Votaciones históricas eliminadas | — | 0 |
| Celdas de voto históricas modificadas | — | 0 |
| IDs de legisladores agregados/eliminados | — | 0/0 |
| Solapamiento con IDs de matriz 2026–2030 | — | 0 |
| Solapamiento con IDs de tabla 2026–2030 | — | 0 |

La matriz nueva es `Harvard Dataverse/Roll calls/matriz__periodo_2022_26_extended.csv`. Se genera con `Harvard Dataverse/prepare_2022_26_extended.R`, que falla si cambia la sección histórica, si desaparece una votación antigua o si aparece cualquier ID del archivo 2026–2030.

SHA-256:

- Histórica: `C9932940E59E78C373977A87B5A8DCDA3C223F3DEED5FF086D4F55E730B328BC`
- Extendida: `7B6593A78AABF97656FCFA4A87815FC7144579513273275C247673AED893A261`

## IDs de las 733 votaciones agregadas

```text
82602, 82603, 82604, 82605, 82606, 82607, 82608, 82609, 82610, 82613, 82614, 82615, 82616, 82617, 82618, 82619, 82620, 82621, 82622, 82623
82624, 82625, 82626, 83348, 83349, 83350, 83351, 83365, 83366, 83367, 83368, 83369, 83397, 83398, 83399, 83400, 83401, 83442, 83443, 83444
83445, 83468, 83497, 83546, 83547, 83548, 83549, 83550, 83551, 83552, 83553, 83554, 83555, 83556, 83576, 83577, 83578, 83579, 83580, 83581
83582, 83583, 83584, 83585, 83586, 83587, 83588, 83589, 83590, 83591, 83592, 83593, 83594, 83595, 83596, 83597, 83598, 83599, 83600, 83601
83602, 83603, 83604, 83605, 83606, 83607, 83608, 83609, 83610, 83611, 83612, 83613, 83614, 83615, 83616, 83617, 83618, 83619, 83620, 83621
83656, 83657, 83658, 83686, 83687, 83688, 83721, 83722, 83751, 83752, 83777, 83778, 83788, 83789, 83791, 83792, 83794, 83795, 83796, 83814
83815, 83816, 83817, 83818, 83819, 83820, 83821, 83852, 83853, 83854, 83855, 83856, 83857, 83899, 83900, 83901, 83902, 83970, 83971, 83972
83973, 84007, 84009, 84033, 84035, 84037, 84039, 84094, 84095, 84124, 84125, 84126, 84127, 84169, 84170, 84171, 84172, 84173, 84199, 84200
84201, 84202, 84203, 84204, 84205, 84206, 84207, 84208, 84209, 84210, 84211, 84212, 84213, 84214, 84215, 84216, 84217, 84218, 84219, 84220
84221, 84222, 84223, 84224, 84225, 84226, 84227, 84228, 84229, 84230, 84231, 84232, 84233, 84234, 84235, 84236, 84237, 84238, 84239, 84240
84241, 84242, 84243, 84244, 84245, 84246, 84247, 84248, 84249, 84250, 84251, 84252, 84253, 84254, 84255, 84256, 84257, 84258, 84259, 84260
84261, 84262, 84263, 84264, 84265, 84266, 84267, 84268, 84269, 84271, 84272, 84273, 84332, 84334, 84335, 84385, 84386, 84409, 84410, 84411
84412, 84419, 84420, 84421, 84422, 84423, 84424, 84465, 84466, 84550, 84551, 84552, 84553, 84554, 84555, 84556, 84577, 84578, 84616, 85232
85247, 85295, 85296, 85297, 85298, 85299, 85300, 85301, 85302, 85358, 85359, 85360, 85384, 85385, 85386, 85387, 85388, 85389, 85390, 85391
85393, 85420, 85421, 85569, 85570, 85598, 85599, 85600, 85601, 85602, 85603, 85604, 85629, 85630, 85631, 85836, 85837, 85838, 85839, 85840
85841, 85842, 85843, 85844, 85845, 85846, 85847, 85848, 85849, 85850, 85895, 85897, 85898, 85899, 86122, 86124, 86125, 86143, 86144, 86145
86200, 86210, 86211, 86212, 86241, 86242, 86243, 86244, 86245, 86246, 86247, 86317, 86318, 86319, 86320, 86324, 86326, 86327, 86328, 86329
86330, 86331, 86332, 86333, 86334, 86336, 86337, 86340, 86343, 86344, 86347, 86352, 86353, 86354, 86355, 86356, 86357, 86358, 86360, 86361
86379, 86380, 86381, 86382, 86383, 86384, 86385, 86386, 86387, 86388, 86389, 86390, 86391, 86392, 86395, 86396, 86397, 86398, 86399, 86590
86400, 86402, 86403, 86404, 86405, 86406, 86407, 86408, 86409, 86410, 86411, 86412, 86413, 86414, 86415, 86416, 86417, 86418, 86419, 86420
86421, 86422, 86423, 86424, 86425, 86426, 86427, 86428, 86429, 86430, 86431, 86432, 86433, 86434, 86435, 86436, 86437, 86438, 86439, 86440
86507, 86508, 86509, 86510, 86511, 86512, 86513, 86514, 86515, 86529, 86530, 86531, 86564, 86565, 86566, 86567, 86568, 86569, 86570, 86571
86572, 86573, 86574, 86575, 86576, 86577, 86578, 86579, 86580, 86581, 86582, 86583, 86600, 86601, 86638, 86685, 86686, 86687, 86725, 86726
86727, 86728, 86729, 86730, 86731, 86732, 86764, 86765, 86766, 86855, 86856, 86857, 86858, 86859, 86913, 86914, 86915, 86916, 86917, 86918
86919, 86920, 86921, 86922, 86923, 86924, 86925, 86926, 86927, 86928, 86929, 86930, 86931, 86932, 86933, 86934, 86994, 86995, 86996, 86997
86998, 86999, 87026, 87027, 87028, 87063, 87064, 87065, 87066, 87067, 87087, 87088, 87089, 87090, 87091, 87092, 87093, 87094, 87095, 87096
87137, 87138, 87139, 87140, 87141, 87142, 87188, 87189, 87190, 87191, 87212, 87213, 87243, 87244, 87245, 87246, 87247, 87248, 87249, 87250
87251, 87252, 87253, 87254, 87255, 87256, 87257, 87258, 87259, 87260, 87261, 87262, 87263, 87264, 87265, 87266, 87267, 87268, 87269, 87270
87271, 87272, 87273, 87274, 87275, 87276, 87277, 87278, 87279, 87280, 87281, 87282, 87283, 87284, 87285, 87286, 87287, 87288, 87289, 87290
87291, 87292, 87293, 87294, 87295, 87296, 87297, 87298, 87299, 87300, 87301, 87302, 87303, 87304, 87305, 87306, 87307, 87308, 87309, 87310
87311, 87348, 87350, 87351, 87352, 87353, 87379, 87380, 87381, 87382, 87383, 87384, 87385, 87386, 87387, 87388, 87389, 87390, 87391, 87392
87393, 87394, 87395, 87396, 87397, 87398, 87399, 87400, 87401, 87402, 87403, 87404, 87405, 87406, 87407, 87408, 87409, 87410, 87411, 87412
87413, 87414, 87415, 87418, 87419, 87420, 87421, 87422, 87423, 87424, 87455, 87456, 87457, 87458, 87459, 87460, 87461, 87462, 87463, 87464
87465, 87466, 87467, 87468, 87469, 87519, 87520, 87521, 87522, 87523, 87524, 87525, 87552, 87553, 87554, 87555, 87556, 87584, 87585, 87586
87587, 87588, 87589, 87590, 87591, 87662, 87663, 87664, 87665, 87666, 87667, 87668, 87688, 87689, 87690, 87691, 87692, 87693, 87694, 87695
87696, 87697, 87698, 87699, 87726, 87727, 87728, 87729, 87730, 87731, 87732, 87733, 87735, 87736, 87737, 87738, 87739, 87740, 87753, 87754
87782, 87783, 87784, 87785, 87786, 87787, 87788, 87789, 87790, 87791, 87792, 87793, 87794
```

## Estimación ideológica extendida

Se ejecutó `emIRT::dynIRT` con la metodología del artículo: semilla 123; ancla DiputadoId 917; valor del ancla 3; desviación del ancla 0,01; 4 hilos; umbral `1e-6`; máximo 1.000 iteraciones. El algoritmo convergió en 103 iteraciones.

La salida adicional es `pape/polarizacion/data/ideologia_diputados_largo_emIRT_con_anclas_2022_26_extended.csv`. Contiene la trayectoria conjunta de seis períodos porque el modelo dinámico se estima en bloque, pero el único insumo de roll calls distinto es la matriz extendida de 2022–2026.

| Verificación | Histórica | Extendida |
|---|---:|---:|
| Filas legislador–período | 2.574 | 2.574 |
| Claves duplicadas | 0 | 0 |
| Conjunto de claves idéntico | sí | sí |
| Estimaciones no faltantes en Período 6 | 157 | 157 |

Para Período 6, la correlación entre la estimación histórica y la extendida es 0,9972224 y el error absoluto medio es 0,1386801. En Períodos 1–5 se conservan exactamente las unidades y conteos no faltantes; las pequeñas diferencias numéricas (MAE entre 0,00125 y 0,00269) son esperables porque `dynIRT` estima conjuntamente la trayectoria temporal. Los archivos históricos no fueron modificados ni borrados.

SHA-256:

- Estimación histórica anclada: `62EB7B3E4AF408F71D52CF8810A527077470AC2BD217E0206A4B335D198D47A4`
- Estimación extendida anclada: `51C617C8E15CBAAA5EB85B8CC087C9E35BDD4C2CD680DFF7968AC07B0F3B65C5`

## Preparación de PA

Se creó `pape/polarizacion/docs/PA/` con copias fuente renombradas de los tres documentos solicitados. `PA/main_document.qmd` define `pa_rollcall_path` y `pa_ideology_path` para seleccionar explícitamente las versiones extendidas. El manuscrito PRQ y sus rutas históricas permanecen intactos.

## Confirmaciones de alcance

- No se modificó ni borró ningún insumo histórico Harvard.
- Las matrices de 2002–2006 a 2018–2022 no cambiaron.
- La nueva matriz solo anexa 733 votaciones al período 2022–2026.
- No se incorporó ningún dato del período 2026–2030.
- La estimación nueva es adicional y la histórica permanece disponible.
- Los archivos temporales de ejecución no forman parte de la actualización.
