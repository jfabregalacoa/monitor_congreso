# Auditoría de actualización 2022–2026

- Fecha de actualización: 2026-08-22
- Rama: `update-2022-26-rollcalls-ideology`
- Origen local: `D:/varios/monitor_congreso/data`
- Alcance temporal: 2022-03-11 (inclusive) a 2026-03-11 (exclusivo)
- Corte observado de la fuente 2022–2026: 2026-03-05 13:39:15
- Primer registro del archivo separado 2026–2030: 2026-03-18 12:47:30

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


- No se modificó ni borró ningún insumo histórico Harvard.
- Las matrices de 2002–2006 a 2018–2022 no cambiaron.
- La nueva matriz solo anexa 733 votaciones al período 2022–2026.
- No se incorporó ningún dato del período 2026–2030.
- La estimación nueva es adicional y la histórica permanece disponible.
- Los archivos temporales de ejecución no forman parte de la actualización.
