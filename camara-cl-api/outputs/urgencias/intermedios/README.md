# Intermedios (no versionados)

Archivos de trabajo que producen los pasos 1 y 2 y que leen los pasos 3 a 8. Se versionaron hasta el commit `e835c8f` (28-10-2026) y después se sacaron de git para alivianar la rama: se regeneran sin cambios corriendo el flujo completo (ver [`src/urgencias/README.md`](../../../src/urgencias/README.md)). Si la caché de descargas no está, el paso 2 vuelve a pedir todo a la Cámara y al Senado (unas 11.000 peticiones, unos 12 minutos).

Para ver la versión versionada: `git show e835c8f:camara-cl-api/outputs/urgencias/intermedios/<archivo>`.

| Archivo | Paso | Filas | Contenido |
|:--|:--|--:|:--|
| `votes_details_2002_14.csv` | 1 | 7.022 | Metadatos de las votaciones 2002-14 tomados de los JSON `details__periodo_*` |
| `votes_details_2018_26.csv` | 1 | 6.913 | Ídem para 2018-26 |
| `votaciones_matrices.csv` | 1 | 17.935 | Una fila por votación de las matrices: período, matriz, columna y origen de los metadatos |
| `ids_2014_18.txt` | 1 | 3.267 | Votaciones 2014-18, que no tienen JSON y se piden a la Cámara |
| `new_vote_ids.txt` | 1 | 733 | Votaciones de la matriz extendida 2022-26 que no están en la histórica |
| `muestra_validacion_ids.txt` | 1 | 300 | Muestra sorteada (semilla 20261028) para validar los votos de las matrices |
| `votes_2014_18.csv`, `votes_new.csv` | 2 | 3.267 y 733 | Metadatos de esas votaciones según la Cámara (fecha, boletín, quórum, resultado, totales) |
| `votos_detalle_camara.csv.gz` | 2 | 467.897 | Voto de cada diputado en las 4.300 votaciones pedidas a la Cámara |
| `votaciones_proyecto_camara.csv` | 2 | 25.065 | Votaciones de cada proyecto según la Cámara: tipo (general/particular), artículo, trámite y totales |
| `proyectos_camara.csv` | 2 | 3.321 | Un proyecto por fila según la Cámara: iniciativa, fecha de ingreso, cámara de origen |
| `senado_proyectos.csv` | 2 | 3.321 | Un proyecto por fila según el Senado: estado, etapa, iniciativa, número de urgencias |
| `urgency_spells.csv` | 2 | 25.661 | Historial de urgencias en bruto del Senado; el paso 3 lo completa y lo publica en `../urgency_spells.csv` |
| `diputados_periodo.csv` | 2 | 963 | Diputados por período (8 períodos) con su fecha de salida |
| `militancias.csv` | 2 | 1.239 | Militancias con fechas de inicio y término |
| `servicio_diputados.csv` | 6 | 808 | Tiempo en el cargo por diputado y período: entrada, salida, reemplazante y fracción del mandato |
| `descargas_resumen.csv`, `fetch_failures.txt` | 2 | — | Resumen de cada lote de descargas y fallos; nunca se versionaron |
