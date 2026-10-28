# Controles de calidad (no versionados)

Tablas que producen los pasos 4, 5 y 8. Se versionaron hasta el commit `e835c8f` (28-10-2026) y después se sacaron de git para alivianar la rama; se regeneran corriendo el flujo (ver [`src/urgencias/README.md`](../../../src/urgencias/README.md)). Los resultados principales están en el [informe](../REPORTE_URGENCIAS.md).

Para ver la versión versionada: `git show e835c8f:camara-cl-api/outputs/urgencias/controles/<archivo>`.

| Archivo | Paso | Qué muestra | Resultado al 28-10-2026 |
|:--|:--|:--|:--|
| `validacion_votos_tabla.csv`, `validacion_votos_resumen.csv` | 4 | Votos de las matrices contra el detalle de la Cámara, celda por celda | 0 discrepancias en 446.488 celdas con voto |
| `coalicion_cobertura.csv` | 5 | Partidos de las militancias presentes en `coalicion_gobierno.csv` | Todos cubiertos |
| `militancia_vs_long_format.csv` | 5 | Militancias con fecha frente a `party_1..party_4` de `long_format` | 135 diputados-período donde `long_format` agrega partidos vecinos y 10 distintos |
| `control_faltantes_por_periodo.csv` | 8 | Votaciones sin fecha, boletín, proyecto, tipo o iniciativa; y de proyectos que nunca tuvieron urgencia | Ninguna con datos faltantes; 5.916 de proyectos sin urgencias |
| `control_urg_nivel_periodo_iniciativa.csv` | 8 | `urg_nivel` por período e iniciativa | Tabla en la sección 3 del informe |
| `control_variacion_por_periodo.csv` | 8 | Proyectos con urgencia distinta entre sus votaciones | 635 en 2002-26 (sección 2 del informe) |
| `control_panel_filas.csv` | 8 | Filas del panel frente a las celdas de las matrices | Coinciden en los 6 períodos |
| `control_ausencias_largas.csv` | 8 | Rachas de 100 o más votaciones seguidas sin voto estando en funciones | 337 rachas; de ahí salieron las suspensiones verificadas |
| `control_oficialismo.csv` | 8 | Celdas en funciones por valor de `oficialista` y motivo | Tabla en la sección 5 del informe |
| `control_cambios_militancia.csv` | 8 | Diputados con cambios de partido fechados dentro del período | 8, 37 y 55 en 2014-18, 2018-22 y 2022-26 |
