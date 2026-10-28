# Urgencias del Ejecutivo en las votaciones de la Cámara (2014-2026)

Reconstruye la urgencia vigente en la Cámara de Diputados el día de cada votación de las matrices de Harvard Dataverse (2014-18, 2018-22 y 2022-26 extendida) y arma una base larga diputado × votación con voto, partido a la fecha, oficialismo, horizonte por la Ley 21.238 e ideología. Es un port, extendido a 2014-2018, del flujo que Aníbal Olivera preparó para el curso Economía Política Formal 2026 (versión del 20-10-2026); se mantienen sus nombres de columnas.

El informe con fuentes, controles y decisiones está en [`../../outputs/urgencias/REPORTE_URGENCIAS.md`](../../outputs/urgencias/REPORTE_URGENCIAS.md) y el diccionario de datos en [`../../outputs/urgencias/diccionario.csv`](../../outputs/urgencias/diccionario.csv).

## Orden de ejecución

Desde la raíz del repositorio (rama `feature/urgencias-votaciones`):

```sh
I=camara-cl-api/outputs/urgencias/intermedios; O=camara-cl-api/outputs/urgencias; C=camara-cl-api/cache/urgencias
Rscript camara-cl-api/src/urgencias/01_exportar_votaciones.R "$PWD" $I        # metadatos ya presentes y listas a descargar
python3 -I camara-cl-api/src/urgencias/02_descargar.py $I $C                  # descargas con caché (Cámara y Senado)
Rscript camara-cl-api/src/urgencias/03_urgencia_en_la_votacion.R $I $O       # urgencia vigente en cada votación
Rscript camara-cl-api/src/urgencias/04_validar_votos.R "$PWD" $I $O          # matrices vs detalle de la Cámara
Rscript camara-cl-api/src/urgencias/05_partido_y_coalicion.R "$PWD" $I $O    # militancias con fecha y cobertura de la coalición
Rscript camara-cl-api/src/urgencias/06_horizonte_ley21238.R "$PWD" $I $O     # límite a la reelección
Rscript camara-cl-api/src/urgencias/07_panel_votos.R "$PWD" $I $O $C         # base larga (.csv.gz)
Rscript camara-cl-api/src/urgencias/08_controles.R $O                        # controles de calidad
```

Requisitos: R con `data.table` y `jsonlite`; Python 3 (solo biblioteca estándar). En macOS se usa el almacén de certificados `/etc/ssl/cert.pem`.

## Descargas

- `02_descargar.py` hace unas 7.900 peticiones (Cámara unas 6.000, Senado unas 1.850). Con 6 conexiones por servidor tarda unos 7 minutos. Si los errores superan el 2 %, primero sube la pausa (de 0,05 a 0,8 s) y solo después baja las conexiones (6, 4, 2, 1).
- Es reanudable: lo que ya está en `camara-cl-api/cache/urgencias/` no se vuelve a pedir. Esa carpeta está en `.gitignore`.
- Servicios: `opendata.camara.cl` (WSLegislativo y WSDiputado) y `tramitacion.senado.cl/wspublico/tramitacion.php`. No se usa `www.camara.cl`, que responde 403 (Cloudflare).

## Insumos editados a mano

Están en `camara-cl-api/outputs/urgencias/` y tienen una fuente por fila:

- `coalicion_gobierno.csv`: partido × gobierno.
- `suspensiones_desafuero.csv`: diputados suspendidos por desafuero firme.

## Archivos que no se versionan

El paso 08 lee la ideología dinámica estimada con la matriz extendida. Antes de correrlo hay que extraerla del historial:

```sh
mkdir -p camara-cl-api/cache/urgencias/insumos_git
git show 6c91e1a:pape/polarizacion/data/ideologia_diputados_largo_emIRT_con_anclas_2022_26_extended.csv \
  > camara-cl-api/cache/urgencias/insumos_git/ideologia_diputados_largo_emIRT_con_anclas_2022_26_extended.csv
```

Ese archivo se despublicó en `2281028`, así que su derivado (`outputs/urgencias/no_versionado/`) queda fuera de git.
