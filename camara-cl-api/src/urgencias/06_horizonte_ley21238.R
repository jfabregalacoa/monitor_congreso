# Uso: Rscript camara-cl-api/src/urgencias/06_horizonte_ley21238.R <raiz_del_repo> <carpeta_intermedios> <carpeta_salida>
#
# Paso 6. Horizonte: ¿el diputado NO podía repostular a la Cámara por el límite de la Ley 21.238?
# Regla (Constitución, art. 51, texto de la Ley 21.238, D.O. 08-07-2020, BCN idNorma 1147301):
#   1. Los diputados pueden ser reelegidos sucesivamente hasta por dos períodos: máximo 3 períodos consecutivos.
#   2. Un período cuenta si el diputado cumplió más de la mitad del mandato (más de 2 de los 4 años).
#   3. La ley no tiene disposiciones transitorias: cuentan también los períodos anteriores a 2020 (así se aplicó en 2021).
# Un diputado del período P no puede repostular en la elección que cierra P si ejerció más de la mitad de P y de los dos
# períodos anteriores. En 2014-18 la ley no existía (ley_aplica = 0); la columna habria_estado_afectado lo calcula igual.
# Tiempo en el cargo: salida = fecha de WSDiputado.retornarDiputadosXPeriodo (el campo FechaInicio trae la fecha de salida);
# entrada de reemplazantes = primera votación con voto registrado en la matriz (aproximación).
# Salidas en <carpeta_salida>: horizonte_ley21238.csv, intermedios/servicio_diputados.csv
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
a <- commandArgs(TRUE); repo <- a[1]; o <- a[2]; out <- a[3]
P <- data.table(periodo = c("2006_10", "2010_14", "2014_18", "2018_22", "2022_26"), k = 0:4, periodo_id = c("5", "6", "8", "9", "10"),
                p_ini = as.Date(c("2006-03-11", "2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11")),
                p_fin = as.Date(c("2010-03-10", "2014-03-10", "2018-03-10", "2022-03-10", "2026-03-10")),
                matriz = c("matriz__periodo_2006_10.csv", "matriz__periodo_2010_14.csv", "matriz__periodo_2014_18.csv",
                           "matriz__periodo_2018_22.csv", "matriz__periodo_2022_26_extended.csv"),
                eleccion_siguiente = as.Date(c("2009-12-13", "2013-11-17", "2017-11-19", "2021-11-21", "2025-11-16")))
ley_pub <- as.Date("2020-07-08")

json_fechas <- function(p) as.data.table(fromJSON(file.path(repo, "camara-cl-api", "outputs", sprintf("details__periodo_%s.json", p))))[
  , .(vote_id = as.character(vote_id), fecha)]
fechas <- rbind(fread(file.path(out, "votaciones_urgencia_2014_2026.csv"), select = c("vote_id", "fecha"), colClasses = "character"),
                json_fechas("2006_10"), json_fechas("2010_14"))
fechas[, fecha := as.Date(substr(fecha, 1, 10))]
dp <- fread(file.path(o, "diputados_periodo.csv"), colClasses = "character")
dp[, salida := as.Date(substr(fecha_inicio, 1, 10))]

S <- rbindlist(lapply(seq_len(nrow(P)), function(i) {
  m <- fread(file.path(repo, "Harvard Dataverse", "Roll calls", P$matriz[i]), colClasses = "character")
  m <- m[, -1]
  l <- melt(m, id.vars = c("DiputadoId", "Nombre", "ApellidoPaterno", "ApellidoMaterno"), variable.name = "vote_id",
            value.name = "v", variable.factor = FALSE)
  l <- merge(l[!is.na(v) & v != ""], fechas, by = "vote_id")
  r <- l[, .(primer_voto = min(fecha), ultimo_voto = max(fecha)), by = .(diputado_id = DiputadoId, nombre = paste(Nombre, ApellidoPaterno, ApellidoMaterno))]
  todos <- unique(m[, .(diputado_id = DiputadoId, nombre = paste(Nombre, ApellidoPaterno, ApellidoMaterno))])
  r <- merge(todos, r, by = c("diputado_id", "nombre"), all.x = TRUE)
  r <- merge(r, dp[periodo_id == P$periodo_id[i], .(diputado_id, salida_camara = salida)], by = "diputado_id", all.x = TRUE)
  r[, `:=`(periodo = P$periodo[i], k = P$k[i], p_ini = P$p_ini[i], p_fin = P$p_fin[i])]
  primer_global <- min(l$fecha)
  r[, reemplazante := as.integer(!is.na(primer_voto) & primer_voto > primer_global + 45 & salida_camara %in% P$p_fin[i])]
  r[, salio := as.integer(!is.na(salida_camara) & salida_camara < p_fin - 5)]
  r[, servicio_inicio := fifelse(reemplazante == 1L, primer_voto, p_ini)]
  r[, servicio_fin := fifelse(salio == 1L, salida_camara, p_fin)]
  r[, fuente_servicio := fcase(reemplazante == 1L, "entrada = primer voto (aprox.)", salio == 1L, "salida = Cámara (WSDiputado)",
                               default = "período completo")]
  r
}))
S[, fraccion_mandato := round(as.numeric(servicio_fin - servicio_inicio + 1) / as.numeric(p_fin - p_ini + 1), 3)]
S[, cuenta := as.integer(fraccion_mandato > 0.5)]
fwrite(S[, .(periodo, diputado_id, nombre, primer_voto, ultimo_voto, salida_camara, reemplazante, salio, servicio_inicio,
             servicio_fin, fuente_servicio, fraccion_mandato, cuenta)], file.path(o, "servicio_diputados.csv"))

W <- dcast(S, diputado_id ~ periodo, value.var = "cuenta", fill = 0L)
H <- S[k >= 2, .(diputado_id, nombre, periodo, k, servicio_inicio, servicio_fin, fraccion_mandato, cuenta)]
H <- merge(H, W, by = "diputado_id")
# períodos consecutivos ejercidos (más de la mitad), contando el actual; basta llegar a 3
H[, periodos_consecutivos := fcase(
  periodo == "2014_18", cuenta * (1L + `2010_14` * (1L + `2006_10`)),
  periodo == "2018_22", cuenta * (1L + `2014_18` * (1L + `2010_14`)),
  periodo == "2022_26", cuenta * (1L + `2018_22` * (1L + `2014_18`)))]
H[, ley_aplica := as.integer(periodo != "2014_18")]
H[, habria_estado_afectado := as.integer(periodos_consecutivos >= 3)]
H[, no_puede_repostular := as.integer(ley_aplica == 1L & periodos_consecutivos >= 3)]
H <- merge(H, P[, .(periodo, eleccion_siguiente)], by = "periodo")
H[, `:=`(fecha_ley = ley_pub, horizonte_corto_desde = fifelse(no_puede_repostular == 1L, pmax(ley_pub, servicio_inicio), as.Date(NA)))]
setnames(H, c("2006_10", "2010_14", "2014_18", "2018_22", "2022_26"), paste0("cuenta_", c("2006_10", "2010_14", "2014_18", "2018_22", "2022_26")))

orden <- c("diputado_id", "nombre", "periodo", "servicio_inicio", "servicio_fin", "fraccion_mandato", "cuenta_2006_10", "cuenta_2010_14",
           "cuenta_2014_18", "cuenta_2018_22", "cuenta_2022_26", "periodos_consecutivos", "ley_aplica", "habria_estado_afectado",
           "no_puede_repostular", "eleccion_siguiente", "fecha_ley", "horizonte_corto_desde")
setorder(H, periodo, -no_puede_repostular, nombre)
fwrite(H[, ..orden], file.path(out, "horizonte_ley21238.csv"))
cat("Diputado-período:", nrow(H), "\n"); print(H[, .(diputados = .N, con_3_o_mas_consecutivos = sum(habria_estado_afectado),
                                                      no_puede_repostular = sum(no_puede_repostular)), keyby = periodo])
cat("\nAfectados (no pueden repostular):\n"); print(H[no_puede_repostular == 1L, .(periodo, diputado_id, nombre, fraccion_mandato, periodos_consecutivos)])
cat("\nFracciones cercanas a la mitad (0,4-0,6), para revisar:\n"); print(S[fraccion_mandato > 0.4 & fraccion_mandato < 0.6, .(periodo, diputado_id, nombre, servicio_inicio, servicio_fin, fraccion_mandato)])
