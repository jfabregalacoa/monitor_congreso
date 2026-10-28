# Uso: Rscript camara-cl-api/src/urgencias/08_controles.R <carpeta_salida>
#
# Paso 8. Controles de calidad. Escribe tablas en <carpeta_salida>/controles/ y las imprime:
#   control_faltantes_por_periodo.csv sin boletín, sin fecha, sin proyecto en el Senado, sin urgencias en el historial
#   control_urg_nivel_periodo_iniciativa.csv, control_variacion_por_periodo.csv
#   control_panel_filas.csv, control_ausencias_largas.csv, control_oficialismo.csv, control_cambios_militancia.csv
suppressPackageStartupMessages(library(data.table))
out <- commandArgs(TRUE)[1]; ctl <- file.path(out, "controles"); dir.create(ctl, showWarnings = FALSE)
V <- fread(file.path(out, "votaciones_urgencia_2002_2026.csv"), colClasses = "character")
V[, `:=`(fecha_d = as.Date(fecha_d), lvl_A = as.integer(lvl_A), lvl_B = as.integer(lvl_B))]
nm <- c("0 sin urgencia", "1 simple", "2 suma", "3 disc. inmediata")

# 1) faltantes
f <- V[, .(votaciones = .N, sin_boletin = sum(sin_boletin == "1"), sin_fecha = sum(sin_fecha == "1"),
           sin_proyecto_senado = sum(sin_proyecto_senado == "1"), proyecto_sin_urgencias_en_historial = sum(sin_urgencias_en_historial == "1"),
           sin_tipo_votacion = sum(tipo_votacion == ""), sin_iniciativa = sum(iniciativa == "")), keyby = periodo]
fwrite(f, file.path(ctl, "control_faltantes_por_periodo.csv")); cat("=== 1) Faltantes por período ===\n"); print(f)

# 2) urg_nivel por período e iniciativa
u <- dcast(V[, .(periodo, iniciativa, urg = factor(lvl_A, 0:3, nm))], periodo + iniciativa ~ urg, fun.aggregate = length, value.var = "urg", drop = FALSE)
u[, pct_con_urgencia := round(100 * (`1 simple` + `2 suma` + `3 disc. inmediata`) / (`0 sin urgencia` + `1 simple` + `2 suma` + `3 disc. inmediata`), 1)]
fwrite(u, file.path(ctl, "control_urg_nivel_periodo_iniciativa.csv")); cat("\n=== 2) urg_nivel (estricta) por período e iniciativa ===\n"); print(u)

# 3) proyectos con variación de urgencia entre sus votaciones (muestra efectiva)
b <- V[, .(n_votos = .N, nA = uniqueN(lvl_A), nB = uniqueN(lvl_B), iniciativa = iniciativa[1]), by = .(periodo, boletin_numero)]
w <- b[n_votos >= 2, .(proyectos_2_o_mas_votos = .N, con_variacion_estricta = sum(nA >= 2), con_variacion_laxa = sum(nB >= 2),
                       mensajes_2_o_mas_votos = sum(iniciativa == "Mensaje"), mensajes_con_variacion = sum(iniciativa == "Mensaje" & nA >= 2),
                       votaciones_en_proyectos_con_variacion = sum(n_votos[nA >= 2])), keyby = periodo]
bt <- V[, .(n_votos = .N, nA = uniqueN(lvl_A), nB = uniqueN(lvl_B), iniciativa = iniciativa[1]), by = boletin_numero]
w <- rbind(w, data.table(periodo = "2002-2026 (proyecto entre períodos)", proyectos_2_o_mas_votos = bt[n_votos >= 2, .N],
                         con_variacion_estricta = bt[n_votos >= 2 & nA >= 2, .N], con_variacion_laxa = bt[n_votos >= 2 & nB >= 2, .N],
                         mensajes_2_o_mas_votos = bt[n_votos >= 2 & iniciativa == "Mensaje", .N],
                         mensajes_con_variacion = bt[n_votos >= 2 & nA >= 2 & iniciativa == "Mensaje", .N],
                         votaciones_en_proyectos_con_variacion = bt[nA >= 2, sum(n_votos)]))
fwrite(w, file.path(ctl, "control_variacion_por_periodo.csv")); cat("\n=== 3) Proyectos con variación de urgencia, por período ===\n"); print(w)

# 4) filas del panel
panel <- file.path(out, c("panel_votos_2002_2014.csv.gz", "panel_votos_2014_2026.csv.gz"))
leer_panel <- function(cols) rbindlist(lapply(panel, fread, select = cols, colClasses = "character"))
X <- leer_panel(c("vote_id", "diputado_id", "periodo", "voto", "en_funciones", "fecha"))
pf <- X[, .(filas = .N, votaciones = uniqueN(vote_id), diputados = uniqueN(diputado_id)), keyby = periodo][, esperado := votaciones * diputados]
pf[, coincide := filas == esperado]
fwrite(pf, file.path(ctl, "control_panel_filas.csv"))
cat("\n=== 4) Filas del panel = diputados x votaciones de cada matriz ===\n"); print(pf)
cat("Duplicados (vote_id, diputado_id):", sum(duplicated(X[, .(vote_id, diputado_id)])), "\n")

# 5) ausencias largas en funciones (posibles suspensiones o licencias no registradas)
setorder(X, diputado_id, periodo, fecha, vote_id)
X[, aus := voto == "Ausente o no vota"]
X[, racha := rleid(aus), by = .(diputado_id, periodo)]
r <- X[aus == TRUE, .(votaciones_seguidas = .N, desde = min(fecha), hasta = max(fecha)), by = .(diputado_id, periodo, racha)][votaciones_seguidas >= 100]
P <- leer_panel(c("diputado_id", "nombre"))[, .SD[1], by = diputado_id]
r <- merge(r, P, by = "diputado_id")[order(-votaciones_seguidas)][, racha := NULL]
fwrite(r, file.path(ctl, "control_ausencias_largas.csv"))
cat("\n=== 5) Rachas de 100 o más votaciones seguidas 'Ausente o no vota' estando en funciones ===\n"); print(r)

# 6) oficialismo: distribución y motivos de NA (celdas en funciones)
O <- leer_panel(c("periodo", "en_funciones", "partido", "oficialista", "oficialista_dudoso", "partido_control", "fecha"))[en_funciones == "1"]
O[, motivo := fcase(oficialista != "", fifelse(oficialista_dudoso == "1", paste0("codificado (", oficialista, ") pero dudoso"), paste0("codificado (", oficialista, ")")),
                    partido == "IND", "NA: independiente", partido_control != "", paste0("NA: ", partido_control),
                    partido == "FRVS", "NA: FRVS desde el 20-08-2025", default = "NA: otro")]
ofi <- O[, .N, by = .(periodo, motivo)][, pct := round(100 * N / sum(N), 1), by = periodo][order(periodo, motivo)]
fwrite(ofi, file.path(ctl, "control_oficialismo.csv"))
cat("\n=== 6) Oficialismo en celdas en funciones (motivo) ===\n"); print(ofi)

# 7) cambios de militancia con fecha
M <- fread(file.path(out, "militancias_2002_2026.csv"), colClasses = "character")
cm <- M[cambio_en_periodo == "1", .(partidos = paste(unique(partido_alias), collapse = " -> ")), by = .(periodo, diputado_id)]
cm[, a_independiente := grepl("IND", partidos)]
cmr <- cm[, .(diputados_con_cambio_fechado = .N, de_ellos_pasan_por_IND = sum(a_independiente)), keyby = periodo]
lf <- fread(file.path(ctl, "militancia_vs_long_format.csv"), colClasses = "character")
cmr <- merge(cmr, lf[, .(sin_militancia_fechada = sum(coincide == "sin militancia fechada"), long_format_distinto = sum(coincide == "distinto"),
                         long_format_agrega = sum(coincide == "long_format agrega partidos")), keyby = periodo], by = "periodo")
fwrite(cmr, file.path(ctl, "control_cambios_militancia.csv"))
cat("\n=== 7) Cambios de militancia dentro del período (con fecha, de la Cámara) ===\n"); print(cmr)
