# Uso: Rscript camara-cl-api/src/urgencias/05_partido_y_coalicion.R <raiz_del_repo> <carpeta_intermedios> <carpeta_salida>
#
# Paso 5 (T6). Militancia de cada diputado con fechas y pertenencia de su partido a la coalición de gobierno.
#   Militancia: servicio WSDiputado de la Cámara (militancias con FechaInicio y FechaTermino, la misma fuente que usó
#   B_Add_Variables.R para party_1..party_4). Se contrasta con party_1..party_4 de
#   Harvard Dataverse/Ideology estimates/ideologia_congreso_chile_2002_2026_long_format.csv.
#   Coalición: <carpeta_salida>/coalicion_gobierno.csv (tabla partido x gobierno con fechas y fuentes, editada a mano).
# Salidas en <carpeta_salida>: militancias_2002_2026.csv; controles/militancia_vs_long_format.csv,
#   controles/coalicion_cobertura.csv
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); repo <- a[1]; o <- a[2]; out <- a[3]
ctl <- file.path(out, "controles"); dir.create(ctl, showWarnings = FALSE, recursive = TRUE)
P <- data.table(periodo = c("2002_06", "2006_10", "2010_14", "2014_18", "2018_22", "2022_26"), Period = paste0("Periodo_", 1:6),
                gobierno = c("Lagos", "Bachelet I", "Piñera I", "Bachelet II", "Piñera II", "Boric"),
                p_ini = as.Date(c("2002-03-11", "2006-03-11", "2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11")),
                p_fin = as.Date(c("2006-03-10", "2010-03-10", "2014-03-10", "2018-03-10", "2022-03-10", "2026-03-10")),
                matriz = c("matriz__periodo_2002_06.csv", "matriz__periodo_2006_10.csv", "matriz__periodo_2010_14.csv",
                           "matriz__periodo_2014_18.csv", "matriz__periodo_2018_22.csv", "matriz__periodo_2022_26_extended.csv"))
filas <- rbindlist(lapply(seq_len(nrow(P)), function(i)
  fread(file.path(repo, "Harvard Dataverse", "Roll calls", P$matriz[i]), select = c("DiputadoId", "Nombre", "ApellidoPaterno", "ApellidoMaterno"),
        colClasses = "character")[, periodo := P$periodo[i]]))
setnames(filas, "DiputadoId", "diputado_id")

M <- fread(file.path(o, "militancias.csv"), colClasses = "character")
M[, `:=`(fi = as.Date(substr(fecha_inicio, 1, 10)), ft = as.Date(substr(fecha_termino, 1, 10)))]
M[, fecha_invertida := as.integer(!is.na(ft) & ft < fi)]
X <- merge(filas[, .(diputado_id, periodo)], M, by = "diputado_id", allow.cartesian = TRUE)
X <- merge(X, P[, .(periodo, p_ini, p_fin)], by = "periodo")
X <- X[pmin(fi, ft) <= p_fin & pmax(fi, ft) >= p_ini]
X[, `:=`(inicio_en_periodo = pmax(pmin(fi, ft), p_ini), fin_en_periodo = pmin(pmax(fi, ft), p_fin))]
setorder(X, periodo, diputado_id, inicio_en_periodo)
X[, n_partidos_periodo := uniqueN(partido_alias), by = .(periodo, diputado_id)]
X[, cambio_en_periodo := as.integer(n_partidos_periodo > 1 | any(inicio_en_periodo > p_ini) | any(fin_en_periodo < p_fin)),
  by = .(periodo, diputado_id)]
# huecos: días del período sin militancia registrada
cob <- X[, .(dias_cubiertos = sum(as.integer(fin_en_periodo - inicio_en_periodo) + 1L)), by = .(periodo, diputado_id)]
cob <- merge(filas[, .(diputado_id, periodo)], cob, by = c("diputado_id", "periodo"), all.x = TRUE)
cob <- merge(cob, P[, .(periodo, dias = as.integer(p_fin - p_ini) + 1L)], by = "periodo")
cob[is.na(dias_cubiertos), dias_cubiertos := 0L]
fwrite(X[, .(periodo, diputado_id, partido_alias, partido_nombre, fecha_inicio = fi, fecha_termino = ft, inicio_en_periodo,
             fin_en_periodo, n_partidos_periodo, cambio_en_periodo, fecha_invertida)], file.path(out, "militancias_2002_2026.csv"))

# contraste con party_1..party_4 (sin fechas)
L <- fread(file.path(repo, "Harvard Dataverse", "Ideology estimates", "ideologia_congreso_chile_2002_2026_long_format.csv"),
           colClasses = "character")[Period %in% P$Period]
L <- merge(L, P[, .(Period, periodo)], by = "Period")
L[, partidos_lf := apply(.SD, 1, function(r) paste(sort(unique(r[!is.na(r) & r != "NA" & r != ""])), collapse = "|")),
  .SDcols = paste0("party_", 1:4)]
D <- X[, .(partidos_fechados = paste(sort(unique(partido_alias)), collapse = "|")), by = .(periodo, diputado_id)]
C <- merge(filas[, .(diputado_id, periodo, nombre = paste(Nombre, ApellidoPaterno, ApellidoMaterno))], D, by = c("diputado_id", "periodo"), all.x = TRUE)
C <- merge(C, L[, .(diputado_id = Id, periodo, partidos_lf, quant_parties)], by = c("diputado_id", "periodo"), all.x = TRUE)
C[, coincide := fcase(is.na(partidos_fechados), "sin militancia fechada", partidos_fechados == partidos_lf, "igual",
                      mapply(function(a, b) all(strsplit(a, "\\|")[[1]] %in% strsplit(b, "\\|")[[1]]), partidos_fechados, partidos_lf),
                      "long_format agrega partidos", default = "distinto")]
C <- merge(C, cob[, .(diputado_id, periodo, dias_sin_militancia = dias - dias_cubiertos)], by = c("diputado_id", "periodo"))
fwrite(C, file.path(ctl, "militancia_vs_long_format.csv"))
cat("Diputado-período:", nrow(C), "| con cambio de militancia fechado dentro del período:",
    X[cambio_en_periodo == 1L, uniqueN(paste(periodo, diputado_id))], "\n")
print(C[, .N, by = .(periodo, coincide)][order(periodo, coincide)])
cat("Diputado-período con días sin militancia registrada:\n"); print(C[dias_sin_militancia > 0, .(n = .N, dias_mediana = median(dias_sin_militancia)), by = periodo])
cat("Militancias con fecha de término anterior a la de inicio:", M[fecha_invertida == 1L, .N], "\n")

# cobertura de la tabla de coalición
G <- fread(file.path(out, "coalicion_gobierno.csv"), colClasses = "character")
G[, `:=`(desde = as.Date(desde), hasta = as.Date(hasta))]
gp <- P[, .(periodo, gobierno)]
U <- unique(merge(X, gp, by = "periodo")[, .(periodo, gobierno, partido_alias, partido_nombre, diputados = uniqueN(diputado_id)),
                                          by = .(periodo, partido_alias)][, .(periodo, gobierno, partido_alias, partido_nombre, diputados)])
U[, en_tabla := mapply(function(g, p) nrow(G[gobierno == g & partido_alias == p]) > 0, gobierno, partido_alias)]
fwrite(U, file.path(ctl, "coalicion_cobertura.csv"))
cat("Partidos presentes sin fila en coalicion_gobierno.csv:", U[en_tabla == FALSE, .N], "\n")
if (U[en_tabla == FALSE, .N]) print(U[en_tabla == FALSE])
cat("Filas por gobierno y oficialista (\"\" = no codificado):\n"); print(G[, .N, by = .(gobierno, oficialista, dudoso)])
