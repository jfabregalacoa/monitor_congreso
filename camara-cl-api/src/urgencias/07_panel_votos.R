# Uso: Rscript camara-cl-api/src/urgencias/07_panel_votos.R <raiz_del_repo> <carpeta_intermedios> <carpeta_salida> <carpeta_cache>
#
# Paso 7 (T5). Base larga diputado x votación, 2014-2026: una fila por celda de las matrices
#   matriz__periodo_2014_18.csv, matriz__periodo_2018_22.csv y matriz__periodo_2022_26_extended.csv.
# Une: urgencia vigente (paso 3), militancia a la fecha y oficialismo (paso 5), horizonte Ley 21.238 (paso 6) e ideología.
# Ideología (por diputado y período, de Harvard Dataverse/Ideology estimates/ideologia_congreso_chile_2002_2026_long_format.csv):
#   ideol_dyn = dim_ideology (emIRT::dynIRT, publicada; usa la matriz 2022-26 histórica), ideol_wnom = wnom_ideology (W-NOMINATE),
#   ideol_bay = bay_ideology (pscl::ideal). La dinámica estimada con la matriz extendida (ideol_dyn_ext, commit 6c91e1a, archivo
#   despublicado) se escribe aparte en <carpeta_salida>/no_versionado/, que git ignora.
# Voto: voto_original es la celda de la matriz tal cual; voto homologa (ver REPORTE_URGENCIAS.md):
#   Afirmativo, En contra, Abstención, Dispensado; "Ausente o no vota" (en funciones y sin voto: NA antes del 09-04-2025,
#   "No Vota" desde esa fecha); "No en funciones" (fuera del período en el cargo). No hay pareos en las fuentes.
# Salidas: <carpeta_salida>/panel_votos_2014_2026.csv.gz, ideologia_diputado_periodo.csv (con intervalos) y
#   no_versionado/ideologia_dyn_ext_2014_2026.csv
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); repo <- a[1]; o <- a[2]; out <- a[3]; cache <- a[4]
P <- data.table(periodo = c("2014_18", "2018_22", "2022_26"), Period = paste0("Periodo_", 4:6), gobierno = c("Bachelet II", "Piñera II", "Boric"),
                matriz = c("matriz__periodo_2014_18.csv", "matriz__periodo_2018_22.csv", "matriz__periodo_2022_26_extended.csv"))

# ---------------------------------------------------------------- celdas de las matrices
X <- rbindlist(lapply(seq_len(nrow(P)), function(i) {
  m <- fread(file.path(repo, "Harvard Dataverse", "Roll calls", P$matriz[i]), colClasses = "character")[, -1]
  l <- melt(m, id.vars = c("DiputadoId", "Nombre", "ApellidoPaterno", "ApellidoMaterno"), variable.name = "vote_id",
            value.name = "voto_original", variable.factor = FALSE)
  l[, periodo := P$periodo[i]]
  cat(P$matriz[i], ":", nrow(m), "diputados x", ncol(m) - 4, "votaciones =", nrow(l), "celdas\n")
  l
}))
X[, nombre := paste(Nombre, ApellidoPaterno, ApellidoMaterno)][, c("Nombre", "ApellidoPaterno", "ApellidoMaterno") := NULL]
setnames(X, "DiputadoId", "diputado_id")
n_esperado <- nrow(X)

# ---------------------------------------------------------------- votación
V <- fread(file.path(out, "votaciones_urgencia_2014_2026.csv"), colClasses = "character")
V <- V[, .(vote_id, fecha = as.Date(fecha_d), boletin = boletin_numero, iniciativa, ley_presupuestos = as.integer(ley_presupuestos),
           tipo_votacion, tipo_votacion_3, quorum, resultado,
           urg_nivel = as.integer(urg_nivel), urg_nivel_laxa = as.integer(urg_nivel_laxa), urg_tipo,
           dias_desde_ingreso_urgencia = as.integer(dias_desde_ingreso_urgencia), dias_al_vencimiento = as.integer(dias_al_vencimiento))]
X <- merge(X, V, by = "vote_id", all.x = TRUE)
stopifnot(!anyNA(X$fecha))

# ---------------------------------------------------------------- en funciones (paso 6) y voto homologado
S <- fread(file.path(o, "servicio_diputados.csv"), colClasses = "character")[periodo %in% P$periodo]
S[, `:=`(servicio_inicio = as.Date(servicio_inicio), servicio_fin = as.Date(servicio_fin))]
X <- merge(X, S[, .(periodo, diputado_id, servicio_inicio, servicio_fin)], by = c("periodo", "diputado_id"), all.x = TRUE)
X[, en_funciones := as.integer(fecha >= servicio_inicio & fecha <= servicio_fin)]
# Suspensiones del cargo: con el desafuero firme el diputado "queda suspendido de su cargo" (Constitución, art. 61).
# Tabla editada a mano con fuentes (<carpeta_salida>/suspensiones_desafuero.csv). Se marca como suspendida la primera racha de
# 20 o más votaciones seguidas sin voto (en orden cronológico) que termina después de la resolución firme, desde esa fecha.
# (La suspensión rige cuando la resolución se comunica a la Cámara, lo que puede tardar semanas.)
con_voto <- c("Afirmativo", "En Contra", "Abstención", "Dispensado")
ts <- fread(file.path(out, "votaciones_urgencia_2014_2026.csv"), select = c("vote_id", "fecha"), colClasses = "character")
setnames(ts, "fecha", "ts")
X <- merge(X, ts, by = "vote_id", all.x = TRUE)
susp <- fread(file.path(out, "suspensiones_desafuero.csv"), colClasses = "character")[, fecha_resolucion_firme := as.Date(fecha_resolucion_firme)]
X[, suspendido := 0L]
res_susp <- rbindlist(lapply(seq_len(nrow(susp)), function(i) {
  s <- susp[i]
  d <- X[periodo == s$periodo & diputado_id == s$diputado_id, .(vote_id, ts, fecha, sin_voto = !voto_original %in% con_voto)][order(ts)]
  d[, racha := rleid(sin_voto)]
  r <- d[sin_voto == TRUE, .(n = .N, fin = max(fecha)), by = racha][fin >= s$fecha_resolucion_firme & n >= 20][1]
  ids <- d[racha == r$racha & fecha >= s$fecha_resolucion_firme, vote_id]
  X[periodo == s$periodo & diputado_id == s$diputado_id & vote_id %in% ids, `:=`(en_funciones = 0L, suspendido = 1L)]
  data.table(periodo = s$periodo, diputado_id = s$diputado_id, nombre = s$nombre, resolucion = s$fecha_resolucion_firme,
             desde = min(d[vote_id %in% ids, fecha]), hasta = max(d[vote_id %in% ids, fecha]), votaciones = length(ids))
}))
cat("Suspensiones aplicadas:\n"); print(res_susp)
X[, ts := NULL]
sust <- c("Afirmativo" = "Afirmativo", "En Contra" = "En contra", "Abstención" = "Abstención", "Dispensado" = "Dispensado")
X[, voto := fifelse(voto_original %in% names(sust), sust[voto_original],
                    fifelse(en_funciones == 1L, "Ausente o no vota", fifelse(suspendido == 1L, "No en funciones (suspendido)", "No en funciones")))]
X[, voto_si := fcase(voto == "Afirmativo", 1L, voto == "En contra", 0L, default = NA_integer_)]
X[, control_voto := fcase(voto_original %in% names(sust) & en_funciones == 0L, "voto fuera del período en funciones",
                          voto_original %in% "" & en_funciones == 1L, "celda vacía en funciones",
                          voto_original %in% "No Vota" & en_funciones == 0L & suspendido == 0L, "No Vota fuera de funciones", default = "")]

# ---------------------------------------------------------------- partido a la fecha y oficialismo (paso 5)
M <- fread(file.path(out, "militancias_2014_2026.csv"), colClasses = "character")
M[, `:=`(ini = as.Date(inicio_en_periodo), fin = as.Date(fin_en_periodo))]
K <- unique(X[, .(periodo, diputado_id, fecha)])
pm <- M[K, on = .(periodo, diputado_id, ini <= fecha, fin >= fecha), nomatch = NA,
        .(periodo, diputado_id, fecha = i.fecha, partido = partido_alias, partido_nombre, ini_mil = x.ini)]
# si hay más de un registro a la fecha se usa el de inicio más reciente; solo se marca si los partidos difieren
pm[, n_mil := uniqueN(partido[!is.na(partido)]), by = .(periodo, diputado_id, fecha)]
pm <- pm[order(periodo, diputado_id, fecha, -as.numeric(ini_mil))][, .SD[1], by = .(periodo, diputado_id, fecha)]
G <- fread(file.path(out, "coalicion_gobierno.csv"), colClasses = "character")
G[, `:=`(desde = as.Date(desde), hasta = as.Date(hasta))]
pm <- merge(pm, P[, .(periodo, gobierno)], by = "periodo")
oc <- G[pm, on = .(gobierno, partido_alias = partido, desde <= fecha, hasta >= fecha), nomatch = NA,
        .(periodo = i.periodo, diputado_id = i.diputado_id, fecha = i.fecha, oficialista = as.integer(x.oficialista),
          oficialista_dudoso = as.integer(x.dudoso), coalicion = x.coalicion)]
pm <- merge(pm, unique(oc, by = c("periodo", "diputado_id", "fecha")), by = c("periodo", "diputado_id", "fecha"), all.x = TRUE)
pm[, partido_control := fcase(n_mil == 0L, "sin militancia registrada a la fecha",
                              n_mil > 1L, "dos partidos a la fecha (se usa el registro más reciente)", default = "")]
pm[n_mil > 1L, oficialista_dudoso := 1L]
pm[is.na(partido) | n_mil == 0L, `:=`(oficialista = NA_integer_, oficialista_dudoso = 1L)]
X <- merge(X, pm[, .(periodo, diputado_id, fecha, partido, oficialista, oficialista_dudoso, coalicion, partido_control)],
           by = c("periodo", "diputado_id", "fecha"), all.x = TRUE)

# ---------------------------------------------------------------- horizonte (paso 6)
H <- fread(file.path(out, "horizonte_ley21238.csv"), colClasses = "character")
X <- merge(X, H[, .(periodo, diputado_id, no_puede_repostular = as.integer(no_puede_repostular),
                    horizonte_corto_desde = as.Date(horizonte_corto_desde))], by = c("periodo", "diputado_id"), all.x = TRUE)
X[, horizonte_corto := as.integer(no_puede_repostular == 1L & !is.na(horizonte_corto_desde) & fecha >= horizonte_corto_desde)]

# ---------------------------------------------------------------- ideología
L <- fread(file.path(repo, "Harvard Dataverse", "Ideology estimates", "ideologia_congreso_chile_2002_2026_long_format.csv"))
L <- merge(L[, .(diputado_id = as.character(Id), Period, ideol_dyn = dim_ideology, ideol_dyn_lo = dim_IC_low, ideol_dyn_hi = dim_IC_high,
                 ideol_wnom = wnom_ideology, ideol_bay = bay_ideology, ideol_bay_lo = bay_ic_low, ideol_bay_hi = bay_ic_high)],
           P[, .(Period, periodo)], by = "Period")[, Period := NULL]
# Equivalencia de Id: Pablo Prieto Lorca figura en la matriz 2018-22 (y en la Cámara) con DiputadoId 1064 y en long_format con Id 209
# (su Id de los años noventa). Solo afecta la unión con la ideología.
alias_id <- data.table(periodo = "2018_22", diputado_id = "1064", id_ideologia = "209")
L <- merge(L, alias_id, by.x = c("periodo", "diputado_id"), by.y = c("periodo", "id_ideologia"), all.x = TRUE, suffixes = c("", ".m"))
L[!is.na(diputado_id.m), diputado_id := diputado_id.m][, diputado_id.m := NULL]
num <- c("ideol_dyn", "ideol_dyn_lo", "ideol_dyn_hi", "ideol_wnom", "ideol_bay", "ideol_bay_lo", "ideol_bay_hi")
L[, (num) := lapply(.SD, round, 4), .SDcols = num]
L <- L[paste(periodo, diputado_id) %in% unique(X[, paste(periodo, diputado_id)])]
fwrite(L[order(periodo, diputado_id)], file.path(out, "ideologia_diputado_periodo.csv"))  # con intervalos; el panel lleva solo los puntos
X <- merge(X, L[, .(periodo, diputado_id, ideol_dyn, ideol_wnom, ideol_bay)], by = c("periodo", "diputado_id"), all.x = TRUE)
E <- fread(file.path(cache, "insumos_git", "ideologia_diputados_largo_emIRT_con_anclas_2022_26_extended.csv"))
E <- merge(E[, .(diputado_id = as.character(DiputadoId), Period = Periodo, ideol_dyn_ext = Ideologia, ideol_dyn_ext_lo = IC_low,
                 ideol_dyn_ext_hi = IC_high)], P[, .(Period, periodo)], by = "Period")[, Period := NULL]
E <- merge(E, alias_id, by.x = c("periodo", "diputado_id"), by.y = c("periodo", "id_ideologia"), all.x = TRUE, suffixes = c("", ".m"))
E[!is.na(diputado_id.m), diputado_id := diputado_id.m][, diputado_id.m := NULL]
E <- E[paste(periodo, diputado_id) %in% unique(X[, paste(periodo, diputado_id)])]
dir.create(file.path(out, "no_versionado"), showWarnings = FALSE)
fwrite(E, file.path(out, "no_versionado", "ideologia_dyn_ext_2014_2026.csv"))

# ---------------------------------------------------------------- escritura y controles
orden <- c("vote_id", "fecha", "periodo", "boletin", "iniciativa", "ley_presupuestos", "tipo_votacion", "tipo_votacion_3", "quorum", "resultado",
           "urg_nivel", "urg_nivel_laxa", "urg_tipo", "dias_desde_ingreso_urgencia", "dias_al_vencimiento",
           "diputado_id", "nombre", "voto_original", "voto", "voto_si", "en_funciones", "partido", "oficialista", "oficialista_dudoso",
           "coalicion", "no_puede_repostular", "horizonte_corto", "ideol_dyn", "ideol_wnom", "ideol_bay", "control_voto",
           "partido_control")
setcolorder(X, orden); setorder(X, fecha, vote_id, diputado_id)
stopifnot(nrow(X) == n_esperado, !anyDuplicated(X[, .(vote_id, diputado_id)]))
fwrite(X[, ..orden], file.path(out, "panel_votos_2014_2026.csv.gz"), compress = "gzip", compressLevel = 9)
cat("\nPanel:", nrow(X), "filas (esperadas", n_esperado, ") | duplicados (vote_id, diputado_id):", anyDuplicated(X[, .(vote_id, diputado_id)]),
    "| tamaño:", round(file.size(file.path(out, "panel_votos_2014_2026.csv.gz")) / 2^20, 1), "MB\n")
cat("\nVoto homologado por período:\n"); print(dcast(X, periodo ~ voto, fun.aggregate = length, value.var = "vote_id"))
cat("\nVoto original por período:\n"); print(dcast(X[, .(periodo, vo = fifelse(is.na(voto_original), "<NA>", fifelse(voto_original == "", "<vacío>", voto_original)))],
                                                   periodo ~ vo, fun.aggregate = length, value.var = "vo"))
cat("\nControles de voto:\n"); print(X[control_voto != "", .N, by = .(periodo, control_voto)])
cat("\nOficialismo (celdas en funciones):\n"); print(dcast(X[en_funciones == 1L], periodo ~ fifelse(is.na(oficialista), "NA", as.character(oficialista)),
                                                          fun.aggregate = length, value.var = "vote_id"))
cat("\nCeldas en funciones sin partido a la fecha:\n"); print(X[en_funciones == 1L & partido_control != "", .N, by = .(periodo, partido_control)])
cat("\nHorizonte corto (celdas):\n"); print(X[, .(no_puede_repostular = sum(no_puede_repostular, na.rm = TRUE), horizonte_corto = sum(horizonte_corto, na.rm = TRUE)), by = periodo])
cat("\nIdeología faltante (celdas en funciones):\n"); print(X[en_funciones == 1L, .(sin_dyn = sum(is.na(ideol_dyn)), sin_wnom = sum(is.na(ideol_wnom)), sin_bay = sum(is.na(ideol_bay))), by = periodo])
cat("ideol_dyn_ext (archivo no versionado):", nrow(E), "diputado-período; NA:", sum(is.na(E$ideol_dyn_ext)), "\n")
