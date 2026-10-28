# Uso: Rscript camara-cl-api/src/urgencias/03_urgencia_en_la_votacion.R <carpeta_intermedios> <carpeta_salida>
#
# Paso 3. Calcula la urgencia VIGENTE EN LA CÁMARA el día de cada votación (2014-2026).
# Port de 03_urgencia_en_la_votacion.R (versión del curso, 20-10-2026): misma regla y mismos nombres de columnas
# (lvl_A, lvl_B, snapshot, ...), más columnas nuevas al final.
#
# Regla (Ley 18.918, art. 27; Constitución, art. 74): una urgencia ingresada a la Cámara de Diputados rige desde su fecha de
# ingreso hasta su retiro o, si no hay retiro, hasta el plazo nominal: simple 30 días, suma 15, discusión inmediata 6.
#   lvl_A / urg_nivel        (estricta): tope en el plazo nominal, aunque el retiro llegue después.
#   lvl_B / urg_nivel_laxa   (laxa): hasta el retiro explícito, aunque pase el plazo.
# Niveles: 0 = sin urgencia, 1 = simple, 2 = suma, 3 = discusión inmediata.
# lvl_A y lvl_B valen 0 cuando falta información (como en la versión del curso); urg_nivel y urg_nivel_laxa valen NA si la
# votación no tiene fecha, boletín o proyecto en el Senado.
# Salidas en <carpeta_salida>: votaciones_urgencia_2014_2026.csv, urgency_spells.csv
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); o <- a[1]; out <- a[2]
lv <- c("Sin urgencia" = 0L, "Simple" = 1L, "Suma" = 2L, "Discusión inmediata" = 3L)
dl <- c("Simple" = 30L, "Suma" = 15L, "Discusión inmediata" = 6L)
nm <- c("Sin urgencia", "Simple", "Suma", "Disc. inmediata")

# ---------------------------------------------------------------- votaciones
v1 <- fread(file.path(o, "votes_details_2018_26.csv"), colClasses = "character")
v1 <- v1[, .(vote_id, periodo, fecha, boletin_numero, snapshot = desc_urgencia_actual, quorum, resultado, tipo,
             fuente = "detalle (rama API)")]
v2 <- fread(file.path(o, "votes_new.csv"), colClasses = "character")
v2 <- v2[, .(vote_id, periodo = "2022_26", fecha, boletin_numero, snapshot = NA_character_, quorum, resultado, tipo,
             fuente = "nueva (sin detalle)")]
v3 <- fread(file.path(o, "votes_2014_18.csv"), colClasses = "character")
v3 <- v3[, .(vote_id, periodo = "2014_18", fecha, boletin_numero, snapshot = NA_character_, quorum, resultado, tipo,
             fuente = "Cámara 2014-18")]
V <- rbind(v1, v2, v3)
vm <- fread(file.path(o, "votaciones_matrices.csv"), colClasses = "character")
falt <- vm[!vote_id %in% V$vote_id]
if (nrow(falt)) {
  cat("AVISO: votaciones de las matrices sin metadatos (fallo de descarga):", nrow(falt), "\n")
  V <- rbind(V, falt[, .(vote_id, periodo, fecha = NA_character_, boletin_numero = NA_character_, snapshot = NA_character_,
                         quorum = NA_character_, resultado = NA_character_, tipo = NA_character_, fuente = "sin metadatos")])
}
stopifnot(!anyDuplicated(V$vote_id), setequal(V$vote_id, vm$vote_id))
V[, fecha_d := as.Date(substr(fecha, 1, 10))]
V[is.na(boletin_numero), boletin_numero := ""]

pr <- fread(file.path(o, "senado_proyectos.csv"), colClasses = "character")
pr[, en_senado := boletin_senado != "" | iniciativa != "" | etapa != ""]
V <- merge(V, pr[, .(boletin_numero, iniciativa, etapa_hoy = etapa, n_urg_proyecto = as.integer(n_urgencias), en_senado)],
           by = "boletin_numero", all.x = TRUE)
V[is.na(en_senado), en_senado := FALSE]

# ---------------------------------------------------------------- historial de urgencias
sp_all <- fread(file.path(o, "urgency_spells.csv"), colClasses = "character")
sp_all[, `:=`(fi = as.Date(f_ingreso), fr = as.Date(f_retiro))]
sp_all[, retiro_antes_ingreso := as.integer(!is.na(fr) & !is.na(fi) & fr < fi)]
sp_all[, usada_en_camara := as.integer(camara_ingreso == "C. Diputados" & tipo %in% names(dl) & f_ingreso != "")]
sp_all[, plazo_dias := dl[tipo]]
sp_all[, fecha_vencimiento_nominal := fi + plazo_dias]
sp_all[, fr_ok := fifelse(retiro_antes_ingreso == 1L, as.Date(NA), fr)]
sp_all[, `:=`(fin_estricto = pmin(fecha_vencimiento_nominal, fifelse(is.na(fr_ok), fecha_vencimiento_nominal, fr_ok)),
              fin_laxo = fifelse(is.na(fr_ok), fecha_vencimiento_nominal, fr_ok))]
sp <- sp_all[usada_en_camara == 1L, .(boletin_numero, tipo, nivel = lv[tipo], fi, venc = fecha_vencimiento_nominal,
                                      endA = fin_estricto, endB = fin_laxo, f_ingreso_corregida = as.integer(f_ingreso_corregida),
                                      retiro_antes_ingreso)]
cat("Urgencias del Senado:", nrow(sp_all), "en", uniqueN(sp_all$boletin_numero), "proyectos | ingresadas a la Cámara y usadas:",
    nrow(sp), "en", uniqueN(sp$boletin_numero), "proyectos | con retiro anterior al ingreso:", sum(sp_all$retiro_antes_ingreso),
    "| con año corregido:", sum(sp_all$f_ingreso_corregida == "1"), "(ingreso),", sum(sp_all$f_retiro_corregida == "1"), "(retiro)\n")

# ---------------------------------------------------------------- urgencia vigente al votar
x <- sp[V[!is.na(fecha_d) & boletin_numero != "", .(vote_id, boletin_numero, fecha_d)],
        on = .(boletin_numero, fi <= fecha_d), allow.cartesian = TRUE, nomatch = 0L,
        .(vote_id = i.vote_id, fecha_d = i.fecha_d, tipo, nivel, fi = x.fi, venc, endA, endB, f_ingreso_corregida, retiro_antes_ingreso)]
xa <- x[endA >= fecha_d][order(vote_id, -nivel, -as.numeric(fi))]
xb <- x[endB >= fecha_d][order(vote_id, -nivel, -as.numeric(fi))]
A <- xa[, .(lvl_A = nivel[1], urg_tipo = tipo[1], dias_desde_ingreso_urgencia = as.integer(fecha_d[1] - fi[1]),
            dias_al_vencimiento = as.integer(venc[1] - fecha_d[1]), n_urg_vigentes = .N,
            urg_ingreso_corregido = f_ingreso_corregida[1]), by = vote_id]
B <- xb[, .(lvl_B = nivel[1], urg_tipo_laxa = tipo[1]), by = vote_id]
V <- merge(V, A, by = "vote_id", all.x = TRUE); V <- merge(V, B, by = "vote_id", all.x = TRUE)
V[is.na(lvl_A), `:=`(lvl_A = 0L, urg_tipo = "Sin urgencia", n_urg_vigentes = 0L)]
V[is.na(lvl_B), `:=`(lvl_B = 0L, urg_tipo_laxa = "Sin urgencia")]
V[is.na(urg_ingreso_corregido), urg_ingreso_corregido := 0L]
V[, urg_retiro_invalido_en_proyecto := as.integer(boletin_numero %in% sp_all[retiro_antes_ingreso == 1L, boletin_numero])]
V[, snap := lv[snapshot]]
V[, `:=`(estA = factor(lvl_A, 0:3, nm), estB = factor(lvl_B, 0:3, nm))]

# ---------------------------------------------------------------- controles y versión con NA
V[, `:=`(sin_fecha = as.integer(is.na(fecha_d)), sin_boletin = as.integer(boletin_numero == ""),
         sin_proyecto_senado = as.integer(boletin_numero != "" & !en_senado),
         sin_urgencias_en_historial = as.integer(en_senado & n_urg_proyecto %in% 0L))]
V[, `:=`(urg_nivel = fifelse(sin_fecha + sin_boletin + sin_proyecto_senado > 0, NA_integer_, lvl_A),
         urg_nivel_laxa = fifelse(sin_fecha + sin_boletin + sin_proyecto_senado > 0, NA_integer_, lvl_B))]

# ---------------------------------------------------------------- datos de la Cámara por proyecto (tipo de votación, trámite)
vp <- fread(file.path(o, "votaciones_proyecto_camara.csv"), colClasses = "character")
vp[, num_consultado := sub("-.*", "", boletin_consultado)]
vp <- merge(vp, V[, .(vote_id, boletin_numero)], by = "vote_id")
vp[, prioridad := as.integer(num_consultado != boletin_numero)]
setorder(vp, vote_id, prioridad)
vp[, n_boletines_camara := .N, by = vote_id]
vp1 <- vp[, .SD[1], by = vote_id]
vp1[, tipo_votacion_3 := fcase(tipo_votacion %in% c("General", "General y Particular"), "general",
                               tipo_votacion == "Particular", "particular", tipo_votacion != "", "otro", default = NA_character_)]
V <- merge(V, vp1[, .(vote_id, tipo_votacion, tipo_votacion_3, articulo, tramite_constitucional, tramite_reglamentario,
                      total_si, total_no, total_abst, total_disp, fecha_camara_proyecto = fecha, n_boletines_camara)],
           by = "vote_id", all.x = TRUE)
pc <- fread(file.path(o, "proyectos_camara.csv"), colClasses = "character")
pc[, boletin_numero := sub("-.*", "", boletin_consultado)]
V <- merge(V, unique(pc[, .(boletin_numero, iniciativa_camara = tipo_iniciativa, nombre_camara = nombre)], by = "boletin_numero"),
           by = "boletin_numero", all.x = TRUE)
# Ley de Presupuestos: no se rige por las urgencias (Ley 18.918, art. 26, inciso tercero); se marca para poder excluirla
V <- merge(V, pr[, .(boletin_numero, titulo_senado = titulo)], by = "boletin_numero", all.x = TRUE)
V[, ley_presupuestos := as.integer(grepl("presupuestos? del sector p[uú]blico", tolower(paste(nombre_camara, titulo_senado))))]
V[, iniciativa_fuente := fifelse(!is.na(iniciativa) & iniciativa != "", "Senado",
                                 fifelse(!is.na(iniciativa_camara) & iniciativa_camara != "", "Cámara", NA_character_))]
V[iniciativa_fuente == "Cámara", iniciativa := iniciativa_camara]
V[, fecha_difiere_camara := as.integer(!is.na(fecha_camara_proyecto) & fecha_camara_proyecto != fecha)]

orden <- c("vote_id", "boletin_numero", "periodo", "fecha", "snapshot", "quorum", "resultado", "fuente", "fecha_d", "iniciativa",
           "etapa_hoy", "n_urg_proyecto", "lvl_A", "lvl_B", "snap", "estA", "estB",
           "tipo", "ley_presupuestos", "urg_nivel", "urg_nivel_laxa", "urg_tipo", "urg_tipo_laxa", "dias_desde_ingreso_urgencia", "dias_al_vencimiento",
           "n_urg_vigentes", "iniciativa_fuente", "iniciativa_camara", "tipo_votacion", "tipo_votacion_3", "articulo",
           "tramite_constitucional", "tramite_reglamentario", "total_si", "total_no", "total_abst", "total_disp",
           "sin_fecha", "sin_boletin", "sin_proyecto_senado", "sin_urgencias_en_historial", "urg_ingreso_corregido",
           "urg_retiro_invalido_en_proyecto", "fecha_difiere_camara", "n_boletines_camara")
setcolorder(V, orden); setorder(V, fecha_d, vote_id)
dir.create(out, showWarnings = FALSE, recursive = TRUE)
fwrite(V[, ..orden], file.path(out, "votaciones_urgencia_2014_2026.csv"))
fwrite(sp_all[, .(boletin_numero, tipo, camara_ingreso, mensaje_ingreso, f_ingreso, f_ingreso_corregida, camara_retiro,
                  mensaje_retiro, f_retiro, f_retiro_corregida, raw_f_ingreso, raw_f_retiro, usada_en_camara,
                  retiro_antes_ingreso, plazo_dias, fecha_vencimiento_nominal, fin_estricto, fin_laxo)],
       file.path(out, "urgency_spells.csv"))

# ---------------------------------------------------------------- resumen por pantalla
cat("Votaciones:", nrow(V), "| sin fecha:", sum(V$sin_fecha), "| sin boletín:", sum(V$sin_boletin),
    "| sin proyecto en Senado:", sum(V$sin_proyecto_senado), "| proyecto sin urgencias en su historial:",
    sum(V$sin_urgencias_en_historial), "\n")
cat("\n% con urgencia vigente (≥1), por período:\n")
print(V[, .(n = .N, pct_estricta = round(100 * mean(lvl_A > 0), 1), pct_laxa = round(100 * mean(lvl_B > 0), 1),
            difieren_A_B = sum(lvl_A != lvl_B)), keyby = .(periodo, fuente)])
cat("\nVotaciones de la Ley de Presupuestos por período:\n"); print(V[, .(ley_presupuestos = sum(ley_presupuestos), proyectos = uniqueN(boletin_numero[ley_presupuestos == 1L]),
                                                                          con_urgencia = sum(ley_presupuestos == 1L & lvl_A > 0)), keyby = periodo])
cat("\nTipo de votación (Cámara) por período:\n"); print(dcast(V, periodo ~ tipo_votacion_3, fun.aggregate = length, value.var = "vote_id"))
by_bill <- V[, .(n_votos = .N, n_niveles = uniqueN(lvl_A), iniciativa = iniciativa[1]), by = .(periodo, boletin_numero)]
cat("\nProyectos con ≥2 votaciones y ≥2 niveles de urgencia (estricta), por período:\n")
print(by_bill[n_votos >= 2, .(proyectos = .N, con_variacion = sum(n_niveles >= 2),
                              mensajes_con_variacion = sum(n_niveles >= 2 & iniciativa %in% "Mensaje")), keyby = periodo])
