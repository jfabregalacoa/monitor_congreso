# Uso: Rscript camara-cl-api/src/urgencias/04_validar_votos.R <raiz_del_repo> <carpeta_intermedios> <carpeta_salida>
#
# Paso 4 (control). Compara, celda por celda, el voto de las matrices de Harvard Dataverse con el detalle de la Cámara
# (retornarVotacionDetalle) en todas las votaciones descargadas: 2014-18 completo, las 733 nuevas de 2022-26 y la muestra de
# 300 votaciones (75 de 2018-22, 75 de 2022-26 histórica y 50 de cada período 2002-06, 2006-10 y 2010-14). Sirve para decidir cómo homologar NA, "No Vota" y celdas vacías.
# Salidas en <carpeta_salida>/controles/: validacion_votos_tabla.csv (cruce por grupo) y validacion_votos_resumen.csv
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); repo <- a[1]; o <- a[2]; out <- a[3]
ctl <- file.path(out, "controles"); dir.create(ctl, showWarnings = FALSE, recursive = TRUE)
rc <- function(f) file.path(repo, "Harvard Dataverse", "Roll calls", f)

largo <- function(f, ids) {
  m <- fread(rc(f), colClasses = "character")
  keep <- intersect(names(m), ids)
  l <- melt(m[, c("DiputadoId", keep), with = FALSE], id.vars = "DiputadoId", variable.name = "vote_id",
            value.name = "voto_matriz", variable.factor = FALSE)
  l[, voto_matriz := fifelse(is.na(voto_matriz), "<NA>", fifelse(voto_matriz == "", "<vacío>", voto_matriz))]
  l
}
ids_1418 <- readLines(file.path(o, "ids_2014_18.txt")); ids_new <- readLines(file.path(o, "new_vote_ids.txt"))
ids_m <- readLines(file.path(o, "muestra_validacion_ids.txt"))
M <- rbind(largo("matriz__periodo_2002_06.csv", ids_m)[, grupo := "2002-06 (muestra)"],
           largo("matriz__periodo_2006_10.csv", ids_m)[, grupo := "2006-10 (muestra)"],
           largo("matriz__periodo_2010_14.csv", ids_m)[, grupo := "2010-14 (muestra)"],
           largo("matriz__periodo_2014_18.csv", ids_1418)[, grupo := "2014-18 (todas)"],
           largo("matriz__periodo_2018_22.csv", ids_m)[, grupo := "2018-22 (muestra)"],
           largo("matriz__periodo_2022_26_extended.csv", setdiff(ids_m, ids_new))[, grupo := "2022-26 histórica (muestra)"],
           largo("matriz__periodo_2022_26_extended.csv", ids_new)[, grupo := "2022-26 nuevas 733 (todas)"])
setnames(M, "DiputadoId", "diputado_id")
D <- fread(file.path(o, "votos_detalle_camara.csv.gz"), colClasses = "character")
D <- unique(D[, .(vote_id, diputado_id, voto_camara = opcion)])
cat("Votos individuales de la Cámara:", nrow(D), "| opciones:\n"); print(D[, .N, by = voto_camara])
J <- merge(M, D, by = c("vote_id", "diputado_id"), all.x = TRUE)
J[is.na(voto_camara), voto_camara := "<no figura>"]
fuera <- D[vote_id %in% M$vote_id][!M, on = .(vote_id, diputado_id)]
cat("Votos de la Cámara de diputados que no son filas de la matriz:", nrow(fuera), "\n")

tab <- J[, .N, by = .(grupo, voto_matriz, voto_camara)][order(grupo, -N)]
fwrite(tab, file.path(ctl, "validacion_votos_tabla.csv"))
for (g in unique(J$grupo)) {
  cat("\n=====", g, "| votaciones:", uniqueN(J[grupo == g, vote_id]), "| celdas:", J[grupo == g, .N], "\n")
  print(dcast(J[grupo == g], voto_matriz ~ voto_camara, fun.aggregate = length, value.var = "vote_id"))
}
sust <- c("Afirmativo", "En Contra", "Abstención", "Dispensado")
J[, tipo_celda := fcase(voto_matriz %in% sust & voto_camara %in% sust & voto_matriz == voto_camara, "coincide (voto)",
                        voto_matriz %in% sust & voto_camara %in% sust, "voto distinto",
                        voto_matriz %in% sust, "matriz con voto, Cámara sin voto",
                        voto_camara %in% sust, "Cámara con voto, matriz sin voto",
                        default = "ambos sin voto")]
res <- J[, .N, by = .(grupo, tipo_celda)][, pct := round(100 * N / sum(N), 3), by = grupo][order(grupo, tipo_celda)]
fwrite(rbind(res, data.table(grupo = "todas", tipo_celda = "votos Cámara fuera de la matriz", N = nrow(fuera), pct = NA)),
       file.path(ctl, "validacion_votos_resumen.csv"))
cat("\n"); print(res)
dis <- J[tipo_celda != "coincide (voto)" & tipo_celda != "ambos sin voto"]
cat("\nCeldas con discrepancia sustantiva:", nrow(dis), "de", J[voto_matriz %in% sust | voto_camara %in% sust, .N],
    "celdas con algún voto (", round(100 * nrow(dis) / J[voto_matriz %in% sust | voto_camara %in% sust, .N], 3), "%)\n")
if (nrow(dis)) print(dis[, .N, by = .(grupo, vote_id)][order(-N)][1:min(.N, 15)])
