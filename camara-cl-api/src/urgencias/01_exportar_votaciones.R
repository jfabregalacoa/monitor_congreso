# Uso: Rscript camara-cl-api/src/urgencias/01_exportar_votaciones.R <raiz_del_repo> <carpeta_intermedios>
#
# Paso 1. Exporta los metadatos de votaciones que ya están en el repositorio y arma las listas de votaciones a descargar.
# Port de 01_exportar_votaciones.R (versión del curso, 20-10-2026), extendido a 2002-2018. Lee los archivos de esta rama
# (feature/urgencias-votaciones), sin git show:
#   camara-cl-api/outputs/details__periodo_{2002_06,2006_10,2010_14,2018_22,2022_26}.json  (desc_urgencia_actual = FOTO)
#   Harvard Dataverse/Roll calls/matriz__periodo_{2002_06,2006_10,2010_14,2014_18,2018_22,2022_26,2022_26_extended}.csv
# Salidas en <carpeta_intermedios>:
#   votes_details_2018_26.csv     metadatos 2018-26 de los JSON (columnas de la versión del curso + boletin_full, desc_boletin)
#   votes_details_2002_14.csv     metadatos 2002-14 de los JSON (mismas columnas)
#   votaciones_matrices.csv       una fila por votación de las matrices del panel (2002-06 a 2022-26 extendida)
#   ids_2014_18.txt               votaciones 2014-18 (sin JSON de detalle; se piden a la Cámara en el paso 2)
#   new_vote_ids.txt              votaciones de la matriz extendida 2022-26 que no están en la histórica (733)
#   muestra_validacion_ids.txt    votaciones para validar las categorías de voto de las matrices: 75 de 2018-22, 75 de la
#                                 parte histórica de 2022-26 y 50 de cada período 2002-06, 2006-10 y 2010-14 (300)
suppressPackageStartupMessages({library(jsonlite); library(data.table)})
args <- commandArgs(TRUE); repo <- args[1]; out <- args[2]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
rc <- function(f) file.path(repo, "Harvard Dataverse", "Roll calls", f)

leer_json <- function(periodos) {
  d <- rbindlist(lapply(periodos, function(p) {
    d <- as.data.table(fromJSON(file.path(repo, "camara-cl-api", "outputs", sprintf("details__periodo_%s.json", p))))
    d[, periodo := p]
  }), fill = TRUE)
  d[, desc_urgencia_actual := fifelse(is.na(desc_urgencia_actual), "", desc_urgencia_actual)]
  d[, vote_id := as.character(vote_id)]
  d
}
cols_det <- c("vote_id", "periodo", "fecha", "boletin_numero", "desc_urgencia_actual", "desc_iniciativa", "desc_etapa",
              "quorum", "resultado", "tipo", "materias", "desc_titulo", "boletin_full", "desc_boletin")
det <- leer_json(c("2018_22", "2022_26"))
fwrite(det[, ..cols_det], file.path(out, "votes_details_2018_26.csv"))
det_old <- leer_json(c("2002_06", "2006_10", "2010_14"))
fwrite(det_old[, ..cols_det], file.path(out, "votes_details_2002_14.csv"))

hdr <- function(f) names(fread(rc(f), nrows = 0, colClasses = "character"))[-(1:5)]
h0206 <- hdr("matriz__periodo_2002_06.csv"); h0610 <- hdr("matriz__periodo_2006_10.csv"); h1014 <- hdr("matriz__periodo_2010_14.csv")
h1418 <- hdr("matriz__periodo_2014_18.csv"); h1822 <- hdr("matriz__periodo_2018_22.csv")
h2226 <- hdr("matriz__periodo_2022_26.csv"); hext <- hdr("matriz__periodo_2022_26_extended.csv")
nuevos <- setdiff(hext, h2226)
stopifnot(all(h2226 %in% hext), !anyDuplicated(c(h0206, h0610, h1014, h1418, h1822, hext)))
writeLines(nuevos, file.path(out, "new_vote_ids.txt"))
writeLines(h1418, file.path(out, "ids_2014_18.txt"))

vm <- rbind(data.table(vote_id = h1418, periodo = "2014_18", matriz = "matriz__periodo_2014_18.csv", col = seq_along(h1418)),
            data.table(vote_id = h1822, periodo = "2018_22", matriz = "matriz__periodo_2018_22.csv", col = seq_along(h1822)),
            data.table(vote_id = hext,  periodo = "2022_26", matriz = "matriz__periodo_2022_26_extended.csv", col = seq_along(hext)),
            data.table(vote_id = h0206, periodo = "2002_06", matriz = "matriz__periodo_2002_06.csv", col = seq_along(h0206)),
            data.table(vote_id = h0610, periodo = "2006_10", matriz = "matriz__periodo_2006_10.csv", col = seq_along(h0610)),
            data.table(vote_id = h1014, periodo = "2010_14", matriz = "matriz__periodo_2010_14.csv", col = seq_along(h1014)))
vm[, origen_meta := fifelse(vote_id %in% c(det$vote_id, det_old$vote_id), "JSON rama API", "Cámara (paso 2)")]
vm[, en_matriz_2022_26_historica := periodo == "2022_26" & vote_id %in% h2226]
fwrite(vm, file.path(out, "votaciones_matrices.csv"))

# Muestra de validación (votaciones que no se piden igual a la Cámara). Las 150 primeras son las de 2018-26 y no cambian al
# agregar 2002-14, porque los sorteos nuevos van después con la misma semilla.
set.seed(20261028)
muestra <- c(sample(h1822, 75), sample(h2226, 75), sample(h0206, 50), sample(h0610, 50), sample(h1014, 50))
writeLines(muestra, file.path(out, "muestra_validacion_ids.txt"))

cat("JSON 2018-22:", det[periodo == "2018_22", .N], "| JSON 2022-26:", det[periodo == "2022_26", .N],
    "| JSON 2002-14:", nrow(det_old), "| boletines distintos en JSON:",
    uniqueN(c(det[boletin_numero != "", boletin_numero], det_old[boletin_numero != "", boletin_numero])), "\n")
cat("Matrices: 2002-06 =", length(h0206), "| 2006-10 =", length(h0610), "| 2010-14 =", length(h1014), "| 2014-18 =", length(h1418),
    "| 2018-22 =", length(h1822), "| 2022-26 extendida =", length(hext), "(histórica", length(h2226), "+ nuevas", length(nuevos),
    ") | total =", nrow(vm), "\n")
for (p in c("2002_06", "2006_10", "2010_14")) {
  h <- vm[periodo == p, vote_id]
  cat("JSON vs matriz", p, ": en JSON y no en matriz =", length(setdiff(det_old[periodo == p, vote_id], h)),
      "| en matriz y no en JSON =", length(setdiff(h, det_old[periodo == p, vote_id])), "\n")
}
cat("JSON vs matriz 2018-22: en JSON y no en matriz =", length(setdiff(det[periodo == "2018_22", vote_id], h1822)),
    "| en matriz y no en JSON =", length(setdiff(h1822, det[periodo == "2018_22", vote_id])), "\n")
cat("JSON vs matriz 2022-26 histórica: en JSON y no en matriz =", length(setdiff(det[periodo == "2022_26", vote_id], h2226)),
    "| en matriz y no en JSON =", length(setdiff(h2226, det[periodo == "2022_26", vote_id])), "\n")
cat("A pedir a la Cámara: 2014-18 =", length(h1418), "| nuevas 2022-26 =", length(nuevos), "| muestra =", length(muestra), "\n")
