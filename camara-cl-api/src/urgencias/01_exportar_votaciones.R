# Uso: Rscript camara-cl-api/src/urgencias/01_exportar_votaciones.R <raiz_del_repo> <carpeta_intermedios>
#
# Paso 1. Exporta los metadatos de votaciones que ya están en el repositorio y arma las listas de votaciones a descargar.
# Port de 01_exportar_votaciones.R (versión del curso, 20-10-2026), extendido a 2014-2018. Lee los archivos de esta rama
# (feature/urgencias-votaciones), sin git show:
#   camara-cl-api/outputs/details__periodo_2018_22.json y details__periodo_2022_26.json  (campo desc_urgencia_actual = FOTO)
#   Harvard Dataverse/Roll calls/matriz__periodo_2014_18.csv, _2018_22.csv, _2022_26.csv, _2022_26_extended.csv
# Salidas en <carpeta_intermedios>:
#   votes_details_2018_26.csv     metadatos 2018-26 de los JSON (columnas de la versión del curso + boletin_full, desc_boletin)
#   votaciones_matrices.csv       una fila por votación de las matrices del panel (2014-18, 2018-22, 2022-26 extendida)
#   ids_2014_18.txt               votaciones 2014-18 (sin JSON de detalle; se piden a la Cámara en el paso 2)
#   new_vote_ids.txt              votaciones de la matriz extendida 2022-26 que no están en la histórica (733)
#   muestra_validacion_ids.txt    150 votaciones 2018-26 (75 por período) para validar las categorías de voto de las matrices
suppressPackageStartupMessages({library(jsonlite); library(data.table)})
args <- commandArgs(TRUE); repo <- args[1]; out <- args[2]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
rc <- function(f) file.path(repo, "Harvard Dataverse", "Roll calls", f)

det <- rbindlist(lapply(c("2018_22", "2022_26"), function(p) {
  d <- as.data.table(fromJSON(file.path(repo, "camara-cl-api", "outputs", sprintf("details__periodo_%s.json", p))))
  d[, periodo := p]
}), fill = TRUE)
det[, desc_urgencia_actual := fifelse(is.na(desc_urgencia_actual), "", desc_urgencia_actual)]
det[, vote_id := as.character(vote_id)]
fwrite(det[, .(vote_id, periodo, fecha, boletin_numero, desc_urgencia_actual, desc_iniciativa, desc_etapa,
               quorum, resultado, tipo, materias, desc_titulo, boletin_full, desc_boletin)],
       file.path(out, "votes_details_2018_26.csv"))

hdr <- function(f) names(fread(rc(f), nrows = 0, colClasses = "character"))[-(1:5)]
h1418 <- hdr("matriz__periodo_2014_18.csv"); h1822 <- hdr("matriz__periodo_2018_22.csv")
h2226 <- hdr("matriz__periodo_2022_26.csv"); hext <- hdr("matriz__periodo_2022_26_extended.csv")
nuevos <- setdiff(hext, h2226)
stopifnot(all(h2226 %in% hext), !anyDuplicated(c(h1418, h1822, hext)))
writeLines(nuevos, file.path(out, "new_vote_ids.txt"))
writeLines(h1418, file.path(out, "ids_2014_18.txt"))

vm <- rbind(data.table(vote_id = h1418, periodo = "2014_18", matriz = "matriz__periodo_2014_18.csv", col = seq_along(h1418)),
            data.table(vote_id = h1822, periodo = "2018_22", matriz = "matriz__periodo_2018_22.csv", col = seq_along(h1822)),
            data.table(vote_id = hext,  periodo = "2022_26", matriz = "matriz__periodo_2022_26_extended.csv", col = seq_along(hext)))
vm[, origen_meta := fifelse(vote_id %in% det$vote_id, "JSON rama API", "Cámara (paso 2)")]
vm[, en_matriz_2022_26_historica := periodo == "2022_26" & vote_id %in% h2226]
fwrite(vm, file.path(out, "votaciones_matrices.csv"))

# Muestra de validación: 75 votaciones de 2018-22 y 75 de la parte histórica de 2022-26 (las que no se piden igual a la Cámara)
set.seed(20261028)
muestra <- c(sample(h1822, 75), sample(h2226, 75))
writeLines(muestra, file.path(out, "muestra_validacion_ids.txt"))

cat("JSON 2018-22:", det[periodo == "2018_22", .N], "| JSON 2022-26:", det[periodo == "2022_26", .N],
    "| boletines distintos en JSON:", uniqueN(det[boletin_numero != "", boletin_numero]), "\n")
cat("Matrices: 2014-18 =", length(h1418), "| 2018-22 =", length(h1822), "| 2022-26 extendida =", length(hext),
    "(histórica", length(h2226), "+ nuevas", length(nuevos), ") | total =", nrow(vm), "\n")
cat("JSON vs matriz 2018-22: en JSON y no en matriz =", length(setdiff(det[periodo == "2018_22", vote_id], h1822)),
    "| en matriz y no en JSON =", length(setdiff(h1822, det[periodo == "2018_22", vote_id])), "\n")
cat("JSON vs matriz 2022-26 histórica: en JSON y no en matriz =", length(setdiff(det[periodo == "2022_26", vote_id], h2226)),
    "| en matriz y no en JSON =", length(setdiff(h2226, det[periodo == "2022_26", vote_id])), "\n")
cat("A pedir a la Cámara: 2014-18 =", length(h1418), "| nuevas 2022-26 =", length(nuevos), "| muestra =", length(muestra), "\n")
