################################################################################
# Build the extended 2022-2026 roll-call matrix without altering the historical
# Harvard Dataverse replica. The historical matrix supplies every existing
# column and metadata value; the local matrix supplies only new vote columns.
################################################################################

rm(list = ls())

library(data.table)
library(here)

root <- here()
historical_path <- file.path(
  root, "Harvard Dataverse", "Roll calls", "matriz__periodo_2022_26.csv"
)
local_path <- file.path(root, "data", "matriz__periodo_2022_26.csv")
local_period_path <- file.path(root, "data", "Votaciones_periodo_2022_26.csv")
future_matrix_path <- file.path(root, "data", "matriz__periodo_2026_30.csv")
future_period_path <- file.path(root, "data", "Votaciones_periodo_2026_30.csv")
output_path <- file.path(
  root, "Harvard Dataverse", "Roll calls",
  "matriz__periodo_2022_26_extended.csv"
)

required <- c(
  historical_path, local_path, local_period_path,
  future_matrix_path, future_period_path
)
if (any(!file.exists(required))) {
  stop("Missing required input(s): ", paste(required[!file.exists(required)], collapse = ", "))
}

# Preserve empty historical cells as empty strings rather than normalizing them.
historical <- fread(historical_path, check.names = FALSE, na.strings = NULL)
local <- fread(local_path, check.names = FALSE, na.strings = NULL)
period_votes <- fread(local_period_path, check.names = FALSE, na.strings = NULL)
future_matrix <- fread(future_matrix_path, check.names = FALSE, na.strings = NULL)
future_votes <- fread(future_period_path, check.names = FALSE, na.strings = NULL)

metadata_n <- 5L
historical_vote_ids <- names(historical)[(metadata_n + 1L):ncol(historical)]
local_vote_ids <- names(local)[(metadata_n + 1L):ncol(local)]
future_vote_ids <- names(future_matrix)[(metadata_n + 1L):ncol(future_matrix)]
added_vote_ids <- setdiff(local_vote_ids, historical_vote_ids)

stopifnot(
  nrow(historical) == nrow(local),
  setequal(historical$DiputadoId, local$DiputadoId),
  all(historical_vote_ids %in% local_vote_ids),
  length(setdiff(historical_vote_ids, local_vote_ids)) == 0L,
  length(added_vote_ids) > 0L,
  length(intersect(local_vote_ids, future_vote_ids)) == 0L,
  length(intersect(as.character(period_votes$Id), as.character(future_votes$Id))) == 0L
)

period_dates <- as.Date(substr(as.character(period_votes$Fecha), 1L, 10L))
future_dates <- as.Date(substr(as.character(future_votes$Fecha), 1L, 10L))
stopifnot(
  all(period_dates >= as.Date("2022-03-11") & period_dates < as.Date("2026-03-11")),
  all(future_dates >= as.Date("2026-03-11"))
)

local <- local[match(historical$DiputadoId, local$DiputadoId)]
extended <- cbind(historical, local[, ..added_vote_ids])

# The local period table contains one historical vote without nominal detail;
# every matrix column must nevertheless belong to the 2022-2026 period table.
stopifnot(
  all(names(extended)[(metadata_n + 1L):ncol(extended)] %in% as.character(period_votes$Id)),
  length(intersect(names(extended), as.character(future_votes$Id))) == 0L,
  identical(extended[, seq_len(ncol(historical)), with = FALSE], historical)
)

fwrite(extended, output_path, na = "")
written <- fread(output_path, check.names = FALSE, na.strings = NULL)
stopifnot(
  identical(written[, seq_len(ncol(historical)), with = FALSE], historical),
  setequal(names(written)[(metadata_n + 1L):ncol(written)], local_vote_ids),
  identical(names(written)[(ncol(historical) + 1L):ncol(written)], added_vote_ids)
)

cat("Historical legislators:", nrow(historical), "\n")
cat("Extended legislators:", nrow(extended), "\n")
cat("Historical roll calls:", length(historical_vote_ids), "\n")
cat("Extended roll calls:", length(local_vote_ids), "\n")
cat("Added roll calls:", length(added_vote_ids), "\n")
cat("2026-2030 roll-call overlap:", length(intersect(local_vote_ids, future_vote_ids)), "\n")
cat("Wrote:", output_path, "\n")
