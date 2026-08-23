################################################################################
# Dynamic ideological estimation with anchors (dynIRT)
#
# This script extends the original dynamic estimation by allowing anchors
# (fixed legislators in specific periods) using informative priors on x.
#
# Three criteria were used to define anchors:
# 1. Select legislators present in as many periods as possible
# 2. Select legislators with few NAs
# 3. Select legislators with clear identification at the left or right
#    of the ideological spectrum
#
# The procedure is in selecciona_anclas.R
#
# Using this procedure Gastón Von Mühlenbrock appears as the best candidate.
# He was a legislator in 5 of the 6 periods with an overall vote rate of 93,4%
# His DiputadoId is 917
################################################################################

rm(list = ls())

library(data.table)
library(dplyr)
library(emIRT)
library(here)

set.seed(123) # Please do not change - for replication

aqui <- here()

# Optional environment variables allow an additional version to be estimated
# without overwriting the historical inputs or output. Defaults preserve the
# original workflow exactly.
roll_call_dir <- Sys.getenv(
  "ROLL_CALL_DIR",
  unset = file.path(aqui, "data")
)
roll_call_2022_26_file <- Sys.getenv(
  "ROLL_CALL_2022_26_FILE",
  unset = "matriz__periodo_2022_26.csv"
)
identification_output <- Sys.getenv(
  "IDENTIFICATION_OUTPUT",
  unset = file.path(aqui, "data", "Identificacion_parlamentarios.csv")
)
ideology_output <- Sys.getenv(
  "IDEOLOGY_OUTPUT",
  unset = file.path(
    aqui, "pape", "polarizacion", "data",
    "ideologia_diputados_largo_emIRT_con_anclas.csv"
  )
)

# Roll calls per period
archivos_periodo <- c(
  "matriz__periodo_2002_06.csv",
  "matriz__periodo_2006_10.csv",
  "matriz__periodo_2010_14.csv",
  "matriz__periodo_2014_18.csv",
  "matriz__periodo_2018_22.csv",
  roll_call_2022_26_file
)
archivos <- file.path(roll_call_dir, archivos_periodo)

if (any(!file.exists(archivos))) {
  stop("Missing roll-call input(s): ", paste(archivos[!file.exists(archivos)], collapse = ", "))
}

# Auxiliary ID table (same as original script)
id_data <- lapply(
  archivos,
  function(f) fread(f, select = c("DiputadoId", "Nombre", "ApellidoPaterno", "ApellidoMaterno"))
)

id_full <- rbindlist(id_data) %>%
  mutate(NombreId = paste(Nombre, ApellidoPaterno, ApellidoMaterno, sep = " ")) %>%
  select(DiputadoId, NombreId) %>%
  distinct()

dir.create(dirname(identification_output), recursive = TRUE, showWarnings = FALSE)
fwrite(id_full, file = identification_output)

# ------------------------------------------------------------------------------
# Dynamic estimate with anchors
# ------------------------------------------------------------------------------
anchor_id <- 917
anchor_value <- 3
anchor_sd <- 0.01

codifica_voto <- function(x) {
  x <- as.character(x)
  ifelse(x == "Afirmativo", 1,
         ifelse(x == "En Contra", -1, 0))
}

procesa_matriz <- function(file) {
  dt <- fread(file)
  if (names(dt)[1] == "") dt <- dt[, -1, with = FALSE]
  votos <- dt[, 6:ncol(dt), with = FALSE]
  votos <- as.data.frame(lapply(votos, codifica_voto))
  dt_votos <- cbind(DiputadoId = dt$DiputadoId, votos)
  rownames(dt_votos) <- dt$DiputadoId
  as.matrix(dt_votos[, -1])
}

matrices_voto <- lapply(archivos, procesa_matriz)

todos_diputados <- unique(unlist(lapply(archivos, function(f) fread(f, select = "DiputadoId")$DiputadoId)))

rellena_na <- function(matriz, ids_completo) {
  ids_actuales <- rownames(matriz)
  matriz_completo <- matrix(NA, nrow = length(ids_completo), ncol = ncol(matriz))
  rownames(matriz_completo) <- ids_completo
  colnames(matriz_completo) <- colnames(matriz)
  matriz_completo[match(ids_actuales, ids_completo), ] <- matriz
  matriz_completo
}

matrices_voto_full <- lapply(matrices_voto, rellena_na, ids_completo = todos_diputados)

rc <- do.call(cbind, matrices_voto_full)
num_items_per_period <- sapply(matrices_voto_full, ncol)
bill.session <- unlist(
  mapply(function(t, n) rep(t - 1, n), 1:length(num_items_per_period), num_items_per_period)
)

presencia <- sapply(matrices_voto_full, function(mat) rowSums(abs(mat) > 0, na.rm = TRUE) > 0)

if (!anchor_id %in% todos_diputados) {
  stop(paste("El DiputadoId ancla no existe en las matrices de voto:", anchor_id))
}

anchor_row <- match(anchor_id, todos_diputados)
periodos_ancla <- which(presencia[anchor_row, ])
anchors <- tibble::tibble(
  DiputadoId = anchor_id,
  Periodo = periodos_ancla,
  anchor_value = anchor_value,
  anchor_sd = anchor_sd
)

startlegis <- apply(presencia, 1, function(x) which(x)[1] - 1)
endlegis <- apply(presencia, 1, function(x) rev(which(x))[1] - 1)

N <- NROW(rc)
J <- NCOL(rc)
T <- 6

rc <- matrix(as.numeric(rc), nrow = nrow(rc), ncol = ncol(rc), dimnames = NULL)
startlegis <- matrix(as.integer(startlegis), nrow = length(startlegis), ncol = 1, dimnames = NULL)
endlegis <- matrix(as.integer(endlegis), nrow = length(endlegis), ncol = 1, dimnames = NULL)
bill.session <- matrix(as.integer(bill.session), nrow = length(bill.session), ncol = 1, dimnames = NULL)
T <- as.integer(T)

attributes(rc) <- attributes(matrix(0, nrow = nrow(rc), ncol = ncol(rc)))
attributes(startlegis) <- attributes(matrix(0, nrow = nrow(startlegis), ncol = 1))
attributes(endlegis) <- attributes(matrix(0, nrow = nrow(endlegis), ncol = 1))
attributes(bill.session) <- attributes(matrix(0, nrow = nrow(bill.session), ncol = 1))

.data <- list(
  rc = rc,
  startlegis = startlegis,
  endlegis = endlegis,
  bill.session = bill.session,
  T = T
)

if (length(periodos_ancla) == 0) {
  stop("No se encontraron períodos con votos para el DiputadoId ancla.")
}

if (any(!anchors$Periodo %in% seq_len(T))) {
  stop("La columna 'Periodo' debe estar entre 1 y 6.")
}

x_mu0 <- matrix(0, nrow = N, ncol = T)
x_sigma0 <- matrix(1, nrow = N, ncol = T)

for (i in seq_len(nrow(anchors))) {
  row_idx <- match(anchors$DiputadoId[i], todos_diputados)
  col_idx <- anchors$Periodo[i]
  x_mu0[row_idx, col_idx] <- anchors$anchor_value[i]
  x_sigma0[row_idx, col_idx] <- anchors$anchor_sd[i]
}

# Estimation
starts <- list(
  alpha = matrix(rnorm(J, mean = 0, sd = 0.1), ncol = 1),
  beta = matrix(rnorm(J, mean = 1, sd = 0.1), ncol = 1),
  x = matrix(0, nrow = N, ncol = T)
)

priors <- list(
  x.mu0 = x_mu0,
  x.sigma0 = x_sigma0,
  beta.mu = matrix(c(0, 1), nrow = 2),
  beta.sigma = diag(2),
  omega2 = matrix(1, nrow = N, ncol = 1)
)

control <- list(
  threads = 4,
  verbose = TRUE,
  thresh = 1e-6,
  maxit = 1000,
  checkfreq = 50
)

resultado <- dynIRT(
  .data = .data,
  .starts = starts,
  .priors = priors,
  .control = control
)

################################################################################
# Saving the results
################################################################################

x_mean <- resultado$means$x
x_var <- resultado$vars$x

x_mean[x_mean == 0.00000000] <- NA
x_var[x_var == 0.00000000] <- NA

x_sd <- sqrt(x_var)

x_low <- x_mean - 1.96 * x_sd
x_high <- x_mean + 1.96 * x_sd

periodos_invertir <- c(1, 2, 4, 6)
x_mean[, periodos_invertir] <- -1 * x_mean[, periodos_invertir]
x_low[, periodos_invertir] <- -1 * x_low[, periodos_invertir]
x_high[, periodos_invertir] <- -1 * x_high[, periodos_invertir]

periodos_nombres <- paste0("Periodo_", seq_len(ncol(x_mean)))
colnames(x_mean) <- periodos_nombres
colnames(x_low) <- periodos_nombres
colnames(x_high) <- periodos_nombres

library(tidyr)

id_full <- fread(identification_output)

stopifnot(length(todos_diputados) == nrow(x_mean))

df_ideologia <- as.data.frame(x_mean) %>%
  mutate(DiputadoId = todos_diputados)

df_low <- as.data.frame(x_low) %>%
  mutate(DiputadoId = todos_diputados)

df_high <- as.data.frame(x_high) %>%
  mutate(DiputadoId = todos_diputados)

# Remove estimates for legislator-periods with no observed votes
presencia_df <- as.data.frame(presencia)
colnames(presencia_df) <- paste0("Periodo_", seq_len(ncol(presencia_df)))
presencia_df <- presencia_df %>%
  mutate(DiputadoId = todos_diputados)

df_ideologia <- df_ideologia %>%
  left_join(presencia_df, by = "DiputadoId", suffix = c("", "_presencia"))

for (periodo in periodos_nombres) {
  presencia_col <- paste0(periodo, "_presencia")
  df_ideologia[[periodo]][!df_ideologia[[presencia_col]]] <- NA
  df_low[[periodo]][!df_ideologia[[presencia_col]]] <- NA
  df_high[[periodo]][!df_ideologia[[presencia_col]]] <- NA
}

df_ideologia <- df_ideologia %>%
  select(-ends_with("_presencia"))

df_ideologia_largo <- df_ideologia %>%
  pivot_longer(
    cols = starts_with("Periodo_"),
    names_to = "Periodo",
    values_to = "Ideologia"
  )

df_low_largo <- df_low %>%
  pivot_longer(
    cols = starts_with("Periodo_"),
    names_to = "Periodo",
    values_to = "IC_low"
  )

df_high_largo <- df_high %>%
  pivot_longer(
    cols = starts_with("Periodo_"),
    names_to = "Periodo",
    values_to = "IC_high"
  )

df_ideologia_largo <- df_ideologia_largo %>%
  left_join(df_low_largo, by = c("DiputadoId", "Periodo")) %>%
  left_join(df_high_largo, by = c("DiputadoId", "Periodo")) %>%
  left_join(id_full, by = "DiputadoId") %>%
  select(DiputadoId, NombreId, Periodo, IC_low, Ideologia, IC_high)

# Additionally, deputy Pablo Prieto Lorca has a different ID assigned
# for his first and fifth periods
# ID for period 1 is 209, ID for period 5 is 1064

df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId == 209][5] <-
  df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId == 1064][5]

df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId == 209][5] <-
  df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId == 1064][5]

df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId == 209][5] <-
  df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId == 1064][5]

df_ideologia_largo <- df_ideologia_largo[df_ideologia_largo$DiputadoId != 1064, ]

dir.create(dirname(ideology_output), recursive = TRUE, showWarnings = FALSE)
write.csv(df_ideologia_largo, ideology_output)
cat("Wrote ideology estimates:", ideology_output, "\n")
