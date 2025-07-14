################################################################################
# This script is provided for replication of 
# Fábrega, Jorge, 2025, "Ideological Estimates of the Chilean Chamber of Deputies, 2002–2026", 
# https://doi.org/10.7910/DVN/FOXOIT, Harvard Dataverse, 
# DRAFT VERSION, UNF:6:0R5mlR/SgWltI4kra2OeVg== [fileUNF] 
#
# If you have any question or comment please reach me at:
# Contact email: jfabrega@udd.cl
################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(emIRT)
library(here)

aqui <- here()

# Roll calls per period
archivos_periodo <- c("matriz__periodo_2002_06.csv",
                      "matriz__periodo_2006_10.csv",
                      "matriz__periodo_2010_14.csv",
                      "matriz__periodo_2014_18.csv",
                      "matriz__periodo_2018_22.csv",
                      "matriz__periodo_2022_26.csv")
archivos <- file.path(aqui, "data", archivos_periodo) # Change this for your own path to the files

id_data <- lapply(archivos, function(f) fread(f, select = c("DiputadoId", "Nombre", "ApellidoPaterno", "ApellidoMaterno")))
id_full <- rbindlist(id_data) %>%
  mutate(NombreId = paste(Nombre, ApellidoPaterno, ApellidoMaterno, sep = " ")) %>%
  select(DiputadoId, NombreId) %>%
  distinct()

fwrite(id_full, file = file.path(aqui, "data", "Identificacion_parlamentarios.csv")) # this auxiliary data set will be used later

################################################################################
# Single period ideological estimations are done using both 
# a frequentist method (DW-NOMINATE) and a bayesian one (PSCL)
################################################################################




# ------------------------------------------------------------------------------
# Frequentist version 
# ------------------------------------------------------------------------------
library(wnominate)

codifica_voto_nominate <- function(x) {
  x <- as.character(x)
  ifelse(x == "Afirmativo", 1,
         ifelse(x == "En Contra", 0, NA))
}

procesa_matriz_nominate <- function(file) {
  dt <- fread(file)
  if (names(dt)[1] == "") dt <- dt[, -1, with = FALSE]  
  votos <- dt[, 6:ncol(dt), with = FALSE]
  votos <- as.data.frame(lapply(votos, codifica_voto_nominate))
  dt_votos <- cbind(DiputadoId = dt$DiputadoId, votos)
  rownames(dt_votos) <- dt$DiputadoId
  as.matrix(dt_votos[, -1])
}

matrices_voto_nominate <- lapply(archivos, procesa_matriz_nominate)
names(matrices_voto_nominate) <- paste0("Periodo_", 1:6)

wnominate_resultados <- list()


for (i in 1:6) {
  matriz <- matrices_voto_nominate[[i]]
  
  ancla <- intersect(rownames(matriz),c(908,865,857,942,917,1135))[1] # for polarity
  # 908 = Ignacio Urrutia Bonilla
  # 865 = Patricio Melero Abaroa
  # 857 = José Antonio Kast
  # 942 = Javier Macaya Danús
  # 917 = Gastón Von Mühlenbrock Zamora
  # 1135 = Johannes Kaiser Barents-Von Hohenhagen
  
  rollcall_obj <- rollcall(
    data = matriz,
    yea = 1,
    nay = 0,
    missing = NA,
    legis.names = rownames(matriz),
    vote.names = colnames(matriz),
    legis.data = NULL,
    desc = paste("Periodo", i)
  )
  
  resultado <- wnominate(
    rcObject = rollcall_obj,
    dims = 1,  # You may like to estimate a second dimension for your own purposes
    polarity = ancla  
  )
  
  resultado$DiputadoId <- row.names(resultado$legislators)
  wnominate_resultados[[paste0("Periodo_", i)]] <- resultado

}

ideal_points_wnom <- lapply(wnominate_resultados, function(x) {
  puntos <- x$legislators[, "coord1D"]
  nombres <- x$DiputadoId 
  names(puntos) <- as.character(nombres)
  puntos
})


# ------------------------------------------------------------------------------
# Bayesian version
# ------------------------------------------------------------------------------

library(pscl)

ideal_resultados <- list()

for (i in 1:6) {
  matriz <- matrices_voto_nominate[[i]]
  
  ancla <- intersect(rownames(matriz),c(908,865,857,942,917,1135))[1] 
  
  rollcall_obj <- rollcall(
    data = matriz,
    yea = 1,
    nay = 0,
    missing = NA,
    legis.names = rownames(matriz),
    vote.names = colnames(matriz),
    legis.data = NULL,
    desc = paste("Periodo", i)
  )
  
  resultado <- ideal(
    object = rollcall_obj,
    d = 1,
    store.item = TRUE,
    burnin = 500,
    thin = 10,
    maxiter = 1000,
    startvals = "eigen",
    verbose = TRUE
  )
  
  ideal_resultados[[paste0("Periodo_", i)]] <- resultado
}

ideal_points_ideal <- lapply(ideal_resultados, function(x) {

  muestras <- x$x            
  nombres <- colnames(muestras)
  medias <- apply(muestras, 2, mean)
  ic_low <- apply(muestras, 2, quantile, probs = 0.025)
  ic_high <- apply(muestras, 2, quantile, probs = 0.975)
  
  df <- data.frame(
    DiputadoId = nombres,
    ic_low = ic_low,
    media = medias,
    ic_high = ic_high,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
})

# Given that there is not a natural order for ideological estimates, 
# we changed the order ex-post to align both estimates when it was required 

ideal_points_ideal$Periodo_2$media <- -ideal_points_ideal$Periodo_2$media
ideal_points_ideal$Periodo_4$media <- -ideal_points_ideal$Periodo_4$media
ideal_points_ideal$Periodo_5$media <- -ideal_points_ideal$Periodo_5$media

ideal_points_ideal$Periodo_2$ic_low <- -ideal_points_ideal$Periodo_2$ic_low      
ideal_points_ideal$Periodo_4$ic_low <- -ideal_points_ideal$Periodo_4$ic_low
ideal_points_ideal$Periodo_5$ic_low <- -ideal_points_ideal$Periodo_5$ic_low

ideal_points_ideal$Periodo_2$ic_high <- -ideal_points_ideal$Periodo_2$ic_high      
ideal_points_ideal$Periodo_4$ic_high <- -ideal_points_ideal$Periodo_4$ic_high
ideal_points_ideal$Periodo_5$ic_high <- -ideal_points_ideal$Periodo_5$ic_high

# ------------------------------------------------------------------------------
# Dynamic estimate 
# ------------------------------------------------------------------------------

# preparing data
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


todos_diputados <- unique(unlist(lapply(archivos, function(f) fread(f, select="DiputadoId")$DiputadoId)))

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
  mapply(function(t, n) rep(t-1, n), 1:length(num_items_per_period), num_items_per_period)
)

N <- nrow(rc)
T <- length(matrices_voto_full)

presencia <- sapply(matrices_voto_full, function(mat) rowSums(abs(mat) > 0, na.rm=TRUE) > 0)

startlegis <- apply(presencia, 1, function(x) which(x)[1] - 1)  # base 0
endlegis   <- apply(presencia, 1, function(x) rev(which(x))[1] - 1)

N <- NROW(rc)
J <- NCOL(rc)
T <- 6

rc <- matrix(as.numeric(rc), nrow = nrow(rc), ncol = ncol(rc), dimnames = NULL)
startlegis <- matrix(as.integer(startlegis), nrow = length(startlegis), ncol = 1, dimnames = NULL)
endlegis   <- matrix(as.integer(endlegis),   nrow = length(endlegis),   ncol = 1, dimnames = NULL)
bill.session <- matrix(as.integer(bill.session), nrow = length(bill.session), ncol = 1, dimnames = NULL)
T <- as.integer(T)

attributes(rc) <- attributes(matrix(0, nrow = nrow(rc), ncol = ncol(rc)))
attributes(startlegis) <- attributes(matrix(0, nrow = nrow(startlegis), ncol = 1))
attributes(endlegis) <- attributes(matrix(0, nrow = nrow(endlegis), ncol = 1))
attributes(bill.session) <- attributes(matrix(0, nrow = nrow(bill.session), ncol = 1))

.data <- list(
  rc = rc,
  startlegis = startlegis,
  endlegis   = endlegis,
  bill.session = bill.session,
  T = T
)

# Estimation
set.seed(123)  # Please do not change - for replication 

starts <- list(
  alpha = matrix(rnorm(J, mean = 0, sd = 0.1), ncol = 1),           
  beta  = matrix(rnorm(J, mean = 1, sd = 0.1), ncol = 1),          
  x     = matrix(0, nrow = N, ncol = T)                           
)

priors <- list(
  x.mu0      = matrix(0, nrow = N, ncol = 1),              
  x.sigma0   = matrix(1, nrow = N, ncol = 1),              
  beta.mu    = matrix(c(0, 1), nrow = 2),                  
  beta.sigma = diag(2),                                    
  omega2     = matrix(1, nrow = N, ncol = 1)               
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
x_low[, periodos_invertir]  <- -1 * x_low[, periodos_invertir]
x_high[, periodos_invertir] <- -1 * x_high[, periodos_invertir]

periodos_nombres <- paste0("Periodo_", seq_len(ncol(x_mean)))
colnames(x_mean) <- periodos_nombres
colnames(x_low)  <- periodos_nombres
colnames(x_high) <- periodos_nombres

library(dplyr)
library(tidyr)

low_long <- as.data.frame(x_low) %>%
  mutate(Diputado = row_number()) %>%
  pivot_longer(cols = starts_with("Periodo_"), names_to = "Periodo", values_to = "IC_low")

x_long <- as.data.frame(x_mean) %>%
  mutate(Diputado = row_number()) %>%
  pivot_longer(cols = starts_with("Periodo_"), names_to = "Periodo", values_to = "Estimacion")

high_long <- as.data.frame(x_high) %>%
  mutate(Diputado = row_number()) %>%
  pivot_longer(cols = starts_with("Periodo_"), names_to = "Periodo", values_to = "IC_high")

x_long_full <- x_long %>%
  left_join(low_long, by = c("Diputado", "Periodo")) %>%
  left_join(high_long, by = c("Diputado", "Periodo"))


library(tidyverse)
library(data.table)

id_full <- fread(file.path(aqui, "data", "Identificacion_parlamentarios.csv"))

colnames(x_mean) <- paste0("Periodo_", 1:ncol(x_mean))
colnames(x_low) <- paste0("Periodo_", 1:ncol(x_low))
colnames(x_high) <- paste0("Periodo_", 1:ncol(x_high))

stopifnot(length(todos_diputados) == nrow(x_mean))

df_ideologia <- as.data.frame(x_mean) %>%
  mutate(DiputadoId = todos_diputados)

df_low <- as.data.frame(x_low) %>%
  mutate(DiputadoId = todos_diputados)

df_high <- as.data.frame(x_high) %>%
  mutate(DiputadoId = todos_diputados)

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


################################################################################
# We correct cases in which a legislator served in non-consecutive periods,
# because in those cases, the algorithm still assigns them a value for the 
# intermediate period(s) in which they were not in office.
################################################################################

# this correction applies to the following deputies:
# --- Gabriel Ascencio Mansilla, was not a deputy in period 4, id = 810
# --- Gaston Von Muhlenbrock, was not a deputy in period 4, id = 917
# --- Carlos Kuschel, was not a deputy in periods 2 to 4, id = 172
# --- Carlos Vilches Guzman, was not a deputy in period 2, id = 962
# --- Jaime Mulet, was not a deputy in periods 3 and 4, id = 872
# --- José Insunza, was not a deputy in period 3, id = 852
# --- René Alinco, was not a deputy in period 4, id = 803
# --- Marcelo Díaz, was not a deputy in period 4, id = 827
# --- Nino Batolu, was not a deputy in period 4, id = 926
# --- Felipe Letelier Norambuena, was not a deputy in periods 2 and 3, id = 117
# --- Sergio Bobadilla Muñoz, was not a deputy in period 4, id = 815
# --- Frank Sauerbaum Muñoz, was not a deputy in period 4, id = 953
# --- Pedro Velásquez Seguel, was not a deputy in period 4, id = 961
# --- Gaspar Rivas Sánchez, was not a deputy in period 5, id = 948
# --- Daniel Melo Contreras, was not a deputy in period 5, id = 990

df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==810][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==917][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==172][2:4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==962][2] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==872][3:4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==852][3] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==803][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==827][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==926][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==177][2] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==177][3] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==815][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==953][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==961][4] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==948][5] <- NA
df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==990][5] <- NA

df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==810][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==917][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==172][2:4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==962][2] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==872][3:4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==852][3] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==803][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==827][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==926][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==177][2] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==177][3] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==815][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==953][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==961][4] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==948][5] <- NA
df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==990][5] <- NA

df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==810][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==917][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==172][2:4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==962][2] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==872][3:4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==852][3] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==803][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==827][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==926][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==177][2] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==177][3] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==815][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==953][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==961][4] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==948][5] <- NA
df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==990][5] <- NA


agrega_estimaciones <- function(df, estimaciones_lista, nombre_columna) {
  df[[nombre_columna]] <- mapply(function(id, periodo) {
    puntos_periodo <- estimaciones_lista[[periodo]]
    if (!is.null(puntos_periodo) && id %in% names(puntos_periodo)) {
      return(puntos_periodo[[id]])
    } else {
      return(NA)
    }
  }, as.character(df$DiputadoId), df$Periodo, USE.NAMES = FALSE)
  return(df)
}

df_ideologia_largo <- agrega_estimaciones(df_ideologia_largo, ideal_points_wnom, "Ideologia_wnom")

agrega_estimaciones_multiple <- function(df, estimaciones_lista, columnas_salida, columna_id = "DiputadoId") {

  for (col in columnas_salida) {
    df[[col]] <- NA
  }
  
  for (i in seq_len(nrow(df))) {
    periodo <- df$Periodo[i]
    id <- as.character(df[[columna_id]][i])
    
    datos_periodo <- estimaciones_lista[[periodo]]
    
    if (!is.null(datos_periodo) && id %in% datos_periodo[[columna_id]]) {
      fila <- datos_periodo[datos_periodo[[columna_id]] == id, ]
      for (col in columnas_salida) {
        df[[col]][i] <- fila[[col]]
      }
    }
  }
  
  return(df)
}


df_ideologia_largo <- agrega_estimaciones_multiple(
  df = df_ideologia_largo,
  estimaciones_lista = ideal_points_ideal,
  columnas_salida = c("ic_low", "media", "ic_high")
)

# Additionally, deputy Pablo Prieto Lorca has a different ID assigned
# for his first and fifth periods
# ID for period 1 is 209, ID for period 5 is 1064

df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$IC_low[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$Ideologia[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$IC_high[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo$Ideologia_wnom[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$Ideologia_wnom[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo$ic_low[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$ic_low[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo$media[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$media[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo$ic_high[df_ideologia_largo$DiputadoId==209][5] <- 
  df_ideologia_largo$ic_high[df_ideologia_largo$DiputadoId==1064][5]

df_ideologia_largo <- df_ideologia_largo[df_ideologia_largo$DiputadoId != 1064, ]


write.csv(df_ideologia_largo,paste0(aqui,"/results/ideologia_diputados_largo_emIRT_con_nombres.csv"))

