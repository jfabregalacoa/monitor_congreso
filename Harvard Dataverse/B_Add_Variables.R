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

library(here)
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(purrr)
  library(tibble)
})

aqui <- here()

base <- read.csv(paste0(aqui,"/results/ideologia_diputados_largo_emIRT_con_nombres.csv"))

diputados_periodo <- list.files(paste0(aqui,"/data/"), pattern = "diputado_periodo_")

per1 <- read.csv(paste0(aqui,"/data/",diputados_periodo[1]))
per2 <- read.csv(paste0(aqui,"/data/",diputados_periodo[2]))
per3 <- read.csv(paste0(aqui,"/data/",diputados_periodo[3]))
per4 <- read.csv(paste0(aqui,"/data/",diputados_periodo[4]))
per5 <- read.csv(paste0(aqui,"/data/",diputados_periodo[5]))
per6 <- read.csv(paste0(aqui,"/data/",diputados_periodo[6]))

# ______________________________________________________________________________
# save the information on gender and date of birth
# ______________________________________________________________________________

lista_df <- list(per1, per2, per3, per4, per5, per6)

solo_ids <- lapply(lista_df, function(df) {
  df %>%
    select(Id, Sexo, FechaNacimiento)
})

datos_combinados <- bind_rows(solo_ids)

datos_unicos <- datos_combinados %>%
  distinct(Id, .keep_all = TRUE)

# ______________________________________________________________________________
# save the information on political membership
# ______________________________________________________________________________

periodos_tbl <- tibble::tibble(
  periodo = c("per1","per2","per3","per4","per5","per6"),
  p_ini = as.Date(c("2002-03-11","2006-03-11","2010-03-11","2014-03-11","2018-03-11","2022-03-11")),
  p_fin = as.Date(c("2006-03-10","2010-03-10","2014-03-10","2018-03-10","2022-03-10","2026-03-10"))
)

.bounds_for <- function(periodo) {
  periodos_tbl %>%
    filter(periodo == !!periodo) %>%
    summarise(p_ini = first(p_ini), p_fin = first(p_fin), .groups = "drop") %>%
    as.list()
}

militantes_periodo <- function(df, periodo = c("per1","per2","per3","per4","per5","per6"),
                               coalescar_consecutivos = FALSE,
                               return_mode = c("wide","long","both")) {
  periodo <- match.arg(periodo)
  return_mode <- match.arg(return_mode)
  bd <- .bounds_for(periodo)
  p_ini <- bd$p_ini; p_fin <- bd$p_fin
  
  largo <- df %>%
    select(Id, matches("^(Alias|FechaInicio|FechaTermino)(\\.|$)")) %>%
    pivot_longer(
      cols = -Id,
      names_to   = c("campo","k"),
      names_pattern = "^(Alias|FechaInicio|FechaTermino)(?:\\.(\\d+))?$",
      values_to  = "valor",
      values_drop_na = FALSE
    ) %>%
    mutate(k = if_else(is.na(k), "0", k)) %>%  
    pivot_wider(names_from = "campo", values_from = "valor") %>%
    rename(
      partido       = Alias,
      fecha_ini_chr = FechaInicio,
      fecha_fin_chr = FechaTermino
    ) %>%
    filter(!is.na(partido) & str_trim(partido) != "")
  
  if (nrow(largo) == 0) {
    empty_wide <- df %>% distinct(Id) %>% mutate(cantidad_partidos = 0)
    if (return_mode == "wide") return(empty_wide)
    if (return_mode == "long") return(largo %>% mutate(periodo = periodo)[0,])
    return(list(wide = empty_wide, long = largo %>% mutate(periodo = periodo)[0,]))
  }
  
  largo <- largo %>%
    mutate(
      fecha_ini_raw = as_date(suppressWarnings(ymd_hms(fecha_ini_chr, tz = "UTC"))),
      fecha_fin_raw = as_date(suppressWarnings(ymd_hms(fecha_fin_chr, tz = "UTC"))),
      fecha_ini_raw = if_else(is.na(fecha_ini_raw), as.Date("0001-01-01"), fecha_ini_raw),
      fecha_fin_raw = if_else(is.na(fecha_fin_raw), as.Date("9999-12-31"), fecha_fin_raw),
      fecha_ini = pmin(fecha_ini_raw, fecha_fin_raw, na.rm = TRUE),
      fecha_fin = pmax(fecha_ini_raw, fecha_fin_raw, na.rm = TRUE)
    ) %>%
    select(-fecha_ini_raw, -fecha_fin_raw)
  
  largo_periodo <- largo %>%
    mutate(
      inicio_efectivo = pmax(fecha_ini, p_ini),
      fin_efectivo    = pmin(fecha_fin, p_fin)
    ) %>%
    filter(inicio_efectivo <= fin_efectivo) %>%
    arrange(Id, inicio_efectivo, fecha_ini)
  
  if (nrow(largo_periodo) == 0) {
    empty_wide <- df %>% distinct(Id) %>% mutate(cantidad_partidos = 0)
    if (return_mode == "wide") return(empty_wide)
    if (return_mode == "long") return(largo_periodo %>% mutate(periodo = periodo))
    return(list(wide = empty_wide, long = largo_periodo %>% mutate(periodo = periodo)))
  }
  
  if (isTRUE(coalescar_consecutivos)) {
    largo_periodo <- largo_periodo %>%
      group_by(Id, partido) %>%
      arrange(inicio_efectivo, .by_group = TRUE) %>%
      mutate(
        new_grp = cumsum(inicio_efectivo > (lag(fin_efectivo, default = first(inicio_efectivo)) + 1))
      ) %>%
      group_by(Id, partido, new_grp) %>%
      summarise(
        inicio_efectivo = min(inicio_efectivo),
        fin_efectivo    = max(fin_efectivo),
        .groups = "drop_last"
      ) %>%
      ungroup()
  }
  
  largo_periodo <- largo_periodo %>%
    mutate(periodo = periodo) %>%
    select(Id, partido, inicio_efectivo, fin_efectivo, periodo)
  
  wide_out <- largo_periodo %>%
    group_by(Id) %>%
    summarise(
      cantidad_partidos = n(),
      partidos = list(partido),
      .groups = "drop"
    )
  
  max_partidos <- max(wide_out$cantidad_partidos, na.rm = TRUE)
  for (i in seq_len(max_partidos)) {
    wide_out[[paste0("partido_", i)]] <- vapply(
      wide_out$partidos,
      function(x) if (length(x) >= i) x[[i]] else NA_character_,
      FUN.VALUE = character(1)
    )
  }
  wide_out <- wide_out %>% select(-partidos)
  
  if (return_mode == "wide") return(wide_out)
  if (return_mode == "long") return(largo_periodo)
  return(list(wide = wide_out, long = largo_periodo))
}

consolidar_periodos <- function(largos_por_periodo) {
  stopifnot(is.list(largos_por_periodo), length(largos_por_periodo) >= 1)
  
  traj <- dplyr::bind_rows(largos_por_periodo) %>%
    dplyr::arrange(Id, inicio_efectivo, fin_efectivo)
  
  traj_coalescada <- traj %>%
    dplyr::group_by(Id, partido) %>%
    dplyr::arrange(inicio_efectivo, .by_group = TRUE) %>%
    dplyr::mutate(
      grp = cumsum(inicio_efectivo >
                     (dplyr::lag(fin_efectivo, default = dplyr::first(inicio_efectivo)) + 1))
    ) %>%
    dplyr::group_by(Id, partido, grp) %>%
    dplyr::summarise(
      inicio = min(inicio_efectivo),
      fin    = max(fin_efectivo),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Id, inicio)
  
  estado_base <- traj_coalescada %>%
    tidyr::crossing(periodos_tbl) %>%
    dplyr::filter(inicio <= p_fin & fin >= p_ini) %>%
    dplyr::mutate(
      inicio_en_periodo = pmax(inicio, p_ini),
      fin_en_periodo    = pmin(fin,    p_fin)
    )
  
  if (nrow(estado_base) == 0) {
    return(list(
      traj_coalescada   = traj_coalescada,
      estado_por_periodo = tibble::tibble(
        Id = integer(), periodo = character(), n_partidos = integer()
      )
    ))
  }
  
  estado_por_periodo <- estado_base %>%
    dplyr::arrange(Id, periodo, inicio_en_periodo, inicio) %>%
    dplyr::group_by(Id, periodo) %>%
    dplyr::distinct(partido, .keep_all = TRUE) %>%  
    dplyr::summarise(
      partidos   = list(partido),
      n_partidos = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(partidos, indices_include = TRUE, indices_to = "pos") %>%
    dplyr::mutate(col = paste0("partido_", pos)) %>%
    dplyr::select(Id, periodo, n_partidos, col, partidos) %>%
    tidyr::pivot_wider(names_from = col, values_from = partidos) %>%
    dplyr::relocate(Id, periodo, n_partidos)
  
  list(
    traj_coalescada    = traj_coalescada,
    estado_por_periodo = estado_por_periodo
  )
}

L1 <- militantes_periodo(per1, "per1", coalescar_consecutivos = TRUE, return_mode = "long")
L2 <- militantes_periodo(per2, "per2", coalescar_consecutivos = TRUE, return_mode = "long")
L3 <- militantes_periodo(per3, "per3", coalescar_consecutivos = TRUE, return_mode = "long")
L4 <- militantes_periodo(per4, "per4", coalescar_consecutivos = TRUE, return_mode = "long")
L5 <- militantes_periodo(per5, "per5", coalescar_consecutivos = TRUE, return_mode = "long")
L6 <- militantes_periodo(per6, "per6", coalescar_consecutivos = TRUE, return_mode = "long")

W1 <- militantes_periodo(per1, "per1", coalescar_consecutivos = TRUE, return_mode = "wide")
W2 <- militantes_periodo(per2, "per2", coalescar_consecutivos = TRUE, return_mode = "wide")
W3 <- militantes_periodo(per3, "per3", coalescar_consecutivos = TRUE, return_mode = "wide")
W4 <- militantes_periodo(per4, "per4", coalescar_consecutivos = TRUE, return_mode = "wide")
W5 <- militantes_periodo(per5, "per5", coalescar_consecutivos = TRUE, return_mode = "wide")
W6 <- militantes_periodo(per6, "per6", coalescar_consecutivos = TRUE, return_mode = "wide")

CONS <- consolidar_periodos(list(L1,L2,L3,L4,L5,L6))
traj_coalescada <- CONS$traj_coalescada        
estado_por_periodo <- CONS$estado_por_periodo  

rebuild_estado <- function(traj_coalescada, periodos_tbl) {
  if (is.null(traj_coalescada) || !nrow(traj_coalescada)) {
    return(tibble(Id = integer(), periodo = character(), n_partidos = integer()))
  }
  estado_base <- traj_coalescada %>%
    tidyr::crossing(periodos_tbl) %>%
    dplyr::filter(inicio <= p_fin & fin >= p_ini) %>%
    dplyr::mutate(inicio_en_periodo = pmax(inicio, p_ini),
                  fin_en_periodo    = pmin(fin,    p_fin))
  
  if (!nrow(estado_base)) {
    return(tibble(Id = integer(), periodo = character(), n_partidos = integer()))
  }
  
  estado_por_periodo <- estado_base %>%
    dplyr::arrange(Id, periodo, inicio_en_periodo, inicio) %>%
    dplyr::group_by(Id, periodo) %>%
    dplyr::distinct(partido, .keep_all = TRUE) %>%   
    dplyr::summarise(partidos = list(partido),
                     n_partidos = dplyr::n(), .groups = "drop") %>%
    tidyr::unnest_longer(partidos, indices_include = TRUE, indices_to = "pos") %>%
    dplyr::mutate(col = paste0("partido_", pos)) %>%
    dplyr::select(Id, periodo, n_partidos, col, partidos) %>%
    tidyr::pivot_wider(names_from = col, values_from = partidos) %>%
    dplyr::relocate(Id, periodo, n_partidos)
  
  estado_por_periodo
}

estado_pp <- CONS$estado_por_periodo
if (is.null(estado_pp) || !NROW(estado_pp)) {
  estado_pp <- rebuild_estado(CONS$traj_coalescada, periodos_tbl)
}

partidos_por_periodo <- estado_pp %>%
  mutate(Periodo = paste0("Periodo_", parse_number(periodo))) %>%
  select(-periodo) %>%
  arrange(Id, Periodo) %>%
  distinct(Id, Periodo, .keep_all = TRUE)

################################################################################
# Joining the datasets
################################################################################
base_con_partidos <- base %>%
  left_join(partidos_por_periodo,
            by = c("DiputadoId" = "Id", "Periodo" = "Periodo"))

datos_unicos_limpio <- datos_unicos %>%
  mutate(FechaNacimiento = suppressWarnings(ymd_hms(FechaNacimiento, tz = "UTC")) |> as_date())

base_final <- base_con_partidos %>%
  left_join(datos_unicos_limpio, by = c("DiputadoId" = "Id"))

conteo_asignados <- base_final %>%
  mutate(tiene_partido = if_any(starts_with("partido_"), ~ !is.na(.))) %>%
  count(Periodo, tiene_partido, name = "n_filas")

colnames(base_final) <- c("X","DiputadoId","Name","Period","dim_IC_low",
                          "dim_ideology","dim_IC_high","wnom_ideology","bay_ic_low","bay_ideology",
                          "bay_ic_high","quant_parties","party_1","party_2","party_3","Sex","Birth_date")

#-------------------------------------------------------------------------------
# wide format
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

period_col <- if ("Period" %in% names(base_final)) "Period" else "Periodo"
name_col   <- if ("Name"   %in% names(base_final)) "Name"   else "NombreId"
sex_col    <- if ("Sex"    %in% names(base_final)) "Sex"    else "Sexo"
birth_col  <- if ("Birth_date" %in% names(base_final)) "Birth_date" else "FechaNacimiento"

party_pat  <- if (any(grepl("^party_\\d+$", names(base_final)))) "^party_\\d+$" else "^partido_\\d+$"
cols_partidos <- grep(party_pat, names(base_final), value = TRUE)

first_non_na <- suppressWarnings(which(!is.na(base_final[[period_col]]))[1])
levels_prefix <- if (!is.na(first_non_na) && grepl("^Period_", base_final[[period_col]][first_non_na])) "Period_" else "Periodo_"
levels_periodos <- paste0(levels_prefix, 1:6)

personas <- base_final %>%
  arrange(DiputadoId) %>%
  group_by(DiputadoId) %>%
  summarise(
    !!sex_col   := dplyr::first(.data[[sex_col]]),
    !!birth_col := dplyr::first(.data[[birth_col]]),
    .groups = "drop"
  )

vars_no_partido <- setdiff(
  names(base_final),
  c("DiputadoId", period_col, sex_col, birth_col, cols_partidos)
)

metrics_wide <- base_final %>%
  mutate(PER = factor(.data[[period_col]], levels = levels_periodos)) %>% 
  select(DiputadoId, PER, all_of(vars_no_partido)) %>%
  pivot_wider(
    names_from  = PER,                       
    values_from = all_of(vars_no_partido),
    names_glue  = "{PER}__{.value}"
  )

partidos_long <- base_final %>%
  mutate(PER = factor(.data[[period_col]], levels = levels_periodos)) %>%
  select(DiputadoId, PER, all_of(cols_partidos)) %>%
  pivot_longer(
    cols = all_of(cols_partidos),
    names_to = "k",
    values_to = "partido",
    values_drop_na = TRUE
  ) %>%
  mutate(k = readr::parse_number(k)) %>%
  arrange(DiputadoId, PER, k)

partidos_per_periodo_wide <- partidos_long %>%
  mutate(col = paste0(PER, "_partido_", k)) %>%
  select(DiputadoId, col, partido) %>%
  pivot_wider(names_from = col, values_from = partido)

partidos_global_long <- partidos_long %>%
  arrange(DiputadoId, PER, k) %>%
  group_by(DiputadoId) %>%
  mutate(global_pos = row_number()) %>%
  ungroup()

partidos_global_only <- partidos_global_long %>%
  transmute(DiputadoId,
            col = paste0("partido_global_", global_pos),
            partido) %>%
  pivot_wider(names_from = col, values_from = partido)

periodos_global_only <- partidos_global_long %>%
  transmute(DiputadoId,
            col = paste0("periodo_global_", global_pos),
            Periodo = as.character(PER)) %>%
  pivot_wider(names_from = col, values_from = Periodo)

partidos_global_wide <- partidos_global_only %>%
  left_join(periodos_global_only, by = "DiputadoId")

base_wide <- personas %>%
  left_join(metrics_wide,              by = "DiputadoId") %>%
  left_join(partidos_per_periodo_wide, by = "DiputadoId") %>%
  left_join(partidos_global_wide,      by = "DiputadoId")

#------------------------------------------------------------------------------
# renaiming variables and droping auxiliary variables
#------------------------------------------------------------------------------

rename_cols <- function(df) {
  names(df) <- names(df) |>
    str_replace_all("Periodo", "Period") |>
    str_replace_all("partido", "party")
  df
}
base_final <- rename_cols(base_final)
base_wide  <- rename_cols(base_wide)

drop_exact <- c(paste0("Periodo_", 1:6, "__X"), 
                paste0("Period_",  1:6, "__X"))  
base_wide <- base_wide %>% select(-any_of(intersect(drop_exact, names(base_wide))))


cols_drop_name <- c(paste0("Period_",  2:6, "__", name_col),
                    paste0("Periodo_", 2:6, "__", name_col))  
base_wide <- base_wide %>% select(-any_of(intersect(cols_drop_name, names(base_wide))))

cand_name1 <- c(paste0("Period_1__" , name_col),
                paste0("Periodo_1__", name_col))
cand_name1 <- cand_name1[cand_name1 %in% names(base_wide)]
if (length(cand_name1) == 1 && !(name_col %in% names(base_wide))) {
  base_wide <- base_wide %>% rename(!!name_col := all_of(cand_name1))
} else if (length(cand_name1) == 1 && (name_col %in% names(base_wide))) {
  base_wide <- base_wide %>%
    mutate(!!name_col := dplyr::coalesce(.data[[name_col]], .data[[cand_name1]])) %>%
    select(-all_of(cand_name1))
}


cols_drop_globals <- c(paste0("party_global_",   1:7),
                       paste0("periodo_global_", 1:7),  
                       paste0("partido_global_", 1:7))  
base_wide <- base_wide %>% select(-any_of(intersect(cols_drop_globals, names(base_wide))))

cols_drop_nparties <- c(paste0("Period_",  1:6, "__n_partys"),
                        paste0("Periodo_", 1:6, "__n_partys"))
base_wide <- base_wide %>% select(-any_of(intersect(cols_drop_nparties, names(base_wide))))

reordenar_parties_final <- function(df) {
  nms <- names(df)
  party_cols <- grep("^(?:Periodo|Period)_[1-6]_party_\\d+$", nms, value = TRUE)
  if (length(party_cols)) {
    m <- stringr::str_match(party_cols, "^(?:Periodo|Period)_([1-6])_party_(\\d+)$")
    party_cols_sorted <- party_cols[order(as.integer(m[,2]), as.integer(m[,3]))]
  } else {
    party_cols_sorted <- character(0)
  }
  other_cols <- setdiff(nms, party_cols)
  df %>% select(all_of(other_cols), all_of(party_cols_sorted))
}
base_wide <- reordenar_parties_final(base_wide)

rename_metric_tokens <- function(df) {
  df %>%
    rename_with(
      ~ .x %>%
        str_replace_all("Ideologia_wnom", "wnom_ideology") %>%
        str_replace_all("__IC_low",  "dim_IC_low") %>%
        str_replace_all("__IC_high", "dim_IC_high") %>%
        str_replace_all("(?<=__)Ideologia(?!_)", "dim_ideology") %>%
        str_replace_all("(?<=__)ic_low(?!_)",  "bay_ic_low") %>%
        str_replace_all("(?<=__)ic_high(?!_)", "bay_ic_high") %>%
        str_replace_all("(?<=__)media(?!_)",   "bay_ideology"),
      .cols = everything()
    )
}

base_wide <- rename_metric_tokens(base_wide)

#------------------------------------------------------------------------------
# Saving data
#------------------------------------------------------------------------------

write.csv(base_final,paste0(aqui,"/results/ideologia_congreso_chile_2002_2026_long_format.csv"))
write_csv(base_wide, paste0(aqui,"/results/ideologia_congreso_chile_2002_2026_wide_format.csv"))

