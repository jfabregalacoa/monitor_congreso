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

aqui <- here()

base <- read.csv(paste0(aqui,"/results/ideologia_diputados_largo_emIRT_con_nombres.csv")) # change the path accordingly

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

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)


militantes <- function(df){
  alias_cols <- grep("^Alias(\\.|$)", names(df), value = TRUE)
  
  militancias_largo <- df %>%
    select(Id, all_of(alias_cols)) %>%
    pivot_longer(
      cols = all_of(alias_cols),
      names_to = "columna_alias",
      values_to = "partido"
    ) %>%
    filter(!is.na(partido)) %>%
    distinct(Id, partido, .keep_all = TRUE) 
  
  militancias_resumen <- militancias_largo %>%
    group_by(Id) %>%
    summarise(
      cantidad_partidos = n(),
      partidos = list(partido),
      .groups = "drop"
    )
  
  max_partidos <- max(militancias_resumen$cantidad_partidos)
  
  for (i in 1:max_partidos) {
    militancias_resumen[[paste0("partido_", i)]] <- sapply(
      militancias_resumen$partidos,
      function(x) if (length(x) >= i) x[[i]] else NA
    )
  }
  
  militancias_final <- militancias_resumen %>%
    select(-partidos)
  
}

militancias_1 <- militantes(per1)
militancias_2 <- militantes(per2)
militancias_3 <- militantes(per3)
militancias_4 <- militantes(per4)
militancias_5 <- militantes(per5)
militancias_6 <- militantes(per6)



################################################################################
# Joining the datasets
################################################################################

library(dplyr)

militancias_list <- list(militancias_1, militancias_2, militancias_3,
                         militancias_4, militancias_5, militancias_6)

periodos <- paste0("Periodo_", 1:6)
lista_actualizados <- list()


for (i in 1:6) {
  base_filtrada <- base %>%
    filter(Periodo == periodos[i])
  
  militancias_actual <- militancias_list[[i]]
  
  combinado <- base_filtrada %>%
    left_join(militancias_actual, by = c("DiputadoId" = "Id"))
  
  lista_actualizados[[i]] <- combinado
}

base_actualizada <- bind_rows(lista_actualizados)

base_actualizada <- base_actualizada %>%
  arrange(DiputadoId, Periodo)

base_actualizada <- base_actualizada %>%
  left_join(datos_unicos, by = c("DiputadoId" = "Id"))


colnames(base_actualizada) <- c("X","Id","Name","Period","dim_IC_low",
                                "dim_ideology","dim_IC_high","wnom_ideology","bay_ic_low","bay_ideology",
                                "bay_ic_high","quant_parties","party_1","party_2","party_3",
                                "party_4","Sex","Birth_date")
base_actualizada <- base_actualizada[,-c(1)] 

write.csv(base_actualizada,paste0(aqui,"/results/ideologia_congreso_chile_2002_2026_long_format.csv"))

long_wide_ready <- base_actualizada %>%
  mutate(Periodo = gsub("Periodo_", "P", Period)) %>%  
  select(
    Id, Name, Sex, Birth_date, Periodo,
    starts_with("dim_"), wnom_ideology,
    starts_with("bay_"), starts_with("party_")
  )

wide_data <- long_wide_ready %>%
  pivot_wider(
    id_cols = c(Id, Name, Sex, Birth_date),
    names_from = Periodo,
    values_from = c(
      dim_ideology, dim_IC_low, dim_IC_high,
      wnom_ideology,
      bay_ideology, bay_ic_low, bay_ic_high,
      party_1
    ),
    names_sep = "_"
  )

write_csv(wide_data, paste0(aqui,"/results/ideologia_congreso_chile_2002_2026_wide_format.csv"))
