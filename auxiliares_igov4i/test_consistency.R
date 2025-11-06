#' @title teste de consistência
#' @author Luiz Paulo T. Gonçalves

base::rm(list = ls())

# Definindo diretório de trabalho ==============================================

pacman::p_load(tidyverse)

user = base::getwd() %>%
       stringr::str_extract("^((?:[^/]*/){3})") %>% print()

path = base::setwd(paste0(user, "4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data/historico"))
print(path)

# Importando índices ===========================================================
# Atualize os meses desejados \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Fri Dec  6 08:50:18 2024 ------------------------------
# Thu Jan  9 09:16:50 2025 ------------------------------

last_month = "2025/junho"
current_month = "2025/julho"

# Testando legislativo ==============================================================================

last_legis = readxl::read_excel(paste0(path, "/",last_month,"/","indice_legis.xlsx"))
current_legis = readxl::read_excel(paste0(path, "/",current_month,"/","indice_legis.xlsx"))

legis_consistency <- function(last, current){

  consistency = last %>%
                dplyr::select(date, indice_legislativo) %>%
                       rename(last_legis = indice_legislativo) %>%
                       left_join(current, by = "date") %>%
                       select(date, gov, last_legis, indice_legislativo) %>%
                       rename(current_legis = indice_legislativo)

  test <- consistency %>%
          mutate(test_consistency = last_legis == current_legis) %>%
          filter(test_consistency == FALSE)

 return(test)



}

resul_legis = legis_consistency(last = last_legis,
                                current = current_legis)

length(row(resul_legis))
resul_legis %>% glimpse()

# Testando Opinião pública =====================================================

last_pop = readxl::read_excel(paste0(path, "/",last_month,"/","indice_pop.xlsx"))
current_pop = readxl::read_excel(paste0(path, "/",current_month,"/","indice_pop.xlsx"))


op_consistency <- function(last, current){

  consistency = last %>%
                dplyr::select(date, indice_pop) %>%
                rename(last_pop = indice_pop) %>%
                left_join(current, by = "date") %>%
                select(date, index, last_pop, indice_pop) %>%
                rename(current_pop = indice_pop)

  test <- consistency %>%
          dplyr::mutate(test_consistency = last_pop == current_pop) %>%
          filter(test_consistency == FALSE)


return(test)

}

result_op = op_consistency(last = last_pop, current = current_pop)

result_op %>% glimpse()

# Teste Judiciário =============================================================

last_jud = readxl::read_excel(paste0(path, "/",last_month,"/","indice_judiciario.xlsx"))
current_jud = readxl::read_excel(paste0(path, "/",current_month,"/","indice_judiciario.xlsx"))

jud_consistency <- function(last, current){

  consistency = last %>%
                dplyr::select(date, indice_judiciario) %>%
                rename(last_jud = indice_judiciario) %>%
                left_join(current, by = "date") %>%
                select(date, gov, last_jud, indice_judiciario) %>%
                rename(current_jud = indice_judiciario)

  test <- consistency %>%
    mutate(test_consistency = last_jud == current_jud) %>%
    filter(test_consistency == FALSE)

  return(test)



}

result_jud = jud_consistency(last = last_jud, current = current_jud)
result_jud %>% glimpse()

