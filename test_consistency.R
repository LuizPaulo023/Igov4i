#' @title teste de consistência 
#' @author Luiz Paulo T. Gonçalves 

base::rm(list = ls())

# Definindo diretório de trabalho ==============================================

user = base::getwd() %>%
       stringr::str_extract("^((?:[^/]*/){3})") %>% print()

path = base::setwd(paste0(user, "4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data/historico"))
# print(path)

# Importando índices ===========================================================
# Atualize os meses desejados \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
last_month = "outubro_2023"
current_month = "dezembro_2023"

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

resul_legis = legis_consistency(last = last_legis, current = current_legis)

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




