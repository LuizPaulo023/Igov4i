#' @title Limpeza base de dados judiciário
#' @author Luiz Paulo Tavares


jud_clean <- function(db = as.data.frame(), depara = as.data.frame()){

  judiciario_sem_dup = aux_jud_clean(db, depara)

  #Adicionar o gov começando na distribuição
  dt_gov_distribuicao <- judiciario_sem_dup %>%
                         dplyr::select(data_distribuicao,
                                       data_decisao,
                                       gov_distribuicao) %>%
                         dplyr::filter(data_distribuicao < min(data_decisao)) %>%
                         dplyr::filter(!is.na(gov_distribuicao)) %>%
                         dplyr::rename(date = data_distribuicao,
                                       gov = gov_distribuicao) %>%
                         dplyr::select(-data_decisao) %>%
                         dplyr::distinct()

  # Base de dados judiciário limpa e organizada
  bd_index <- judiciario_sem_dup %>%
              dplyr::select(data_decisao,
                            gov_decisao,
                            index,
                            coletiva) %>%
              dplyr::rename(date = data_decisao,
                            gov = gov_decisao,
                            category = coletiva) %>%
              #Remove ADI sem decisão
              dplyr::filter(index != 0) %>%
              dplyr::bind_rows(dt_gov_distribuicao) %>%
              dplyr::mutate(index = ifelse(index == -1, 'contra', 'favor'),
                            category = ifelse(category == 1, 'coletiva', 'individual'),
                            date = lubridate::floor_date(date, 'month')) %>% dplyr::arrange(date)


  return(bd_index)


}
