#' @title Tempo médio de julgamento
#'

jud_tempo_julgamento <- function(judiciario_sem_dup){

  #Adicionar o gov começando na distribuição
  dt_gov_distribuicao <- judiciario_sem_dup %>%
                         dplyr::select(data_distribuicao, data_decisao, gov_distribuicao) %>%
                         dplyr::filter(data_distribuicao < min(data_decisao)) %>%
                         dplyr::filter(!is.na(gov_distribuicao)) %>%
                         dplyr::rename(date = data_distribuicao,
                                       gov = gov_distribuicao) %>%
                         dplyr::select(-data_decisao) %>%
                         dplyr::distinct()

  qtd_julgada_mes_tempo_medio <- judiciario_sem_dup %>%
                                 dplyr::filter(index != 0) %>%
                                 dplyr::select(-coletiva) %>%
                                 dplyr::mutate(msmo_gov = ifelse(gov_distribuicao == gov_decisao, 'Governo Atual', 'Governo Anterior'),
                                            msmo_gov = ifelse(is.na(msmo_gov), 'Governo Anterior', msmo_gov)) %>%
                                 dplyr::rename(date = data_decisao,
                                            gov = gov_decisao) %>%
                                 dplyr::select(-ends_with('distribuicao')) %>%
                                 dplyr::mutate(date = lubridate::floor_date(date, 'month')) %>%
                                 dplyr::group_by(date, gov, msmo_gov) %>%
                                 dplyr::summarise(qtd = n(),
                                               tempo_medio = mean(tempo)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::bind_rows(dt_gov_distribuicao) %>%
                                 dplyr::arrange(date)


  return(qtd_julgada_mes_tempo_medio)

}

