#' @title Outpur do Igov Judiciário
#'

jud_output <- function(estoque_semdec, qtd_julgada_mes_tempo_medio){

output_completo <- estoque_semdec %>%
                   dplyr::left_join(qtd_julgada_mes_tempo_medio %>%
                   tidyr::pivot_wider(names_from = msmo_gov,
                          values_from = c(qtd, tempo_medio)) %>%
                  dplyr::select(-ends_with('_NA'))) %>%
    dplyr::rename(
                  estoque_sem_decisao = sem_decisao,
                  estoque_distribuicao = distribuido_tot,
                  estoque_decisao = decidido_tot,
                  ADI_favor_individual_mes = individual_qtd_favor,
                  ADI_contra_individual_mes = individual_qtd_contra,
                  ADI_favor_coletiva_mes = coletiva_qtd_favor,
                  ADI_contra_coletiva_mes = coletiva_qtd_contra,
                  ADI_total_distribuicao_gov_anterior = `qtd_Governo Anterior`,
                  ADI_total_distribuicao_gov_atual = `qtd_Governo Atual`,
                  tempo_medio_decisao_gov_anterior = `tempo_medio_Governo Anterior`,
                  tempo_medio_decisao_gov_atual = `tempo_medio_Governo Atual`,
  )

}
