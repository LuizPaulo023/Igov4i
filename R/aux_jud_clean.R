#' @title Auxiliar na limpeza base de dados judiciário
#' @author Luiz Paulo Tavares

aux_jud_clean <- function(db = as.data.frame(), depara = as.data.frame()){

    jud_raw <- db %>%
               janitor::clean_names() %>%
               #Remove ADI com a decisão feita antes da distribuição
               dplyr::filter((data_distribuicao <= data_decisao) | index == 0) %>%
               dplyr::mutate(coletiva = ifelse(coletiva == 0, 0, 1),
                             depara_distribuicao = stringr::str_sub(data_distribuicao,
                                                                    start = 1,
                                                                    end = 7),
                             depara_decisao = stringr::str_sub(data_decisao,
                                                               start = 1,
                                                               end = 7)) %>%
                dplyr::left_join(depara, by = c("depara_distribuicao" = "depara")) %>%
                dplyr::rename(gov_distribuicao = depara_gov) %>%
                dplyr::left_join(depara, by = c("depara_decisao" = "depara")) %>%
                dplyr::rename(gov_decisao = depara_gov) %>%
                dplyr::select(-depara_distribuicao,
                              -depara_decisao,
                              -individual)

    #Remove ADIs que foram julgadas posteriormente novamente
    #Mantem apenas a última decisão
    judiciario_sem_dup <- jud_raw %>%
                          dplyr::mutate(tempo = as.integer(difftime(data_decisao,
                                                           data_distribuicao,
                                                           units = "weeks")),
                                       adi = stringr::str_remove(adi, "\\*+")) %>%
                          dplyr::group_by(adi) %>%
                          dplyr::filter(tempo == max(tempo)) %>% dplyr::ungroup()


    return(judiciario_sem_dup)

}


