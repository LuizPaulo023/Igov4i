#'
#' @author Luiz Paulo Tavares Gonçalves

igov_geral <- function(legislativo,judiciario, popularidade){

  legislativo <- legislativo %>% dplyr::select(date, gov, indice_legislativo)
  judiciario <- judiciario %>% dplyr::select(date, indice_judiciario)
  popularidade <- popularidade %>% dplyr::select(date, indice_pop)


  indice_gov <- legislativo %>%
                dplyr::full_join(judiciario) %>%
                dplyr::full_join(popularidade) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(indice_governabilidade = sum(indice_legislativo,indice_judiciario,indice_pop)/3,
                # dplyr::across(where(is.numeric), ~round(.x,2))
                ) %>%
                dplyr::ungroup() %>%
                dplyr::rename(Judiciario = indice_judiciario,
                              Legislativo = indice_legislativo,
                              Popularidade = indice_pop,
                              Governabilidade = indice_governabilidade) %>%
                dplyr::filter(date >= '2003-01-01')

}

