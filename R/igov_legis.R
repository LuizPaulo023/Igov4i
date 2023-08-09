#' @title Índice Legislativo
#' @name igov_legis
#' @author Gabriel Bellé & Luiz Paulo Tavares
#' @description Função para calcular o índice Legislativo.
#'
#' @param df Dataframe contendo as colunas \code{date}, \code{analisada}, \code{pura}, \code{emendada}, \code{sem_eficacia}, \code{gov}.
#' @param peso Constante indicando a relação da importância entre MPs puras e MPs emendadas para o índice.
#'
#' @details O arquivo de input deve corresponder a base de dados já limpa, contendo colunas indicando a quantidade de
#' Medidas Provisórias (MPs) julgadas pelo poder Legislativo, classificadas em:
#' \code{analisada} - Quantidade total de MPs analisadas;
#' \code{pura} - Quantidade de MPs aprovadas sem modificação;
#' \code{emendada} - Quantidade de MPs aprovadas com modificação;
#' \code{sem_eficacia} - Quantidade de MPs não aprovadas.
#' Além de conter as colunas:
#' \code{date} - coluna de datas no formato YYYY/MM/DD;
#' \code{gov} - Governo no qual houve o julgamento da MPs.
#'
#' @return O retorno é um dataframe agrupado por dia e governo contendo as colunas:
#' \code{date} - índice do estabelecimento diário;
#' \code{gov} - média do dia para o estabelecimento;
#' \code{analisada} - Quantidade total de MPs analisadas;
#' \code{pura} - Quantidade de MPs aprovadas sem modificação;
#' \code{emendada} - Quantidade de MPs aprovadas com modificação;
#' \code{sem_eficacia} - Quantidade de MPs não aprovadas.
#' \code{indice_pura} - Índice considerando apenas MPs puras;
#' \code{indice_emendada} - Índice considerando apenas MPs emendadas;
#' \code{indice_legislativo} - Índice agregando o indice_pura com o indice_emendada, utilizando um peso.
#
#'
#' @examples
#'
#' \dontrun{
#'
#' igov_legis(df = df_cleaned, peso = 2)
#'
#' }
#'
#' @export

igov_legis <- function(df, peso) {

  #' O peso é a relação entre o indice_pura e indice_emendada;
  #' A soma de ambos índices deve ser 100%, pois o índice deve variar entre 0 e 1.

  peso_pura = 1
  peso_emendada = 1/peso

  #' Os índices são uma média móvel de 12 períodos (englobando um ano),
  #' Ponderado pela quantidade de MPs.
  #'
  #' Seu cômputo é feito a partir de uma soma móvel das quantidadas e,
  #' Encontrando a porcentagem de cada categoria sobre o total de MPs
  indice_mm_ponderado <- df %>%
    tidyr::pivot_longer(-c(date, gov)) %>%
    tidyr::fill(gov) %>%
    dplyr::group_by(name, gov) %>%
    dplyr::mutate(soma_movel = zoo::rollapply(value,
                                              width = 12,
                                              FUN = function(x) sum(x, na.rm = T),
                                              by.column = T,
                                              partial = T,
                                              fill = NA,
                                              align = 'right')) %>% dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = name, values_from = c(soma_movel, value)) %>%
    stats::na.omit() %>%
    #group_by() %>%
    dplyr::mutate(pct_pura = soma_movel_pura/soma_movel_analisada,
                  pct_emendada = soma_movel_emendada/soma_movel_analisada,
                  pct_sem_eficacia = soma_movel_sem_eficacia/soma_movel_analisada) %>%
    dplyr::mutate(pura_peso = (pct_pura * peso_pura),
                  emendada_peso =  (pct_emendada * peso_emendada),
                  indice_legislativo = pura_peso + emendada_peso
    )
}

