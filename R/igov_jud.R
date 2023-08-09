#' @title Índice Judiciário.
#' @name igov_jud
#'
#' @description Função para o cálculo do índice Judiciário.
#'
#' @param df Dataframe contendo as colunas \code{date}, \code{gov}, \code{index}, \code{category}.
#' @param peso Constante indicando a relação da importância entre ADIs coletivas e ADIs individuais para o índice.
#' @author Gabriel Bellé & Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a base de dados já limpa, contendo colunas indicando ocorrência das
#' Ações Diretas de Inconstitucionalidades (ADIs) julgadas pelo poder Judiciário.
#' Cada linha representa uma ação julgada que foi solicitada e finalizada dentro de um mesmo governo:
#' O arquivo deve conter as seguintes colunas:
#' \code{date} - Coluna de datas no formato YYYY/MM/DD;
#' \code{gov} - Governo no qual houve o julgamento da ADIs;
#' \code{index} - Decisão tomada pelo Judiciário (contra ou favor);
#' \code{category} - Organização do Judiciário que tomou a decisão (coletiva ou individual);
#'
#' @return O retorno é um dataframe agrupado por dia e governo contendo as colunas:
#' \code{date} - Coluna de datas no formato YYYY/MM/DD;
#' \code{gov} - Governo no qual houve o julgamento da ADIs;
#' \code{indice_judiciario) - Índice Judiciário que agrega os sub-índices judiciário, utilizando um peso.
#'
#' @examples
#' \dontrun{
#'
#' calc_judiciario(df = df_cleaned, peso = 2)
#'
#' }
#'
#' @export

igov_jud <- function(df, peso) {

  #' O peso é a relação entre o indice_coletivo_favoravel e indice_individual_favoravel;
  #' A soma de ambos índices deve ser 100%, pois o índice deve variar entre 0 e 1.

  peso_col = 1
  peso_indv = 1/peso

  min_dt = min(df$date)
  max_dt = max(df$date)

  sub_indv <- calc_new_sub_indice_judiciario(df, group_name = 'individual',
                                             min_dt = min_dt, max_dt = max_dt)
  sub_col <- calc_new_sub_indice_judiciario(df, group_name = 'coletiva',
                                            min_dt = min_dt, max_dt = max_dt)
  sub_agg <- calc_new_sub_indice_judiciario(df, group_name = 'agg',
                                            min_dt = min_dt, max_dt = max_dt)

  indice <- base::data.frame(sub_indv,
                             sub_col,
                             sub_agg) %>%
           dplyr::rowwise() %>%
           dplyr::mutate(indice_judiciario = sum((coletiva_pct_favor * agg_pct_coletiva) * peso_col,
                                            (individual_pct_favor * agg_pct_individual) * peso_indv, na.rm = T),
                         indice_judiciario = ifelse(if_all(contains('_pct_'), is.na),NA, indice_judiciario)) %>%
           dplyr::ungroup() %>%
           dplyr::select(date, gov, indice_judiciario, contains('_qtd_')) %>%
           dplyr::select(-c(starts_with('agg_qtd'), ends_with('_total')))

  return(indice)
}
