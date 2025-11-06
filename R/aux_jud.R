#' @title Sub-índices para cálculo do índice Judiciário.
#' @name aux_jud
#'
#' @description Função para calcular sub-índices necessários para o cálculo do índice Judiciário.
#'
#' @param df Dataframe contendo as colunas \code{date}, \code{gov}, \code{index}, \code{category}.
#' @param group_name String contendo o grupo que será utilizado para calcular o sub-índice.
#' @author Gabriel Bellé & Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a base de dados já limpa, contendo colunas indicando ocorrência das
#' Ações Diretas de Inconstitucionalidades (ADIs) julgadas pelo poder Judiciário.
#' Cada linha representa uma ação julgada que foi solicitada e finalizada dentro de um mesmo governo:
#' O arquivo deve conter as seguintes colunas:
#' \code{date} - coluna de datas no formato YYYY/MM/DD;
#' \code{gov} - Governo no qual houve o julgamento da ADIs;
#' \code{index} - Decisão tomada pelo Judiciário (contra ou favorável);
#' \code{category} - Organização do Judiciário que tomou a decisão (coletivo ou individual);
#'
#' @return O retorno é um dataframe agrupado por dia e governo contendo as colunas:
#' \code{date} - Coluna de datas no formato YYYY/MM/DD;
#' \code{gov} - Governo no qual houve o julgamento da ADIs;
#' \code{pct) - Coluna contendo a porcentagem que representa o sub-índice,
#'              o nome varia de acordo com o parâmetro \code{group_name}.
#'
#' @examples
#' \dontrun{
#' calc_sub_indice_judiciario(df = df_cleaned, group_name = 'individual')
#' }
#'
#' @export

aux_jud <- function(df, group_name = 'coletiva',
                                           min_dt, max_dt) {

  if (group_name %in% unique(df$category)) {
    df_grouped <- df %>%
      dplyr::filter(category == group_name) %>%
      dplyr::group_by(date, gov, index) %>%
      dplyr::summarise(qtd = n()) %>%
      dplyr::ungroup() %>%
      dplyr::rename(grouped = 3)
  } else{
    df_grouped <- df %>%
      dplyr::filter(!is.na(category)) %>%
      dplyr::group_by(date, gov, category) %>%
      dplyr::summarise(qtd = n()) %>%
      dplyr::ungroup() %>%
      dplyr::rename(grouped = 3)
  }

  sub_indice <- df_grouped %>%
    tidyr::pivot_wider(names_from = grouped, values_from = qtd) %>%
    dplyr::right_join(
    dplyr::tibble(date = seq(min_dt, max_dt, by = 'month'))
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ifelse(is.na(.x), 0, .x)),
      total = base::rowSums(dplyr::across(where(is.numeric)))) %>%
    tidyr::pivot_longer(-c(date, gov)) %>%
    dplyr::mutate(
      gov = case_when(
        date > '1994-12-01' &
          date <= '2002-12-01' ~ 'FHC',
        date > '2002-12-01' &
          date <= '2006-12-01' ~ 'Lula',
        date > '2006-12-01' &
          date <= '2010-12-01' ~ 'Lula',
        date > '2010-12-01' &
          date <= '2014-12-01' ~ 'Dilma',
        date > '2014-12-01' &
          date <= '2016-05-01' ~ 'Dilma',
        date > '2016-05-01' &
          date <= '2018-12-01' ~ 'Temer',
        date > '2018-12-01' &
          date <= '2022-12-01' ~ 'Bolsonaro',
        date > '2022-12-01' ~ 'Lula III')
    )


  # Verifica se a data "2025-01-01" já existe no data.frame

  if (!any(sub_indice$date == as.Date("2025-01-01"))) {

    janeiro_2025 <- base::subset(sub_indice,
                                 date == as.Date("2024-12-01")) %>%
                    dplyr::mutate(date = as.Date("2025-01-01"),
                                  value = 0)

    # Adicionar janeiro/2025

    sub_indice <- rbind(sub_indice, janeiro_2025)

  }


    sub_indice <- sub_indice %>%
    dplyr::group_by(name, gov) %>%
    dplyr::mutate(soma_movel = zoo::rollapply(value,
                                       width = 12,
                                       FUN = function(x) sum(x, na.rm = T),
                                       by.column = T,
                                       partial = T,
                                       fill = NA,
                                       align = 'right')) %>% ungroup()


  value <- sub_indice %>%
    dplyr::select(date, name, value) %>%
    dplyr::mutate(name = paste0(group_name, '_qtd_',name)) %>%
    tidyr::pivot_wider(names_from = name,
                       values_from = value)

  sub_indice = sub_indice %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = name, values_from = soma_movel) %>%
    na.omit() %>%
    mutate(across(where(is.numeric), ~.x/total),
           across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x))) %>%
    select(-total)


  colnames(sub_indice)[3:4] <- paste0(group_name,
                                      '_pct_',
                                      colnames(sub_indice)[3:4])

  sub_indice = merge(sub_indice, value, by = "date")

  return(sub_indice)
}

