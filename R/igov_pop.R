#' @title Índice de Popularidade
#' @name igov_pop
#'
#' @author Gabriel Bellé & Luiz Paulo Tavares
#'
#' @description Função para calcular o índice de Opinião Pública, isto é, mensurar o desempenho do governo com relação a sociedade civil.
#'
#' @param df Dataframe contendo as colunas \code{date}, \code{instituto}, \code{presidente} e o grau de aprovação da sociedade civil:
#'
#' \code{ótimo/bom};
#'
#' \code{regular};
#'
#' \code{ruim/péssimo};
#'
#' \code{não sabe}.
#'
#' @details O arquivo de input deve corresponder a base de dados já limpa, contendo colunas indicando as pesquisas de opinião referente aos institutos de pesquisa.
#' @return O retorno é um dataframe agrupado por mês e governo contendo as colunas:
#'
#' \code{date};
#'
#' \code{media_bom};
#'
#' \code{media_regular};
#'
#' \code{indice_pop}.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' igov_pop <- Igov4i::igov_pop(df = pesq_pop)
#'
#' }
#'
#' @export


igov_pop <- function(df = as.data.frame()){

# Depara: número de institutos/volume de institutos

depara_gov = df %>%
             dplyr::select(date, instituto) %>%
             stats::na.omit() %>%
             dplyr::group_by(date) %>%
             dplyr::summarise(numero_institutos = n(),
                              institutos_distintos = n_distinct(instituto)) %>% dplyr::ungroup()


# Calculando o índice de popularidade ========================================

  mean_pop <- df %>%
              dplyr::group_by(date, index) %>%
              dplyr::summarise(media_bom = mean(bom, na.rm = T),
                               media_regular = mean(regular, na.rm=T),
                               date = date) %>%
              dplyr::ungroup() %>%
              dplyr::distinct(.keep_all = T) %>%
              dplyr::mutate(media_bom = ifelse(is.nan(media_bom), NA, media_bom),
                            media_regular = ifelse(is.nan(media_regular), NA, media_regular)) %>%
              dplyr::group_by(index) %>%
              dplyr::mutate(media_bom = zoo::na.approx(media_bom, na.rm = F),
                            media_regular = zoo::na.approx(media_regular, na.rm = F)) %>%
              tidyr::fill(media_bom) %>%
              tidyr::fill(media_regular) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(media_bom = media_bom/100,
                            media_regular = media_regular/100,
                            indice_pop = (media_bom + media_regular/2),
                            date = as.Date(date),
                            index = factor(index,
                            levels = c('Lula',
                                       'Dilma',
                                       'Temer',
                                       'Bolsonaro',
                                       'Lula III')))


  # Join com volume de institutos ==============================================

  mean_pop = dplyr::full_join(mean_pop, depara_gov, by = "date") %>%
             dplyr::filter(date > "2002-12-01")

  return(mean_pop)

}


