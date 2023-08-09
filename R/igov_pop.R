#' @title Índice de Popularidade
#' @name igov_pop
#' @author Gabriel Bellé & Luiz Paulo Tavares
#'
#' @description Função para calcular o índice de popularidade
#'
#' @param df Dataframe contendo as colunas \code{date}, \code{instituto}, \code{presidente}, \code{ótimo/bom}, \code{regular}, \code{ruim/péssimo}, \code{não sabe}.
#' @param peso
#'
#' @details O arquivo de input deve corresponder a base de dados já limpa, contendo colunas indicando as pesquisas de opinião referente aos institutos de pesquisa.
#' \code{date} - coluna de datas no formato YYYY/MM/DD;
#' \code{instituto} - referente ao instituto de pesquisa;
#' \code{presidente} - representa o presidente da República no período da pesquisa;
#' \code{ótimo/bom} - representa a % da população que considera o governo ótimo/bom;
#' \code{regular} - representa a % da população que considera o governo regular;
#' \code{ruim/péssimo} - representa a % da população que considera o governo ruim/péssimo;
#' \code{não sabe} - representa a % da população não sabe/não opinou na pesquisa referente.
#'
#' @return O retorno é um dataframe agrupado por mês e governo contendo as colunas:
#'
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


igov_pop <- function(df = as.data.frame()){

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

}


