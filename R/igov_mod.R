#'
#' @author Luiz Paulo Tavares

igov_mod <- function(indice_gov){

indice_mod <- indice_gov %>%
              dplyr::group_by(gov) %>%
              tidyr::fill(Legislativo, .direction = "down") %>%
              tidyr::fill(Legislativo, .direction = "up") %>%
              tidyr::fill(Judiciario, .direction = "down") %>%
              tidyr::fill(Judiciario, .direction = "up") %>%
              tidyr::fill(Popularidade, .direction = "down") %>%
              tidyr::fill(Popularidade, .direction = "up") %>%
              tidyr::fill(Governabilidade, .direction = "down") %>%
              tidyr::fill(Governabilidade, .direction = "up") %>%
              dplyr::ungroup()

}
