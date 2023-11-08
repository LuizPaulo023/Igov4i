#' @title Estoque de ADI's
#'

jud_estoque_adi <- function(judiciario_sem_dup, indice_judiciario){

  estoque = tibble::tibble()

  for (dt in seq(min(
    as.Date(lubridate::floor_date(judiciario_sem_dup$data_distribuicao,
                                  'months'))),
    max(
      as.Date(lubridate::floor_date(judiciario_sem_dup$data_distribuicao,
                                    'months'))),
    by = 'month')) {

    dt <- as.Date(dt, origin = '1970-01-01') %>%
      lubridate::ceiling_date('months') - lubridate::days(1)

    inday = judiciario_sem_dup %>%
            dplyr::filter(data_distribuicao <= dt)

    gov_inday <- inday %>%
                 utils::tail(1) %>%
                 purrr::pluck('gov_distribuicao')

    qtd_distribuido <- inday %>% base::nrow()

    qtd_decidido <- inday %>%
                    dplyr::filter(data_decisao <= dt) %>% base::nrow()

    # if(qtd_decidido == 0) {
    #   qtd_decidido_fav = 0
    #   qtd_decidido_contra = 0
    # } else {
    #   qtd_decidido_fav <- inday %>%
    #     #filter(gov_decisao == gov_inday) %>%
    #     filter(data_decisao <= dt) %>%
    #     filter(index == 1) %>%
    #     nrow()
    #
    #   qtd_decidido_contra <- inday %>%
    #     #filter(gov_decisao == gov_inday) %>%
    #     filter(data_decisao <= dt) %>%
    #     filter(index == -1) %>%
    #     nrow()
    # }
    #
    # qtd_decidido_idv <- inday %>%
    #   filter(data_decisao <= dt) %>%
    #   filter(coletiva == 0) %>%
    #   nrow()
    #
    # qtd_decidido_col <- inday %>%
    #   filter(data_decisao <= dt) %>%
    #   filter(coletiva == 1) %>%
    #   nrow()

    estoque = estoque %>%
              dplyr::bind_rows(tibble(date = lubridate::floor_date(dt, 'months'),
                               distribuido_tot = qtd_distribuido,
                               decidido_tot = qtd_decidido,
      ))

  }

  estoque_semdec <- estoque %>%
                    # group_by(month = lubridate::floor_date(date, 'month')) %>%
                    # filter(date == max(date)) %>%
                    dplyr::ungroup() %>%
                    #select(-date) %>%
                    #rename(date = month) %>%
                    dplyr::right_join(indice_judiciario) %>%
                    dplyr::arrange(date) %>%
                    dplyr::relocate(date, gov, indice_judiciario) %>%
                    tidyr::fill(names(estoque)) %>%
                    dplyr::mutate(
                      sem_decisao = distribuido_tot - decidido_tot
                    ) %>%
                    dplyr::relocate(sem_decisao, .after = 'indice_judiciario')


}
