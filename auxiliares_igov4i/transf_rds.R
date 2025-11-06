#' @title Transformar xlsx em rds
#' @author Luiz Paulo Tavares

base::rm(list = ls())
update = "setembro"
diretorio = setwd(paste0("C:/Users/LuizPauloTavaresGonç/4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data/historico/2025/", update))
print(diretorio)

# Dependências =================================================================

pacman::p_load(tidyverse, readxl)

# Dados referente ao igov

arquivos <- list.files(pattern = "\\.xlsx$")# Obter a lista de arquivos .xlsx no diretório

if(!is.null(arquivos)){

  lista_data_frames <- list()

  for (arquivo in arquivos) {
    nome_data_frame <- gsub("\\.xlsx", "", arquivo)
    assign(nome_data_frame, read_excel(arquivo))

    # Armazenar o nome do data frame na lista
    lista_data_frames[[nome_data_frame]] <- get(nome_data_frame)
  }
}else{

  stop(paste0("Não encontrando arquivos no diretório: ", diretorio))

}

# Definir local

setwd("C:/Users/LuizPauloTavaresGonç/4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data/historico")
getwd()


# Índice de governabilidade geral \* ===========================================

indice_governabilidade = indice_governabilidade %>%
                         dplyr::mutate(across(-c(date, gov), ~ .*100))


base::saveRDS(indice_governabilidade, "indice_governabilidade.rds")

# Índice Judiciário - ADIs \* ==================================================

indice_judiciario = indice_judiciario %>%
                    dplyr::mutate(indice_judiciario = indice_judiciario*100)


base::saveRDS(indice_judiciario, "indice_judiciario.rds")

# Índice Legislativo - MPs \* ==================================================

indice_legis = indice_legis %>%
               dplyr::mutate(indice_legislativo = indice_legislativo*100)

base::saveRDS(indice_legis, "indice_legis.rds")

# Índice de popularidade \* ====================================================

indice_pop = indice_pop %>%
             dplyr::mutate(indice_pop = indice_pop*100)

base::saveRDS(indice_pop, "indice_pop.rds")

# índice modificado \* =========================================================

indice_gov_modificado <- indice_gov_modificado %>%
                         stats::na.omit() %>%
                         dplyr::mutate(across(-c(date, gov), ~. *100))

base::saveRDS(indice_gov_modificado, "indice_gov_modificado.rds")

