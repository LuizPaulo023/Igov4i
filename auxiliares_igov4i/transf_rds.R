#' @title Transformar xlsx em rds
#' @author Luiz Paulo Tavares

base::rm(list = ls())
update = "marco_2024"
diretorio = setwd(paste0("C:/Users/LuizPauloTavaresGonç/4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data/historico/2024/", update))
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

base::saveRDS(indice_governabilidade, "indice_governabilidade.rds")
base::saveRDS(indice_judiciario, "indice_judiciario.rds")
base::saveRDS(indice_legis, "indice_legis.rds")

# índice modificado

indice_gov_modificado <- indice_gov_modificado %>% stats::na.omit()

base::saveRDS(indice_gov_modificado, "indice_gov_modificado.rds")

