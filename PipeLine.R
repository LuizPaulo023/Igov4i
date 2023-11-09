#' @title {Igov4i}
#' @author Luiz Paulo Tavares

# Configurações de ambiente do usuário

rm(list = ls())
graphics.off()

# Dependências
# install.packages("devtools") // devtools devidamente instalado
# devtools::install_github("LuizPaulo023/Igov4i", dependencies = T)

pacman::p_load(tidyverse, Igov4i)

# Dependências de construção
# pacman::p_load(Igov4i, roxygen2, devtools)
# Buildar a documentação
# Compilar via Roxygen2
# devtools::document()

# Definindo diretório de trabalho ==============================================

user = base::getwd() %>%
       stringr::str_extract("^((?:[^/]*/){3})") %>% print()

path = base::setwd(paste0(user, "4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/input"))
# print(path)

# Importando base de dados =====================================================

pesq_pop = readxl::read_excel("DB_GOV.xlsx", sheet = "opiniao_publica")
dataset_legis = readxl::read_excel("DB_GOV.xlsx", sheet = "legislativo")
dataset_jud = readxl::read_excel("DB_GOV.xlsx", sheet = "judiciario")
depara = readxl::read_excel("DB_GOV.xlsx", sheet = "depara_gov")

# Calculando índices ===========================================================
# Índice de popularidade e legislativo podem ser calculados diretos
# Apenas importando a base de dados: DB_GOV
# Ou seja, não depedente de função auxiliar para limpar a base de dados

igov_popularidade = Igov4i::igov_pop(df = pesq_pop) # Calculando índice de popularidade
igov_legislativo = Igov4i::igov_legis(df = dataset_legis, peso = 2) # Calculando índice Legislativo

# Judiciário ===================================================================

# O judiciário, por sua vez:
# {aux_jud_clean} Remove ADIs que foram julgadas posteriormente e sem duplicações
# Entra como auxliar na função {jud_clean} para limpar a base de dados
# Depois o índice é finalmente calculado pela função {aux_jud} e {igov_jud}

judiciario_sem_dup = Igov4i::aux_jud_clean(dataset_jud, depara = depara)
jud_clean = Igov4i::jud_clean(db = dataset_jud, depara = depara)

# IGOV: judiciário -------------------------------------------------------------

judiciario = Igov4i::igov_jud(df = jud_clean)

# Calculando o estoque de ADI's ------------------------------------------------

estoque_adi <- Igov4i::jud_estoque_adi(judiciario_sem_dup, judiciario)

# Tempo médio julgamento: {qtd_julgada_mes_tempo_medio} ------------------------

tempo_medio <- Igov4i::jud_tempo_julgamento(judiciario_sem_dup)

# Output Igov4i Judiciario =====================================================

igov_judiciario = Igov4i::jud_output(estoque_semdec = estoque_adi, qtd_julgada_mes_tempo_medio = tempo_medio)

# Governabilidade4i ============================================================

indice_governabilidade = Igov4i::igov_geral(legislativo = igov_legislativo,
                                            judiciario = igov_judiciario,
                                            popularidade = igov_popularidade)

# Governabilidade41 Modificado

indice_gov_modificado = Igov4i::igov_mod(indice_gov = indice_governabilidade)

# ATENÇÃO ======================================================================
# SALVANDO ATUALIZAÇÕES
# DEFININDO DIRETÓRIO:

base::setwd(paste0(user, "4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data"))

writexl::write_xlsx(igov_legislativo,'indice_legis.xlsx')
writexl::write_xlsx(igov_judiciario,'indice_judiciario.xlsx')
writexl::write_xlsx(igov_popularidade,'indice_pop.xlsx')
writexl::write_xlsx(indice_governabilidade,'indice_governabilidade.xlsx')
writexl::write_xlsx(indice_gov_modificado,'indice_gov_modificado.xlsx')




