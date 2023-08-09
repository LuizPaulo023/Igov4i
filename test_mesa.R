#' @title Testando as funções
#' @author Luiz Paulo Tavares Gonçalves

rm(list = ls())
graphics.off()
#setwd("C:/Users/LuizPauloTavaresGonç/4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/input")
# getwd()

# import package
pacman::p_load(Igov4i)
#library(tidyverse)

# Testando o índice de popularidade --------------------------------------------
pesq_pop = readxl::read_excel("DB_GOV.xlsx", sheet = "opiniao_publica")

test_igov_pop <- Igov::igov_pop(df = pesq_pop)

# Testando o índice Legislativo ------------------------------------------------
dataset_legis = readxl::read_excel("DB_GOV.xlsx", sheet = "legislativo")

test_igov_legis <- Igov::igov_legis(df = dataset_legis, peso = 2)





















