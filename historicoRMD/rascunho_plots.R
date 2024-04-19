#' @title Plots Igov
#' @author Luiz Paulo

base::rm(list = ls())
graphics.off()

update = "fevereiro_2024" # último update: 07/03/2024

paleta = list(primaria = "#4C94FF", secundaria = "#C4314B")

# Definindo ambiente de trabalho ===============================================

user = base::getwd() %>%
       stringr::str_extract("^((?:[^/]*/){3})") %>% print()

base::setwd(paste0(user, "4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/governabilidade/output/Data/historico/2024/", update))

# Dependências
# library(ggtext)

pacman::p_load(tidyverse,
               ggside,
               lubridate,
               RColorBrewer,
               scales)

# Definindo tema dos plotes ====================================================

thema = thema = theme(legend.position = "bottom",
                      ggside.panel.scale = 0.2,
                      panel.background = element_rect(fill = "transparent"),
                      #strip.text = element_text(face="bold", size=9),
                      strip.background = element_rect(fill = "#f5f5f5"),
                      legend.background = element_blank(),
                      axis.text.x = element_text(angle = 90,
                                                 hjust = 1,
                                                 size = 16),
                      axis.text.y = element_text(size = 14),
                      strip.text.x = element_text(size = 14),
                      strip.text.y = element_text(size = 17),
                      axis.title.y = element_text(size = 11L),
                      legend.text = element_text(color = "#0A2240",
                                                 size =  15),
                      legend.key = element_rect(fill = "transparent",
                                                colour = "transparent"),
                      legend.key.size = unit(0.1, 'cm'),
                      legend.margin = margin(-0.2, unit = "cm"),
                      rect = element_rect(fill = "transparent"),
                      plot.title.position = "plot",
                      plot.margin = ggplot2::margin(1,1,1,1,
                                                    unit = "lines"),
                      plot.caption.position =  "plot",
                      # plot.caption = element_markdown(color = "black",
                      #                                 size = 12),
                      plot.title = element_text(color = "black",
                                                size = 18,
                                                face = 'bold',
                                                margin = ggplot2::margin(b = 6)),
                      plot.subtitle = element_text(color = "#616060",
                                                   size = 14,
                                                   margin = ggplot2::margin(b = 6)),
                      panel.grid.major = element_line(color = "#e9e9e9"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.grid  = element_blank(),
                      axis.ticks = element_blank(),
                      plot.background = element_rect(fill = "transparent",
                                                     colour = NA))+
  theme(strip.text.x = element_text(margin = margin(.1, 1, .1, 1, "cm")))

# índice popularidade ==========================================================
# getwd()

indice_pop <- readxl::read_excel("indice_pop.xlsx") %>%
              dplyr::mutate(index = ifelse(date >= "2022-12-01", "Lula III", index)) %>%
              dplyr::mutate(date = as.Date(date),
                           index = factor(index,
                           levels = c('Lula', 'Dilma', 'Temer', 'Bolsonaro','Lula III')))


ggplot2::ggplot(indice_pop) +
         aes(x = date, y = indice_pop) +
         geom_line(size = 1.0) +
         facet_grid(~index,  scale = 'free_x', space="free_x")+
         labs(y = "", x = "")+
         ylim(0,1)+
         scale_y_continuous(labels = percent_format(),
                            limits = c(0,1))+
         scale_x_date(date_labels = "%Y-%m",
                      date_breaks = '1 year',
                      date_minor_breaks = '3 month',
                      expand = c(0.05,1))+thema

# ggplot2::ggplot(indice_pop) +
#          aes(x = date, y = indice_pop,
#              label = ifelse(date == max(date),
#                             scales::percent(indice_pop), "")) +
#          geom_line(size = 1.0) +
#          geom_text(hjust = 0.9,
#                    vjust = 1.0,
#                    size = 3.5) +
#          facet_grid(~index, scale = 'free_x') +
#          labs(y = "", x = "") +
#          ylim(0, 1) +
#          scale_y_continuous(labels = percent_format(),
#                             limits = c(0, 1)) +
#          scale_x_date(date_labels = "%Y-%m",
#                       date_minor_breaks = '10 month',
#                       expand = c(0.01, 1)) + thema

# getwd()

ggplot2::ggsave("plot_popularidade.png",
                width = 15,
                height = 7,
                dpi = 300,
                units = "in",
                device='png')

# Plote legislativo ============================================================

# indice_legislativo <- readxl::read_excel("indice_legis.xlsx")
#
# bd_composicao = indice_legislativo %>%
#                 dplyr::select(date,
#                               gov,
#                               indice_legislativo,
#                               pura_peso, emendada_peso) %>%
#                 mutate(pura_peso = ifelse(is.na(pura_peso), 0, pura_peso),
#                        emendada_peso = ifelse(is.na(emendada_peso), 0, emendada_peso),
#                        pura_composicao = pura_peso,
#                        emendada_composicao = emendada_peso) %>%
#                 rowwise() %>%
#                 mutate(indice_legislativo = sum(pura_peso+emendada_peso, na.rm = T)) %>%
#                 tidyr::pivot_longer(cols = indice_legislativo:emendada_composicao,names_to = "indices") %>%
#                 dplyr::mutate(type = case_when(indices == "pura_peso" ~ "Pura",
#                                                indices == "emendada_peso" ~ "Emendada",
#                                                indices == "indice_legislativo" ~ "Índice Legislativo",
#                                                indices == "pura_composicao" ~ "Índice",
#                                                indices == "emendada_composicao" ~ "Índice"),
#                               gov = factor(gov, levels = c("Lula I",
#                                                            "Lula II",
#                                                            "Dilma I",
#                                                            'Dilma II',
#                                                            "Temer",
#                                                            "Bolsonaro",
#                                                            "Lula III")),
#                              type = factor(type, levels = c("Índice Legislativo",
#                                                             "Índice",
#                                                             "Pura",
#                                                             "Emendada")),
#                             indices = factor(indices,
#                                              levels = c("indice_legislativo",
#                                                         "pura_peso",
#                                                         "emendada_peso",
#                                                         "pura_composicao",
#                                                         "emendada_composicao"),
#                                              labels = c("Índice Legislativo",
#                                                         "Pura",
#                                                         "Emendada",
#                                                         "Pura",
#                                                         "Emendada"))) %>%
#                           rename(value_indices = value) %>%
#                           mutate(value_indices = ifelse(
#     value_indices == 0.00000000, NA, value_indices))
#
#
# bd_mps = indice_legislativo %>%
#   dplyr::select(date,
#                 value_pura,
#                 value_emendada,
#                 value_sem_eficacia) %>%
#   tidyr::pivot_longer(-c(date),
#                       names_to = "mps") %>%
#   dplyr::full_join(
#     bd_composicao %>%
#       filter(type == "Índice Legislativo")) %>%
#   mutate(mps = factor(mps,
#                       levels = c("value_pura",
#                                  "value_emendada",
#                                  'value_sem_eficacia'),
#                       labels = c("Pura",
#                                  "Emendada",
#                                  'Sem Eficácia')), date = as.Date(date))
#
#
#
#
#
# bd_mps %>%
#   filter(mps != 'Sem Eficácia') %>%
#   #filter(date<'2023-01-01') %>%
#   ggplot2::ggplot()+
#   aes(x = date, y = value_indices)+
#   geom_line(size = 1.0)+
#   geom_xsidehistogram(mapping = aes(y = value,
#                                     fill = mps),
#                       stat = "identity",
#                       position = "identity")+
#   scale_ysidex_continuous(position = "bottom")+
#   ggside(collapse = "x",
#          x.pos = "bottom")+
#   scale_fill_manual(values = c(Pura = paleta$azul_4i,
#                                Emendada = paleta$laranja_4i))+
#   facet_grid(~gov,
#              space = "free_x",
#              scales = "free_x")+
#   labs(
#     y = "",
#     x = "",
#     fill = "",
#     caption = "Fonte: Congresso Nacional, organização metodológica 4i."
#   )+
#   scale_xsidey_continuous(minor_breaks = NULL)+
#   scale_y_continuous(labels = percent_format(),
#                      limits = c(0,1))+
#   scale_x_date(date_labels = "%Y-%m",
#                date_breaks = '1 year',
#                date_minor_breaks = '3 month',
#                expand = c(0.05,1))+ thema
#
# # Save Plot
#
# ggplot2::ggsave("plot_legis.png",
#                 width = 15,
#                 height = 7,
#                 dpi = 300,
#                 units = "in",
#                 device='png')
#




# Plote Judiciário =============================================================
