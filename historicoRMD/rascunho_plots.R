thema = theme(legend.position = "bottom",
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



indice_pop_dantas <- readxl::read_excel(paste0(alt_path,
                                               'governabilidade/output/Data/',
                                               'indice_pop.xlsx')) %>%
  mutate(index = ifelse(date >= "2022-12-01", "Lula III", index)) %>%
  mutate(date = as.Date(date),
         index = factor(index,
                        levels = c('Lula', 'Dilma', 'Temer', 'Bolsonaro', 'Lula III')))


ggplot2::ggplot(indice_pop_dantas) +
  aes(x = date, y = indice_pop) +
  geom_line(size = 1.0) +
  facet_grid(~index,  scale = 'free_x', space="free_x")+
  labs(y = "", x = "")+
  ylim(0,1)+
  scale_y_continuous(labels = percent_format(),
                     limits = c(0,1))+
  scale_x_date(date_labels = "%Y-%m",
               date_breaks = '1 year',
               date_minor_breaks = '10 month',
               expand = c(0.01,1))+
  thema
  # theme(strip.text.x = element_text(size = 08))



ggplot2::ggsave(filename =  paste0(
  alt_path,
  "governabilidade/output/Plots/Opiniao_Publica/",
  "/plot_popularidade.png"),
  width = 15,
  height = 7,
  dpi = 300,
  units = "in",
  device='png')




