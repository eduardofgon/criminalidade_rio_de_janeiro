########################################################################
#                      Carregar pacotes
########################################################################

library(sf)           # Para manusear dados espaciais
library(ggplot2)      # Para plotar
library(dplyr)        # Para manipulação de dados
library(viridis)      # Para escalas de cor
library(tidyr)        # Para manipulação de dados
library(lubridate)    # Para manipulação de datas
library(scales)       # Para formatar percentuais
library(stringr)      # Para manipulação de textos

########################################################################
#                      Carregar arquivos
########################################################################

# Shapefile das CISPs
cisp_shape <- st_read("dados/limite_cisp_072024/lm_cisp_bd.shp")

# Dados criminais CSV
dados <- read.csv2("dados/BaseDPEvolucaoMensalCisp.csv")

########################################################################
#              Manipular dados e produzir visualizações
########################################################################

######## Indicadores estratégicos
########################################

dados_1 <- dados %>%
       select(c("cisp", "mes", "ano", "mes_ano", "aisp", "risp", "munic",
                "mcirc", "regiao", "letalidade_violenta", "roubo_rua",
                "roubo_veiculo", "roubo_carga"))

##### Gráfico de linhas temporais

dados_2 <- dados_1 %>%
       group_by(mes_ano) %>%
       summarise(across(9:12, sum)) %>%
       mutate(mes_ano = ym(mes_ano)) %>%
       ungroup()

nomes_legendas <- c("roubo_rua" = "Roubos de rua",
                    "roubo_carga" = "Roubos de carga",
                    "roubo_veiculo" = "Roubos de veículos",
                    "letalidade_violenta" = "Letalidade violenta")

dados_2 <- dados_2 %>%
       pivot_longer(cols = -mes_ano,
                    names_to = "crimes",
                    values_to = "registros") %>%
       mutate(crimes = nomes_legendas[crimes])

graf_1 <- ggplot(dados_2, aes(x = mes_ano,
                              y = registros,
                              color = crimes)) +
       geom_line(linewidth = 1) +
       labs(title = "Evolução dos indicadores estratégicos",
            y = "Registros",
            caption = "Fonte: ISP",
            color = "Crimes") +
       theme_minimal() +
       theme(legend.position = "right",
             plot.title = element_text(hjust = 0.5),
             text = element_text(size = 20),
             axis.title.x = element_blank(),
             axis.title.y = element_text(color = "gray20"),
             plot.caption = element_text(color = "gray20",
                                         margin = margin(t = 15))) +
       scale_x_date(date_labels = "%Y",
                    date_breaks = "2 years") +
       scale_y_continuous(breaks = seq(1000, 15000, by = 1000)) +
       scale_color_viridis(discrete = TRUE,
                           option = "viridis") +
       coord_cartesian(ylim = c(0, 15000), expand = FALSE)

if (!dir.exists("graficos/series_temporais")) {
       dir.create("graficos/series_temporais", recursive = TRUE)}

ggsave("graficos/series_temporais/graf_temp_ind_est.png",
       plot = graf_1,
       width = 16,
       height = 8,
       units = "in",
       bg = "white")

##### Gráfico de barras início-fim

dados_3 <- dados_2 %>%
       filter(mes_ano == min(mes_ano) | mes_ano == max(mes_ano)) %>%
       pivot_wider(names_from = mes_ano, 
                   values_from = registros) %>%
       mutate(diferenca = .[[3]] - .[[2]],
              dif_perc = percent(diferenca / .[[2]])) %>%
       pivot_longer(cols = -c(crimes, diferenca, dif_perc),
                    names_to = "data",
                    values_to = "registros") %>%
       mutate(label_absoluto = if_else(data == max(data),
                                       paste0("Variação:\n",
                                              diferenca,
                                              " (",
                                              dif_perc,
                                              ")"),
                                       ""),
              data = ymd(data),
              cor_texto = case_when(crimes %in% c("Roubos de rua",
                                                  "Roubos de veículos") ~ "black",
                                    TRUE ~ "white"))

graf_2 <- ggplot(dados_3, aes(x = data,
                              y = registros,
                              fill = crimes)) +
       geom_bar(stat = "identity") +
       labs(title = "Comparação entre Janeiro de 2003 e Fevereiro de 2025",
            y = "Registros",
            caption = "Fonte: ISP") +
       geom_text(aes(label = label_absoluto, 
                     y = registros + 50),
                 vjust = 0,
                 size = 4,
                 color = "gray1",
                 lineheight = 0.8) +
       geom_text(aes(label = registros, 
                     y = registros/2),
                 color = dados_3$cor_texto,
                 vjust = 0.5) +
       scale_fill_viridis(discrete = TRUE) +
       scale_x_date(date_labels = "%b-%Y", 
                    breaks = unique(dados_3$data)) +
       facet_wrap(~crimes, nrow = 1) +
       theme_minimal() +
       theme(legend.position = "none",
             text = element_text(size = 17),
             axis.title.x = element_blank(),
             axis.title.y = element_text(color = "gray20"),
             plot.caption = element_text(color = "gray20",
                                         margin = margin(t = 15)),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_line(color = "gray85",
                                               linewidth = 0.25),
             panel.grid.minor.y = element_line(color = "gray85"),
             panel.border = element_rect(color = "gray50",
                                         fill = NA,
                                         linewidth = 0.5)) +
       scale_y_continuous(breaks = seq(0, 5500, by = 500)) +
       coord_cartesian(ylim = c(200, 5400))

ggsave("graficos/series_temporais/graf_barras_ind_est.png",
       plot = graf_2,
       width = 16,
       height = 8,
       units = "in",
       bg = "white")

##### Matrizes de calor por CISP e mês

dados_4 <- dados_1 %>%
       group_by(cisp, mes) %>%
       summarise(across(8:11, sum)) %>%
       ungroup() %>%
       mutate(mes_ext = month(mes, label = TRUE, abbr = FALSE))

levels(dados_4$mes_ext) <- str_to_title(levels(dados_4$mes_ext))

mat_cal.fun <- function(var_fill, var_ext) {
       ggplot(dados_4, aes(x = mes_ext,
                           y = cisp,
                           fill = !!sym(var_fill))) +
       geom_tile(color = "gray50") +
       scale_fill_viridis_c() +
       labs(title = paste(var_ext,
                          "por área de delegacia e por mês"),
            subtitle = "De Janeiro de 2003 e Fevereiro de 2025",
            y = "CISP",
            fill = var_ext,
            caption = "Fonte: ISP") +
       scale_y_continuous(breaks = c(1,
                                     seq(10, max(dados_4$cisp), by = 10),
                                     max(dados_4$cisp))) +
       theme_minimal() +
       theme(panel.grid = element_blank(),
             text = element_text(size = 25),
             axis.title.x = element_blank(),
             axis.title.y = element_text(color = "gray20"),
             plot.caption = element_text(color = "gray20",
                                         margin = margin(t = 15)),
             axis.text.x = element_text(angle = 30, hjust = 1)) +
       coord_cartesian(ylim = c(7, 168))}

graf_3 <- mat_cal.fun("letalidade_violenta", "Letalidade violenta")
graf_4 <- mat_cal.fun("roubo_carga", "Roubos de carga")
graf_5 <- mat_cal.fun("roubo_rua", "Roubos de rua")
graf_6 <- mat_cal.fun("roubo_veiculo", "Roubos de veículos")

if (!dir.exists("graficos/matrizes_de_calor")) {
       dir.create("graficos/matrizes_de_calor")}

ggsave("graficos/matrizes_de_calor/matriz_letalidade_violenta.png",
       plot = graf_3,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

ggsave("graficos/matrizes_de_calor/matriz_roubo_carga.png",
       plot = graf_4,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

ggsave("graficos/matrizes_de_calor/matriz_roubo_rua.png",
       plot = graf_5,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

ggsave("graficos/matrizes_de_calor/matriz_roubo_veiculo.png",
       plot = graf_6,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

##### Mapas por CISPs

dados_5 <- dados_1 %>%
       filter(ano != "2025") %>%
       group_by(ano, cisp) %>%
       summarise(across(8:11, sum)) %>%
       ungroup() %>%
       group_by(cisp) %>%
       summarise(across(2:5, mean)) %>%
       ungroup()

dados_5 <- left_join(cisp_shape, dados_5, by = "cisp")

mapa.fun <- function(var_fill, var_ext1, var_ext2) {
       ggplot(dados_5) +
       geom_sf(aes(fill = !!sym(var_fill)),
                   color = "black",
                   size = 0.1) +
       scale_fill_viridis_c() +
       labs(title = paste("Média anual de",
                          var_ext1,
                          "entre 2003 e 2024"),
            subtitle = "Números absolutos por CISP",
            fill = var_ext2,
            caption = "Fonte: ISP") +
       theme_minimal() +
       theme(text = element_text(size = 30),
             plot.caption = element_text(color = "gray20",
                                         margin = margin(t = 25)))}

graf_7 <- mapa.fun("letalidade_violenta",
                   "letalidade violenta",
                   "Letalidade violenta")
graf_8 <- mapa.fun("roubo_carga",
                   "roubos de carga",
                   "Roubos de carga")
graf_9 <- mapa.fun("roubo_rua",
                   "roubos de rua",
                   "Roubos de rua")
graf_10 <- mapa.fun("roubo_veiculo",
                    "roubos de veículos",
                    "Roubos de veículos")

if (!dir.exists("mapas/media_anual_cisp")) {
       dir.create("mapas/media_anual_cisp", recursive = TRUE)}

ggsave("mapas/media_anual_cisp/mapa_media_letalidade_violenta.png",
       plot = graf_7,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

ggsave("mapas/media_anual_cisp/mapa_media_roubo_carga.png",
       plot = graf_8,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

ggsave("mapas/media_anual_cisp/mapa_media_roubo_rua.png",
       plot = graf_9,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")

ggsave("mapas/media_anual_cisp/mapa_media_roubo_veiculo.png",
       plot = graf_10,
       width = 16,
       height = 12,
       units = "in",
       bg = "white")
