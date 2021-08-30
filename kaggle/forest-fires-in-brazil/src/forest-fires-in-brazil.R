
# Explicação codigo --------------------------------------------------------

# Neste código iremos analisar os focos de incendio de 1998 a 2017nas florestas 
#brasileiras. 
#Objetivo: Analisar e verificar quais os estados com mais focos de incêndio,
#quais aa região com mais foco de incêndio
#e posteriormente a criação de um dashboard interativo com shiny.



# Carregando bibliotecas --------------------------------------------------
# Dados encontrados em
# https://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif/resource/39308794-da81-4cab-89e7-1691c2f3893b
# https://www.kaggle.com/gustavomodelli/forest-fires-in-brazil
library(tidyverse)
library(sf)
library(geobr)


# Obtendo os dados --------------------------------------------------------

dados <- data.table::fread("http://homolog-dados.mma.gov.br/pt_BR/dataset/ffd9ab35-5719-4ec1-8d13-ae8f738bebc2/resource/949310f1-05bc-4f56-a63f-aef67aac6164/download/rf_incendiosflorestais_focoscalor_estados_1998-2017.csv",
                          quote="", encoding = 'Latin-1', check.names = TRUE)

estados <- read_state()
regioes <- read_region()



# Tema para os mapas ------------------------------------------------------


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      #panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# Limpeza de dados --------------------------------------------------------


glimpse(dados)

# Renomeando as variáveis, removendo as aspas e convertendo formatos
dados <- dados |> 
  select(-Período.) |> 
  rename(ANO = X.Ano,
         UF = Estado,
         MES = Mês,
         N_calor = Número,
         ) |> 
  mutate(
    ANO = str_remove(ANO,"\""),
    #DATA = str_remove(DATA,"\""),
    ANO = as.double(ANO),
    ANO = ordered(ANO),
    N_calor = as.integer(N_calor),
    UF = factor(janitor::make_clean_names(UF, case = "title", unique_sep = ".")),
    UF = str_replace(UF,"\\..*","")
    )



#write.csv2(dados, file = "fire_number.csv")

glimpse(dados)


# Estado por ano ----------------------------------------------------------

estados <- estados |>  
  mutate(
    name_state = factor(janitor::make_clean_names(name_state, case = "title", unique_sep = ".")),
    name_state = str_replace(name_state,"\\..*",""))


# dados |> 
#   right_join(estados, by = c("UF" = "name_state")) |> 
#   data.table::as.data.table() |>
#   select(-geom) |>
#   write.csv2(file = "fire_number.csv")
  

foco_calor_ano <- dados |> 
  select(-MES) |> 
  group_by(ANO, UF) |> 
  summarise(ANO, UF, N_calor) |> 
  mutate(N_calor = sum(N_calor)) |> 
  unique() |> 
  right_join(estados, by = c("UF" = "name_state")) |> 
  st_as_sf()




  

foco_calor_ano |> 
  ggplot(aes(reorder(UF, -N_calor),N_calor)) +
  geom_col(color = "blue", fill = "blue")+
  coord_flip()+
  labs(
    y = "Número de focos de incêndio",
    x = "Estados"
  )




foco_calor_ano |> 
  with_groups(abbrev_state , summarise, N_calor = sum(N_calor)) |> 
  unique() |> 
  ggplot() +
  geom_sf(data = estados) +
  geom_sf(mapping = aes(fill = N_calor), color = "black", show.legend = TRUE) + 
  #geom_sf_label(mapping = aes(label = abbrev_state))+
  scale_fill_viridis_c()+
  #scale_fill_gradientn(colours = c("darkblue", "blue", "yellow", "orange", "red"))+
  theme_void() + 
  #facet_wrap("ANO")+
  labs(title = "Quantidade de focos de incêndio de 1998 à 2017",
       fill ="Focos de calor") + 
  theme(plot.title = element_text(vjust = 3))


# Região ------------------------------------------------------------------

foco_calor_regiao <- dados |>
  right_join(estados, by = c("UF" = "name_state")) |> 
  select(-c("abbrev_state", "code_state", "UF", "MES", "geom", "code_region")) |> 
  group_by(ANO, name_region) |> 
  mutate(N_calor = sum(N_calor)) |> 
  unique() |> 
  right_join(regioes, by = c("name_region" = "name_region")) |> 
  st_as_sf()


glimpse(foco_calor_regiao)

# foco_calor_regiao |>
#   data.table::as.data.table() |>
#   select(-geom) |>
#   write_csv2(file = "regiao.csv")

#Grafico colunas da região com menos focos para a com mais.
foco_calor_regiao |>
  ggplot(aes(reorder(name_region, N_calor),N_calor)) +
  geom_col(color = "blue", fill = "blue")



#grafico por cores indicando a quantidade de focos de calor
foco_calor_regiao |> 
  with_groups(name_region, summarise, N_calor = sum((N_calor))) |> 
  unique() |> 
  ggplot() +
  geom_sf(data = estados) +
  geom_sf(aes(fill = N_calor), color = "black", show.legend = TRUE) + 
  geom_sf_label(mapping = aes(label = name_region))+
  scale_fill_gradient(low = "#D6EAF8", high = "#154360")+
  theme_map()+
  #facet_wrap("name_region")+
  labs(title = "Quantidade de focos de incêndio de 1998 à 2017",
       fill ="Focos de calor")



# Foco de incêndio por mês ------------------------------------------------


foco_calor_mes <- dados |> 
  group_by(UF, ANO, MES) |> 
  summarise(UF, ANO,
            N_calor= sum(N_calor),
            MES = ordered(MES, levels = c("Janeiro", "Fevereiro", "Março", "Abril",
                                          "Maio", "Junho", "Julho", "Agosto",
                                          "Setembro", "Outubro", "Novembro",
                                          "Dezembro"))) |> 
  right_join(estados, by = c("UF" = "name_state")) |> 
  unique() |> 
  st_as_sf() 
  


foco_calor_mes |> 
  arrange("MES") |> 
  filter(UF == "Sao Paulo" & ANO == "1999") |> 
  ggplot() +
  geom_sf(data = estados) +
  geom_sf(aes(fill = N_calor), color = "black", show.legend = TRUE) + 
  scale_fill_viridis_c()+
  theme_map() + 
  facet_wrap("MES")+
  labs(title = "")




# Gifs --------------------------------------------------------------------




library(gganimate)
library(patchwork)
library(ggrepel)
### install.packages("transformr")

anim <- foco_calor_ano |> 
  mutate(ANO = as.numeric(ANO)) |>
  with_groups(c(abbrev_state, ANO) , summarise, N_calor = sum(N_calor)) |> 
  unique() |> 
  ggplot() +
  geom_sf(data = estados) +
  geom_sf(mapping = aes(fill = N_calor), color = "black", show.legend = TRUE) + 
  scale_fill_viridis_c()+
  theme_map() + 
  labs(title = 'Ano: {frame_time}') +
  transition_time(ANO) +
  enter_fade()





animate(
  anim, nframes = 20, fps = 0.5,
  width = 800, height = 400
)
