
# Carregamento bibliotecas ------------------------------------------------

library(tidyverse)

# Tratamento dos Dados diários --------------------------------------------------------------

#Lendo o arquivos com dados diários e gravando em variáveis.
#No processo está sendo limpo os nomes das variáveis para remoção de espaços e
#letras maiúsculas. Bem como alterando o tipo da coluna que contem as datas
#de todas as tabelas diárias

#Lendo dados de "dailyActivity_merged.csv" e limpando os nomes das variáveis
daily_activity <- "bellabeat_data/dailyActivity_merged.csv" |> 
  data.table::fread(sep = ',') |> 
  janitor::clean_names()
#transformando a coluna de "activity_date" de string para o tipo data.
#alterano o nome da coluna "activity_date" para "date"
daily_activity <- daily_activity |> 
  mutate(activity_date = lubridate::mdy(activity_date),
         date = lubridate::date(activity_date)) |> 
  select(-activity_date)


#Lendo dados de "sleepDay_merged.csv" e limpando os nomes das variáveis
daily_sleep <- "bellabeat_data/sleepDay_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()
#transformando a coluna de "sleep_day" de string para o tipo data.
#alterano o nome da coluna "sleep_day" para "date"
daily_sleep <- daily_sleep |> 
  mutate(sleep_day = lubridate::mdy_hms(sleep_day),
         date = lubridate::date(sleep_day)) |> 
  select(-sleep_day)


#Lendo dados de "heartrate_seconds_merged.csv" e limpando os nomes das variáveis
heartrate <- "bellabeat_data/heartrate_seconds_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()
#transformando a coluna de "time" de string para o tipo data.
#alterano o nome da coluna "time" para "date"
heartrate <- heartrate |> 
  mutate(time = lubridate::mdy_hms(time),
         date = lubridate::date(time)) |>
  select(-time) |> 
  with_groups(c(id, date), summarise, heartrate = mean(value))
#Na linha acima é feita a transformação dos valores
#em segundos para valores diários


#Lendo dados de "heartrate_seconds_merged.csv" e limpando os nomes das variáveis
weight_log <- "bellabeat_data/weightLogInfo_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()
#transformando a coluna de "date" de string para o tipo data.
weight_log <- weight_log |> 
  mutate(date = lubridate::mdy_hms(date),
         date = lubridate::date(date))
  

#Realizando a mesclagem das tabelas "dailyActivity_merged", "sleepDay_merged"
#e "heartrate_seconds_merged" em ma única tabela;
#Alterando a coluna "id" para o tipo fator;
#Criando a coluna "day_of_the_week" com os dias da semana;
#Alterando as colunas que medem tempo de minuto para hora;
#Alterando o nome dos usuários da coluna "id";
#Removendo as colunas em minutos;
#Reordenando as colunas da tabela resultante;
#Alterando o tipo da coluna "log_id" de inteiro para fator.
daily_data <- reduce(list(daily_activity,
                          heartrate,
                          daily_sleep,
                          weight_log),
                     left_join,
                     by = c("id", "date")) |> 
  mutate(id = as.factor(id),
         day_of_the_week = lubridate::wday(date,
                                           label = TRUE),
         total_time_asleep = total_minutes_asleep/60,
         total_time_in_bed = total_time_in_bed/60,
         sedentary_time = sedentary_minutes/60,
         lightly_active_time = lightly_active_minutes/60,
         fairly_active_time = fairly_active_minutes/60,
         very_active_time = very_active_minutes/60,
         id = fct_relabel(id, ~ paste0('U', 1:33))
         ) |>
  select(-c(total_minutes_asleep, sedentary_minutes, lightly_active_minutes,
            fairly_active_minutes, very_active_minutes)) |> 
  relocate(where(is.numeric), .after = last_col()) |> 
  relocate(is_manual_report, .before = log_id) |> 
  relocate(c(weight_kg:log_id), .after = last_col()) |> 
  mutate(log_id = as.factor(log_id))


#Verificando o Número de usuários diferentes e seus nomes.
daily_data |> 
  distinct(id)

#Salvando dados em um arquivo
#openxlsx::write.xlsx(daily_data, "bellabeat_data/daily_data.xlsx")
#write_csv(daily_data, "bellabeat_data/daily_data.csv")


# Análise dados diários ---------------------------------------------------
glimpse(daily_data)



#Resumo da tabela resultante 
daily_data |> 
  skimr::skim()

#Distância e passos#

# Total de passos vs Total de calorias
daily_data |> 
  with_groups(id, 
              summarise,
              total_distance = mean(total_distance),
              calories = mean(calories)) |> 
  ggplot() +
  geom_smooth(aes(x = total_distance, y = calories), method = "lm")+
  geom_point(aes(x = total_distance, y = calories))+
  ggrepel::geom_label_repel(aes(x = total_distance, y = calories, label = id))+
  ggthemes::theme_clean()+
  labs(
    x = "Distância total",
    y = "Total de calorias",
    title = "Distândia total vs total de calorias gastas por dia"
  )

# Total de distância vs Total de calorias
daily_data |> 
  with_groups(id, 
              summarise,
              total_distance = mean(total_distance),
              calories = mean(calories)) |> 
  ggplot() +
  geom_smooth(aes(x = total_distance, y = calories), method = "lm")+
  geom_point(aes(x = total_distance, y = calories))+
  ggthemes::theme_clean()+
  labs(
    x = "Total de distância",
    y = "Total de calorias",
    title = "Total de passos vs total de calorias gastas por dia"
  )

daily_data |> 
  select(id, total_distance, bmi) |> 
  drop_na() |> 
  with_groups(id,
              summarise,
              total_distance = mean(total_distance),
              bmi = mean(bmi)
              ) |>
  ggplot() +
  geom_point(aes(x = bmi, y = total_distance, size = bmi, color = id))+
  ggthemes::theme_clean()+
  labs(
    x = "Indice de Massa corporal",
    y = "Total de distância",
    title = "Indice de massa corporal vs distância percorrida",
  ) +
  theme(legend.position = "none")



# Sono
daily_data |> 
  select(id, total_distance, total_time_in_bed) |> 
  drop_na() |> 
  group_by(id) |> 
  summarise(
    total_distance = mean(total_distance),
    sleep_time = ordered(case_when(
      total_time_in_bed < 7 ~ "Menos_que_7h",
      total_time_in_bed >= 7 & total_time_in_bed <= 10 ~ "Entre_7_10h",
      total_time_in_bed > 10 ~ "Mais_que_10h"
    ), levels = c("Menos_que_7h", "Entre_7_10h", "Mais_que_10h"))
  ) |> 
  ggplot() +
  geom_col(aes(x = sleep_time, y = total_distance),
           color = "blue", fill = "blue")+
  ggthemes::theme_clean()+
  labs(
    x = "Tempo deitado",
    y = "Distância Total",
    title = "Total de passos vs total de calorias gastas por dia"
  )+
  theme(
    
  )

daily_data |> 
  mutate(sedentary_minutes = sedentary_minutes/60,
         total_minutes_asleep = total_minutes_asleep/60) |> 
  ggplot() +
  geom_smooth(aes(x = sedentary_minutes, y = total_minutes_asleep),
              method = "lm")+
  geom_point(aes(x = sedentary_minutes, y = total_minutes_asleep))+
  scale_x_continuous(n.breaks = 21, limits = c(1,21))+
  scale_y_continuous(n.breaks = 14, limits = c(1,14))+
  ggthemes::theme_clean()+
  labs(
    x = "Tempo sedentário",
    y = "Tempo dormindo",
    title = "Sedentarismo vs horas dormidas "
  )

daily_data |> 
  mutate(sedentary_minutes = sedentary_minutes/60,
         total_minutes_asleep = total_minutes_asleep/60) |> 
  ggplot() +
  geom_smooth(aes(x = sedentary_minutes, y = total_distance),
              method = "lm")+
  geom_point(aes(x = sedentary_minutes, y = total_distance))+
  #scale_x_continuous(n.breaks = 21, limits = c(1,21))+
  #scale_y_continuous(n.breaks = 14, limits = c(1,14))+
  ggthemes::theme_clean()+
  labs(
    x = "Tempo sedentário",
    y = "Distância total",
    title = "Sedentarismo vs Distância percorrida"
  )

daily_data |> 
  mutate(sedentary_minutes = sedentary_minutes/60,
         total_minutes_asleep = total_minutes_asleep/60) |> 
  ggplot() +
  geom_smooth(aes(x = total_minutes_asleep, y = total_distance),
              method = "lm")+
  geom_point(aes(x = total_minutes_asleep, y = total_distance))+
  #scale_x_continuous(n.breaks = 21, limits = c(1,21))+
  #scale_y_continuous(n.breaks = 14, limits = c(1,14))+
  ggthemes::theme_clean()+
  labs(
    x = "Tempo dormindo",
    y = "Distância total",
    title = "Qantidade de horas dormidas vs Distância percorrida"
  )

























# Tratamento dos Dados por hora --------------------------------------------------------------

hourly_calories <- "bellabeat_data/hourlyCalories_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()


hourly_intensities <- "bellabeat_data/hourlyIntensities_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()


hourly_steps <- "bellabeat_data/hourlySteps_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()



horly_data <- hourly_calories |> 
  left_join(hourly_intensities, by = c("id" = "id",
                                       "activity_hour" = "activity_hour")) |> 
  left_join(hourly_steps, by = c("id" = "id",
                                 "activity_hour" = "activity_hour"))
#Join Activity, sleep and heatrate data


























# Tratamento dos Dados por minto ------------------------------------------


minute_calories_narrow <- "bellabeat_data/minuteCaloriesNarrow_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()


minute_calories_wide <- "bellabeat_data/minuteCaloriesWide_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names()


minute_intensities_narrow <- data.table::fread("bellabeat_data/minuteIntensitiesNarrow_merged.csv",
                     sep = ',') |>
  janitor::clean_names()

minute_intensities_wide <- data.table::fread("bellabeat_data/minuteIntensitiesWide_merged.csv",
                     sep = ',') |>
  janitor::clean_names()

minute_mets_narrow <- data.table::fread("bellabeat_data/minuteMETsNarrow_merged.csv",
                     sep = ',') |>
  janitor::clean_names()

minute_sleep <- data.table::fread("bellabeat_data/minuteSleep_merged.csv",
                     sep = ',') |>
  janitor::clean_names()

minute_steps_narrow <- data.table::fread("bellabeat_data/minuteStepsNarrow_merged.csv",
                     sep = ',') |>
  janitor::clean_names()

minute_steps_wide <- data.table::fread("bellabeat_data/minuteStepsWide_merged.csv",
                     sep = ',') |>
  janitor::clean_names()


# minte_data_narrow <- minute_calories_narrow |>
#   left_join(minute_intensities_narrow, by = c("id" = "id", "activity_minute" = "activity_minute")) |>
#   left_join(minute_mets_narrow, by = c("id" = "id", "activity_minute" = "activity_minute")) |>
#   left_join(minute_steps_narrow, by = c("id" = "id", "activity_minute" = "activity_minute"))

minte_data_narrow <- plyr::join_all(list(minute_calories_narrow,
                                     minute_intensities_narrow,
                                     minute_mets_narrow,
                                     minute_steps_narrow),
                                by=c("id" = "id", "activity_minute" = "activity_minute"), type='left')  


minte_data_wide <- plyr::join_all(list(minute_calories_wide,
                                       minute_intensities_wide,
                                       minute_steps_wide),
                                  by=c("id" = "id", "activity_hour" = "activity_hour"), type='left')






