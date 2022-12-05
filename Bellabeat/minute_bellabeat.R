library(tidyverse)
library(patchwork)


minute_calories_narrow <- "bellabeat_data/minuteCaloriesNarrow_merged.csv" |> 
  data.table::fread(sep = ',') |>
  janitor::clean_names() |> 
  mutate(activity_minute = lubridate::mdy_hms(activity_minute),
         activity_minute = lubridate::ceiling_date(activity_minute, "minute")
  )



minute_intensities_narrow <- data.table::fread("bellabeat_data/minuteIntensitiesNarrow_merged.csv",
                                               sep = ',') |>
  janitor::clean_names() |> 
  mutate(activity_minute = lubridate::mdy_hms(activity_minute),
         activity_minute = lubridate::ceiling_date(activity_minute, "minute")
  )


minute_mets_narrow <- data.table::fread("bellabeat_data/minuteMETsNarrow_merged.csv",
                                        sep = ',') |>
  janitor::clean_names() |> 
  mutate(activity_minute = lubridate::mdy_hms(activity_minute),
         activity_minute = lubridate::ceiling_date(activity_minute, "minute"),
         mets_value = me_ts
  ) |> 
  select(-me_ts)


minute_steps_narrow <- data.table::fread("bellabeat_data/minuteStepsNarrow_merged.csv",
                                         sep = ',') |>
  janitor::clean_names() |> 
  mutate(activity_minute = lubridate::mdy_hms(activity_minute),
         activity_minute = lubridate::ceiling_date(activity_minute, "minute")
  )


heartrate <- data.table::fread("bellabeat_data/heartrate_seconds_merged.csv",
                                      sep = ',') |>
  janitor::clean_names() |> 
  mutate(activity_minute = lubridate::mdy_hms(time),
         activity_minute = as.Date(activity_minute),
         heartrate = value 
         ) |> 
  select(-c(time, value)) |> 
  with_groups(c(id,
                activity_minute
                ),
              summarise,
              heartrate = mean(heartrate)
  ) |> 
  distinct()



daily_sleep <- data.table::fread("bellabeat_data/sleepDay_merged.csv",
                                 sep = ',') |>
  janitor::clean_names()|> 
  mutate(
    id = as.factor(id),
    date = lubridate::mdy_hms(sleep_day),
    date = as.Date(date)
  ) |> 
  select(-c(sleep_day))




bellabeat_data <- reduce(list(minute_calories_narrow,
                              minute_intensities_narrow,
                              minute_mets_narrow,
                              minute_steps_narrow
                              ),
                         left_join,
                         by = c("id", "activity_minute")) |> 
  mutate(
     id = as.factor(id),
    intensity = ordered(intensity),
    intensity = fct_recode(intensity,
                           Sedentário = "0",
                           Leve = "1",
                           Moderado = "2",
                           Intenso = "3"
    ),
    time = 1
  ) |>
  relocate(where(is.numeric), .after = last_col()) |> 
  mutate(date = as.Date(activity_minute),
         time = time) |> 
  select(-c(activity_minute)) |> 
  with_groups(
    c(id, date, intensity),
    summarise_all,
    sum
  ) |> 
  left_join(daily_sleep, by = c("id" = "id", "date" = "date")) |> 
  mutate(
    id = fct_relabel(id, ~ paste0('U', 1:33))
    )




summary(bellabeat_data)




bellabeat_data |> 
  group_by(id, intensity) |> 
  skimr::skim()



daily_activity |> 
  ggplot() +
  geom_point(aes(x = time, y = steps,
                 color = intensity, fill = intensity))+
  geom_smooth(aes(x = time, y = steps), method = "lm")+
  scale_color_viridis_d(aes(label = "Atividade")) +
  scale_fill_viridis_d(aes(label = "Atividade")) +
  scale_x_continuous(n.breaks = 9)+
  ggpubr::theme_pubclean() +
  facet_wrap(vars(intensity))+
  labs(
    x = "Tempo (h)",
    y = "Quantidade de passos",
    title = "Tempo por atividade vs total de passos",
    caption = "Período analisado de 12/04/2016 à 12/05/2016\n
    Fonte: kaggle"
  ) +
  theme(legend.position='bottom')



daily_activity |>
  ggplot() +
  geom_point(aes(x = time, y = calories,
                 color = intensity, fill = intensity))+
  geom_smooth(aes(x = time, y = calories), method = "lm")+
  scale_color_viridis_d(aes(label = "Atividade")) +
  scale_fill_viridis_d(aes(label = "Atividade")) +
  scale_x_continuous(n.breaks = 9)+
  ggpubr::theme_pubclean() +
  facet_wrap(vars(intensity))+
  labs(
    x = "Tempo (h)",
    y = "Calorias gastas",
    title = "Tempo de atividade vs total de calorias gastas",
    caption = "Período analisado de 12/04/2016 à 12/05/2016\n
    Fonte: kaggle"
  ) +
  theme(legend.position='bottom')



daily_activity |>
  ggplot() +
  geom_point(aes(x = time, y = mets_value,
                 color = intensity, fill = intensity))+
  geom_smooth(aes(x = time, y = mets_value), method = "lm")+
  scale_color_viridis_d(aes(label = "Atividade")) +
  scale_fill_viridis_d(aes(label = "Atividade")) +
  scale_x_continuous(n.breaks = 9)+
  ggpubr::theme_pubclean() +
  facet_wrap(vars(intensity))+
  labs(
    x = "Tempo (h)",
    y = "Mets",
    title = "Tempo de atividade vs Mets",
    caption = "Período analisado de 12/04/2016 à 12/05/2016\n
    Fonte: kaggle"
  ) +
  theme(legend.position='bottom')






plot5 <- daily_activity |> 
  mutate(day_of_the_week = lubridate::wday(date,
                                           label = TRUE)) |> 
  select(-date) |> 
  with_groups(
    c(id, day_of_the_week, intensity),
    summarise_all,
    sum
  ) |> 
  distinct() |>
  ggplot() +
  geom_col(aes(x = day_of_the_week, y = time,
               color = intensity, fill = intensity))+
  labs(
    x = "Dia da semana",
    y = "Tempo (h)",
  )



plot6 <- daily_activity |> 
  mutate(day_of_the_week = lubridate::wday(date,
                                           label = TRUE)) |> 
  select(-date) |> 
  with_groups(
    c(id, day_of_the_week, intensity),
    summarise_all,
    sum
  ) |> 
  distinct() |> 
  ggplot() +
  geom_col(aes(x = day_of_the_week, y = calories,
               color = intensity, fill = intensity))+
  labs(
    x = "Dia da semana",
    y = "Calorias gastas"
  )


patchwork2 <- (plot5 + plot6) + plot_layout(guides='collect') & scale_color_viridis_d(aes(label = "Atividade")) &
  scale_fill_viridis_d(aes(label = "Atividade")) &
  ggpubr::theme_pubclean() &
  theme(legend.position='bottom')

patchwork2 + plot_annotation(
  title = "Tempo de atividade e Calorias gastas por dia da semana",
  caption = "Período analisado de 12/04/2016 à 12/05/2016\n
    Fonte: kaggle"
)

