if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2', dep = TRUE); library(ggplot2)
if (!require('fpp3')) install.packages('fpp3'); library(fpp3)
if (!require('tsibble')) install.packages('tsibble'); library(tsibble)
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('tsfeatures')) install.packages('tsfeatures'); library(tsfeatures)
if (!require('seastests')) install.packages('seastests'); library(seastests)
if (!require('stats')) install.packages('stats'); library(stats)


################## TEMPERATURA ################## 
##### BRASIL ##### 
# lendo arquivo temperatura
df_Brazil_temperature0 <- 
  read.csv("output/brazil_temperature_2012_2019.csv")%>% 
  mutate(groupby_column = 'Brazil')


df_Brazil_temperature <-
  df_Brazil_temperature0 %>%
  mutate(month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month)


# gráfico com temperaturas média, máxima e mínima
ggplot(df_Brazil_temperature, aes(x = month)) +
  geom_line(aes(y = median_temp_ar, colour = "Median air temperature (ºC)")) +
  geom_line(aes(y = median_temp_max, colour = "Maximum temperature")) +
  geom_line(aes(y = median_temp_min, colour = "Minimum temperature")) +
  labs(title = "Temperature",
       subtitle = "Brazil",
       y = "") +
  theme(legend.position = "bottom")


ggplot(df_Brazil_temperature, aes(x = month)) +
  geom_line(aes(y = median_temp_ar, colour = 'red')) +
  labs(x = "",
       y = "Median air temperature (ºC)") +
  scale_color_manual(values="blue") +
  theme_classic() +
  theme(legend.position = 'none')


# boxplot com temperatura média
df_Brazil_temperature$month_inter <- factor(df_Brazil_temperature$month_inter, #colocando os meses na ordem correta
                                            levels = month.abb)
ggplot() + 
  geom_boxplot(data = df_Brazil_temperature,
               aes(x = month_inter, y = median_temp_ar)) +
  labs(x = "",
       y = "Median air temperature (ºC)")


# plot anual de temperatura média
df_Brazil_temperature %>%
  gg_season(median_temp_ar, labels = "both") +
  labs(x = "",
       y = "AMedian air temperature (ºC)")


# tabela com percentis de temperatura
df_desc_temperature_Brazil_series <- 
  df_Brazil_temperature0 %>% 
  group_by(groupby_column) %>% 
  summarise(q25 = quantile(median_temp_ar, probs = 0.25),
            q50 = quantile(median_temp_ar, probs = 0.5),
            q75 = quantile(median_temp_ar, probs = 0.75)) %>%
  mutate(median_temp_ar_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
  select(groupby_column, median_temp_ar_p50_iqr)
View(df_desc_temperature_Brazil_series)


Brazil_temperature_series_list <- 
  df_Brazil_temperature0 %>%
  arrange(groupby_column, ano, month_inter) %>% 
  split(.$groupby_column) %>%
  map(~ts(pull(., median_temp_ar),
          start = c(2012, 01), end = c(2019, 12), # removendo anos da pneumonia
          frequency = 12))



# tabela com a forca da tendencia e da sazonalidade
df_strength_Brazil_temperature_series <- 
  Brazil_temperature_series_list %>% 
  imap(~bind_cols(groupby_column = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                                              trend, seasonal_strength, peak, trough))) %>% 
  bind_rows()
View(df_strength_Brazil_temperature_series)


df_temperature_Brazil_ts_stats <- inner_join(df_desc_temperature_Brazil_series, df_strength_Brazil_temperature_series, by = c("groupby_column" = "groupby_column"))
View(df_temperature_Brazil_ts_stats)


write_csv(df_temperature_Brazil_ts_stats, "output/df_temperature_Brazil_ts_stats.csv")


##### REGIAO #####
# lendo arquivo de temperatura
df_region <- read.csv("output/region_temperature_2012_2019.csv")

df_region2 <-
df_region %>%
  mutate(month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month,
             key = c(regiao))

# boxplot com temperatura media
df_region$month_inter <- factor(df_region$month_inter, #colocando os meses na ordem correta
                                levels = month.abb)
df_region$regiao <- factor(df_region$regiao, levels = c("N", "NE", "CO", "SE", "S"),
                           labels = c("North", "Northeast", "Center-West", "Southeast", "South"), ordered = T)
ggplot() + 
  geom_boxplot(data = df_region,
               aes(x = month_inter, y = median_temp_ar, fill = regiao)) +
  guides(fill = FALSE)  +
  facet_wrap(~regiao, scales = "free") +
  labs(x = "",
       y = "Median air temperature (ºC)")


# plot anual de temperatura media
gg_season(df_region2, median_temp_ar) +
  labs(x = "",
       y = "Median air temperature (ºC)")


# tabela com percentis de hospitalizacoes
df_desc_temperature_region_series <- 
df_region %>% 
  group_by(regiao) %>% 
  summarise(q25 = quantile(median_temp_ar, probs = 0.25),
            q50 = quantile(median_temp_ar, probs = 0.5),
            q75 = quantile(median_temp_ar, probs = 0.75)) %>%
  mutate(median_temp_ar_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
  select(regiao, median_temp_ar_p50_iqr)


region_temperature_series_list <- 
  df_region %>%
  arrange(regiao, ano, month_inter) %>% 
  split(.$regiao) %>%
  map(~ts(pull(., median_temp_ar),
          start = c(2012, 01), end = c(2019, 12), # removendo anos da pneumonia
          frequency = 12))


# tabela com a forca da tendencia e da sazonalidade
df_strength_all_region_temperature_series <- 
region_temperature_series_list %>% 
  imap(~bind_cols(regiao = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                                      trend, seasonal_strength, peak, trough))) %>% 
  bind_rows()
View(df_strength_all_region_temperature_series)


df_temperature_region_ts_stats <- inner_join(df_desc_temperature_region_series, df_strength_all_region_temperature_series, by = c("regiao" = "regiao"))


write_csv(df_temperature_region_ts_stats, "output/df_temperature_region_ts_stats.csv")
