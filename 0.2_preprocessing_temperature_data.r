if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('data.table')) install.packages('data.table'); library(data.table)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)


################## TEMPERATURA ################## 
Sys.setlocale("LC_ALL","English") # colocando o sistema em inglês para os meses ficarem em inglês


# criando dataframe de temperatura
df_temperatura1 <- 
read.csv("input/df_temperatura_1.csv") %>% 
  mutate(data = as.Date(data), # convertendo o tipo da coluna de string para data 
         mes_inter_int = strftime(data, "%Y-%m"),
         month_inter = strftime(data, "%b")) %>%  # cria uma coluna com o mess abreviado
  group_by(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  arrange(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  select(avg_temp_ar,avg_temp_max,avg_temp_min) %>% 
  distinct(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade, .keep_all = TRUE)

df_temperatura2 <- 
read.csv("input/df_temperatura_2.csv") %>% 
  mutate(data = as.Date(data), # convertendo o tipo da coluna de string para data 
         mes_inter_int = strftime(data, "%Y-%m"),
         month_inter = strftime(data, "%b")) %>%  # cria uma coluna com o mess abreviado
  group_by(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  arrange(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  select(avg_temp_ar,avg_temp_max,avg_temp_min) %>% 
  distinct(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade, .keep_all = TRUE)

df_temperatura3 <- 
read.csv("input/df_temperatura_3.csv") %>% 
  mutate(data = as.Date(data), # convertendo o tipo da coluna de string para data 
         mes_inter_int = strftime(data, "%Y-%m"),
         month_inter = strftime(data, "%b")) %>%  # cria uma coluna com o mess abreviado
  group_by(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  arrange(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  select(avg_temp_ar,avg_temp_max,avg_temp_min) %>% 
  distinct(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade, .keep_all = TRUE)

df_temperatura4 <- 
read.csv("input/df_temperatura_4.csv") %>% 
  mutate(data = as.Date(data), # convertendo o tipo da coluna de string para data 
         mes_inter_int = strftime(data, "%Y-%m"),
         month_inter = strftime(data, "%b")) %>%  # cria uma coluna com o mess abreviado
  group_by(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  arrange(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade) %>% 
  select(avg_temp_ar,avg_temp_max,avg_temp_min) %>% 
  distinct(ano, mes, mes_inter_int, month_inter, regiao, estado, cidade, .keep_all = TRUE)

df_temperature <- rbind(df_temperatura1, df_temperatura2, df_temperatura3, df_temperatura4) 


##### BRASIL ##### 
df_Brazil_temperature <- 
df_temperature %>% 
  group_by(ano, mes, mes_inter_int,month_inter) %>% 
  summarise(avg_temp_ar_new = mean(avg_temp_ar, na.rm = TRUE),
            avg_temp_max_new = mean(avg_temp_max, na.rm = TRUE),
            avg_temp_min_new =  mean(avg_temp_min, na.rm = TRUE),
            median_temp_ar = quantile(avg_temp_ar, probs = 0.5, na.rm = TRUE),
            median_temp_max = quantile(avg_temp_max, probs = 0.5, na.rm = TRUE),
            median_temp_min = quantile(avg_temp_min, probs = 0.5, na.rm = TRUE)) %>% 
  filter(ano >= 2012)

write.csv(df_Brazil_temperature, file = 'output/brazil_temperature_2012_2019.csv')


##### REGIAO ##### 
df_region_temperature <- 
df_temperature %>% 
  group_by(ano, mes, mes_inter_int, month_inter, regiao) %>% 
  summarise(avg_temp_ar_new = mean(avg_temp_ar, na.rm = TRUE),
            avg_temp_max_new = mean(avg_temp_max, na.rm = TRUE),
            avg_temp_min_new =  mean(avg_temp_min, na.rm = TRUE),
            median_temp_ar = quantile(avg_temp_ar, probs = 0.5, na.rm = TRUE),
            median_temp_max = quantile(avg_temp_max, probs = 0.5, na.rm = TRUE),
            median_temp_min = quantile(avg_temp_min, probs = 0.5, na.rm = TRUE)) %>% 
  filter(ano >= 2012)

write.csv(df_region_temperature, file = 'output/region_temperature_2012_2019.csv')


##### REGIAO METROPOLITANA ##### 
# criando dataframe de capitais e municipios de regiao metropolitana
df_info_regiao_metropolitana <- 
  read.csv("input/cidades_classificacoes.csv", sep = ";") %>%
  filter(classificacao %in% c("Capital", "Periferia 1")) %>% 
  mutate(city = toupper(gsub(" ", "", nome, fixed = TRUE)))

df_temperature <-
df_temperature %>% 
  mutate(city = paste(gsub(" ", "", cidade, fixed = TRUE), "-", estado, sep = ""))


# criando dataframe de pneumonia com apenas as capitais e municipios de regiao metropolitana
df_temperature_regiao_metropolitana <- left_join(x = df_info_regiao_metropolitana, y = df_temperature, by = c("city" = "city",
                                                                                                              "estado" = "estado"))
                                                                                                                
df_temperature_regiao_metropolitana <- 
df_temperature_regiao_metropolitana %>% 
  group_by(ano, mes, mes_inter_int, month_inter, regiao, estado, codigo_ibge_6dig, nome, cidade) %>% 
  summarise(median_temp_ar = quantile(avg_temp_ar, probs = 0.5, na.rm = TRUE)) %>% 
  filter(ano >= 2012)


write.csv(df_temperature_regiao_metropolitana, file = 'output/metropolitan_area_temperature_2012_2019.csv')


# juntando com hospitalizações
df_pneumonia_regiao_metropolitana <- read.csv("output/metropolitan_area_age_ajusted_sih_aih_pneumo_2012_2021.csv")

df_pneumonia_temperature_regiao_metropolitana <- left_join(df_pneumonia_regiao_metropolitana, df_temperature_regiao_metropolitana, by = c("ano_inter" = "ano",
                                                                                                                                          "month_inter" = "month_inter",
                                                                                                                                          "regiao" = "regiao",
                                                                                                                                          "estado" = "estado",
                                                                                                                                          "codigo_ibge_6dig" = "codigo_ibge_6dig"))

write.csv(df_pneumonia_temperature_regiao_metropolitana, file = 'output/metropolitan_area_temperature_2012_2019_pneum_2012_2021.csv')
