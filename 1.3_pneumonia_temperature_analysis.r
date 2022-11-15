if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2', dep = TRUE); library(ggplot2)
if (!require('fpp3')) install.packages('fpp3'); library(fpp3)
if (!require('tsibble')) install.packages('tsibble'); library(tsibble)
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('tsfeatures')) install.packages('tsfeatures'); library(tsfeatures)
if (!require('seastests')) install.packages('seastests'); library(seastests)
if (!require('stats')) install.packages('stats'); library(stats)


################## CORRECAO ENTRE PNEUMONIA E TEMPERATURA ################## 
##### BRASIL ##### 
df_Brazil <- 
  read.csv("output/brazil_age_ajusted_sih_aih_pneumo_2012_2021.csv")%>% 
  mutate(groupby_column = 'Brazil',
         month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month)%>%
  arrange(groupby_column, ano_inter, month_inter)


df_Brazil_temperature <- 
  read.csv("output/brazil_temperature_2012_2019.csv")%>% 
  mutate(groupby_column = 'Brazil',
         month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month)%>%
  arrange(groupby_column, ano, month_inter)


df_Brazil_pneumonia_temp <- 
df_Brazil %>% 
  select(groupby_column, ano_inter, month, age_ajusted_hospitalizations) %>% 
  inner_join(df_Brazil_temperature %>% 
               select(groupby_column, ano, month, median_temp_ar),
             by = c("groupby_column" = "groupby_column",
                    "ano_inter" = "ano",
                    "month" = "month"))


df_Brazil_pneumonia_temp %>%
  split(.$groupby_column) %>% 
  map(~cor.test(.$age_ajusted_hospitalizations, .$median_temp_ar, method = "spearman"))


##### REGIAO ##### 
df_region <- 
read.csv("output/region_age_ajusted_sih_aih_pneumo_2012_2021.csv") %>%
  mutate(month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month,
             key = c(regiao))%>%
  arrange(regiao, ano_inter, month_inter)


df_region_temperature <- 
read.csv("output/region_temperature_2012_2019.csv") %>%
  mutate(month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month,
             key = c(regiao))%>%
  arrange(regiao, ano, month_inter)


df_region_pneumonia_temp <- 
df_region %>% 
  select(regiao, ano_inter, month, age_ajusted_hospitalizations) %>% 
  inner_join(df_region_temperature %>% 
               select(regiao, ano, month, median_temp_ar),
             by = c("regiao" = "regiao",
                    "ano_inter" = "ano",
                    "month" = "month"))


df_region_pneumonia_temp %>%
  split(.$regiao) %>% 
  map(~cor.test(.$age_ajusted_hospitalizations, .$median_temp_ar, method = "spearman"))
