if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('writexl')) install.packages('dplyr'); library(writexl)
if (!require('data.table')) install.packages('data.table'); library(data.table)


################## PNEUMONIA COMUNITÁRIA ################## 
# criando dataframe de pneumonia
df <- read.csv("input/sih_aih_env_2011_2020_pneucom.csv")
head(df)

sort(unique(df$diag_princ_trim_upper))


df_filtered <- 
  df %>%
  filter(diag_princ_trim_upper %like% 'J10|J11|J12|J13|J14|J15|J16|J17|J18', # filtrando para os CIDs de J10 até J18
         ano_inter != 2020, # removendo a pandemia  
         idade_real_anos >= 2) %>% # removendo casos de pessoas com menos de 20 anos 
  mutate(mes_inter = months(as.Date(date_inter))) # criando coluna com mês


# salvando como csv o dataframe filtrado
write.csv(df_filtered, file = 'output/filtered_sih_aih_env_2011_2020_pneucom.csv')


################## TEMPERATURA ################## 
# criando dataframe de temperatura


