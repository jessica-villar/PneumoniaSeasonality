if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('dplyr'); library(ggplot2)



################## PNEUMONIA COMUNITÁRIA ################## 
# lendo arquivo filtrado de pneumonia
df <- read.csv("output/filtered_sih_aih_env_2011_2020_pneucom.csv")
  
head(df)

# checando a proporção dos dados
as.data.frame(sort(prop.table(table(df$estado))*100, decreasing = TRUE))

as.data.frame(sort(prop.table(table(df$regiao))*100, decreasing = TRUE)) # quase não tem casos de pneumonia no Norte nem no Centro Oeste. Será que isso é devido a subnotificação? É uma limitação

as.data.frame(sort(prop.table(table(df$sexo))*100, decreasing = TRUE))

as.data.frame(sort(prop.table(table(df$raca_cor))*100, decreasing = TRUE)) # poucas pessoas se declaram negras ou os profissionais de saude declaram pouco as pessoas como negras

as.data.frame(sort(prop.table(table(df$los_hosp))*100, decreasing = TRUE))

as.data.frame(sort(prop.table(table(df$los_uti))*100, decreasing = TRUE))

as.data.frame(sort(prop.table(table(df$uti))*100, decreasing = TRUE))

as.data.frame(sort(prop.table(table(df$morte))*100, decreasing = TRUE)) # vale ver como a morte fica por grupo de idade, raça, sexo e região

as.data.frame(sort(prop.table(table(df$diag_princ_trim_upper))*100, decreasing = TRUE)) # vale entender o que cada um dos códigos significa

as.data.frame(sort(prop.table(table(df$ano_inter))*100, decreasing = TRUE)) # curioso que 2019 tem mais registros do que os demais anos; 2020 também, uma hipótese para isso é pandemia

as.data.frame(sort(prop.table(table(df$idade_grupo_who))*100, decreasing = TRUE)) # tem mais registros de pneumonia em pessoas mais velhas. Talvez isso ocorra porque as pessoas jovens se automedicam em casa, enquanto as mais velhas procuram ajudam médica


##
# agrupando dados por ano
df_group_year <-
df %>% 
  group_by(ano_inter, regiao) %>% 
  summarize(casos = n(),
            mortes = sum(morte),
            letalidade = mortes * 100/ casos)

head(df_group_year)

