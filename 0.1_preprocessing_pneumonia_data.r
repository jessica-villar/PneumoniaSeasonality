if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('data.table')) install.packages('data.table'); library(data.table)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)



################## PNEUMONIA COMUNITÁRIA ################## 
# criando dataframe de capitais
df_info_capital <- 
read.csv("input/cidades_classificacoes.csv", sep = ";") %>%
  filter(classificacao == "Capital")
  # filter(classificacao %in% c("Capital", "Periferia 1"))

# criando dataframe de pneumonia
df_pneumonia <- read.csv("input/sih_aih_pneumo_2011_2021.csv")
head(df_pneumonia)

sort(unique(df_pneumonia$diag_princ_trim_upper))

Sys.setlocale("LC_ALL","English") # colocando o sistema em inglês para os meses ficarem em inglês

df_pneumonia_filtered <- 
df_pneumonia %>%
  filter(diag_princ_trim_upper %like% 'J10|J11|J12|J13|J14|J15|J16|J17|J18', # filtrando para os CIDs de J10 até J18
         ano_inter != 2011, # removendo 2011 para termos 10 anos de análise
         idade_real_anos >= 20) %>% # removendo casos de pessoas com menos de 20 anos 
  mutate(date_inter = as.Date(date_inter), # convertendo o tipo da coluna de string para data 
         mes_inter = months(as.Date(date_inter)), # criando coluna com mês
         mes_inter_int = strftime(date_inter, "%Y-%m"), # criando uma coluna numérica com o mês
         month_inter = strftime(date_inter, "%b"), # cria uma coluna com o mês abreviado
         idade_grupo_who2 = case_when(idade_grupo_who %in% c("20-24","25-29") ~ "20-29",
                                      idade_grupo_who %in% c("30-34","35-39") ~ "30-39",
                                      idade_grupo_who %in% c("40-44","45-49") ~ "40-49",
                                      idade_grupo_who %in% c("50-54","55-59") ~ "50-59",
                                      idade_grupo_who %in% c("60-64","65-69") ~ "60-69",
                                      idade_grupo_who %in% c("70-74","75-79") ~ "70-79",
                                      TRUE ~ "mais de 80 anos")) 


df_all_cases_filtered <- 52813383 #peguei esse número do trabalho de envelhecimento da Rachel

count(df_pneumonia_filtered)
count(df_pneumonia_filtered) / df_all_cases_filtered

##### BRASIL ##### 
# criando dataframe Brasil com dados por idade
df_filtered_Brazil_age <- 
df_pneumonia_filtered %>%
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, idade_grupo_who2) %>% 
  summarize(casos = n(),
            mortes = sum(morte))


# criando dataframe com proporção de população por grupo de idade segundo a OMS
df_pop <- 
read.csv("input/who_pop_std_rates.csv") %>%
  filter(!(age_group %in% c("0-4", "5-9", "10-14", "15-19"))) %>% 
  mutate(idade_grupo_who2 = case_when(age_group %in% c("20-24","25-29") ~ "20-29",
                                      age_group %in% c("30-34","35-39") ~ "30-39",
                                      age_group %in% c("40-44","45-49") ~ "40-49",
                                      age_group %in% c("50-54","55-59") ~ "50-59",
                                      age_group %in% c("60-64","65-69") ~ "60-69",
                                      age_group %in% c("70-74","75-79") ~ "70-79",
                                      TRUE ~ "mais de 80 anos")) %>%
  group_by(idade_grupo_who2) %>%
  summarize(pop_rate_perc2 = sum(pop_rate_perc))


# criando dataframe com projeção do total da população brasileira segundo o IGBE

df_pop_Brazil <- 
  # read.csv("https://raw.githubusercontent.com/lslbastos/ibge_population_projection/main/output/data_pop_ibge_proj_2010-2016.csv") %>%
read.csv("input/data_pop_ibge_proj_2010-2016.csv") %>%
  mutate(age = as.numeric(age)) %>% 
  filter(region == "Brazil",
         sex == "all",
         age >= 20) %>%
  mutate(idade_grupo_who2 = case_when(age <= 29 ~ "20-29",
                                      age <= 39 ~ "30-39",
                                      age <= 49 ~ "40-49",
                                      age <= 59 ~ "50-59",
                                      age <= 69 ~ "60-69",
                                      age <= 79 ~ "70-79",
                                      TRUE ~ "mais de 80 anos")) %>%
  group_by(year, idade_grupo_who2) %>% 
  summarize(population = sum(population))


# juntando dataframe de pneumonia com dataframe de população brasileira
df_cases_pop_Brazil <- left_join(x = df_filtered_Brazil_age, y = df_pop_Brazil, by = c("ano_inter" = "year",
                                                                                       "idade_grupo_who2" = "idade_grupo_who2"))

# junto dataframe de pneumonia e população com população da OMS
df <- left_join(x = df_cases_pop_Brazil, y = df_pop, by = c("idade_grupo_who2" = "idade_grupo_who2"))


df$taxa_casos <- df$casos/df$population
df$casos_esperados <- df$taxa_casos * df$pop_rate_perc2

df$taxa_mortes <- df$mortes/df$population
df$mortes_esperadas <- df$taxa_mortes * df$pop_rate_perc2

df$letalidade_esperada <- df$mortes_esperadas/df$casos_esperados

df_ajuste_idade <-
  df %>% group_by(ano_inter, mes_inter, mes_inter_int, month_inter) %>% 
  summarize(age_ajusted_hospitalizations = sum(casos_esperados) * 100000/sum(pop_rate_perc2), # chamamos de age ajusted
            hospitalizations = sum(casos) * 100000/sum(population), # chamamos de crude
            age_ajusted_deaths = sum(mortes_esperadas) * 100000/sum(pop_rate_perc2), 
            deaths = sum(mortes) * 100000/sum(population),
            lethality = deaths / hospitalizations,
            age_ajusted_lethality = sum(mortes_esperadas)/sum(casos_esperados)) 


# salvando como csv o dataframe filtrado
write.csv(df_ajuste_idade, file = 'output/brazil_age_ajusted_sih_aih_pneumo_2012_2021.csv')


##### ESTADOS ##### 
# criando dataframe por estados com dados por idade
df_filtered_states_age <- 
  df_pneumonia_filtered %>%
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, regiao, estado, idade_grupo_who2) %>% 
  summarize(casos = n(),
            mortes = sum(morte))


# criando dataframe com projeção do total da população por estado segundo o IGBE
df_pop_states <- 
read.csv("input/data_pop_ibge_proj_2010-2016.csv") %>%  mutate(age = as.numeric(age)) %>% 
  filter(region != "Brazil",
         sex == "all",
         age >= 20) %>%
  mutate(idade_grupo_who2 = case_when(age <= 29 ~ "20-29",
                                      age <= 39 ~ "30-39",
                                      age <= 49 ~ "40-49",
                                      age <= 59 ~ "50-59",
                                      age <= 69 ~ "60-69",
                                      age <= 79 ~ "70-79",
                                      TRUE ~ "mais de 80 anos")) %>%
  group_by(year, uf, idade_grupo_who2) %>% 
  summarize(population = sum(population))


# juntando dataframe de pneumonia com dataframe de população brasileira
df_cases_pop_states <- left_join(x = df_filtered_states_age, y = df_pop_states, by = c("ano_inter" = "year",
                                                                                       "idade_grupo_who2" = "idade_grupo_who2",
                                                                                       "estado" = "uf"))

# junto dataframe de pneumonia e população com população da OMS
df_states <- left_join(x = df_cases_pop_states, y = df_pop, by = c("idade_grupo_who2" = "idade_grupo_who2"))


df_states$taxa_casos <- df_states$casos/df_states$population
df_states$casos_esperados <- df_states$taxa_casos * df_states$pop_rate_perc2

df_states$taxa_mortes <- df_states$mortes/df_states$population
df_states$mortes_esperadas <- df_states$taxa_mortes * df_states$pop_rate_perc2

df_ajuste_idade_states <-
df_states %>% 
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, regiao, estado) %>% 
  summarize(age_ajusted_hospitalizations = sum(casos_esperados) * 100000/sum(pop_rate_perc2), # chamamos de age ajusted
            hospitalizations = sum(casos) * 100000/sum(population), # chamamos de crude
            age_ajusted_deaths = sum(mortes_esperadas) * 100000/sum(pop_rate_perc2), 
            deaths = sum(mortes) * 100000/sum(population),
            lethality = deaths / hospitalizations,
            age_ajusted_lethality = sum(mortes_esperadas)/sum(casos_esperados)) 


# salvando como csv o dataframe filtrado
write.csv(df_ajuste_idade_states, file = 'output/states_age_ajusted_sih_aih_pneumo_2012_2021.csv')


##### REGIAO ##### 
# juntando dataframe de pneumonia com dataframe de população brasileira
df_cases_pop_region <- 
df_cases_pop_states %>% 
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, regiao, idade_grupo_who2) %>% 
  summarize(casos = sum(casos),
            mortes = sum(mortes),
            population = sum(population))

# junto dataframe de pneumonia e população com população da OMS
df_region <- left_join(x = df_cases_pop_region, y = df_pop, by = c("idade_grupo_who2" = "idade_grupo_who2"))


df_region$taxa_casos <- df_region$casos/df_region$population
df_region$casos_esperados <- df_region$taxa_casos * df_region$pop_rate_perc2

df_region$taxa_mortes <- df_region$mortes/df_region$population
df_region$mortes_esperadas <- df_region$taxa_mortes * df_region$pop_rate_perc2

df_ajuste_idade_region <-
  df_region %>% 
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, regiao) %>% 
  summarize(age_ajusted_hospitalizations = sum(casos_esperados) * 100000/sum(pop_rate_perc2), # chamamos de age ajusted
            hospitalizations = sum(casos) * 100000/sum(population), # chamamos de crude
            age_ajusted_deaths = sum(mortes_esperadas) * 100000/sum(pop_rate_perc2), 
            deaths = sum(mortes) * 100000/sum(population),
            lethality = deaths / hospitalizations,
            age_ajusted_lethality = sum(mortes_esperadas)/sum(casos_esperados)) 


# salvando como csv o dataframe filtrado
write.csv(df_ajuste_idade_region, file = 'output/region_age_ajusted_sih_aih_pneumo_2012_2021.csv')


##### CAPITAL ##### 
# criando dataframe de pneumonia com apenas as capitais
df_filtered_capital <- left_join(x = df_info_capital, y = df_pneumonia_filtered, by = c("codigo_ibge_6dig" = "munic_res",
                                                                                        "estado" = "estado"))


# criando dataframe de capitais com dados por idade
df_filtered_capital_age <- 
  df_filtered_capital %>%
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, idade_grupo_who2, regiao, estado, codigo_ibge_6dig, nome) %>% 
  summarize(casos = n(),
            mortes = sum(morte))


# criando dataframe com projeção do total da população por capital segundo o IGBE
df_pop_capital <- 
read.csv("input/populacao_municipio.csv") %>% 
  filter(capital == 1) %>%  # pegando dados apenas das capitais
  select(-c("id_estado","estado","regiao","no_regiao","macrorregiao","no_macro","pop0a4","pop5a9","pop10a14","pop15a19")) %>% 
  pivot_longer(c("pop20a24","pop25a29","pop30a34","pop35a39","pop40a44","pop45a49","pop50a54","pop55a59","pop60a64","pop65a69","pop70a74","pop75a79","pop80m"),
               names_to = "age_group",
               values_to = "population") %>% 
  mutate(idade_grupo_who2 = case_when(age_group %in% c("pop20a24","pop25a29") ~ "20-29",
                                      age_group %in% c("pop30a34","pop35a39") ~ "30-39",
                                      age_group %in% c("pop40a44","pop45a49") ~ "40-49",
                                      age_group %in% c("pop50a54","pop55a59") ~ "50-59",
                                      age_group %in% c("pop60a64","pop65a69") ~ "60-69",
                                      age_group %in% c("pop70a74","pop75a79") ~ "70-79",
                                      TRUE ~ "mais de 80 anos")) %>% 
  group_by(ano, codmun, idade_grupo_who2) %>% 
  summarize(population = sum(population))


# criando dataframe com os dados de 2020 replicados para 2021
df_pop_capital_2021 <-
df_pop_capital %>% 
  filter(ano == 2020) %>% 
  mutate(ano = 2021)


# crianda dataframe de dados de capitais até 2021
df_pop_capital <- rbind(df_pop_capital,df_pop_capital_2021)


# juntando dataframe de pneumonia com dataframe de população brasileira
df_cases_pop_capital <- left_join(x = df_filtered_capital_age, y = df_pop_capital, by = c("ano_inter" = "ano",
                                                                                          "codigo_ibge_6dig" = "codmun",
                                                                                          "idade_grupo_who2" = "idade_grupo_who2"))

# junto dataframe de pneumonia e população com população da OMS
df_capital <- left_join(x = df_cases_pop_capital, y = df_pop, by = c("idade_grupo_who2" = "idade_grupo_who2"))


df_capital$taxa_casos <- df_capital$casos/df_capital$population
df_capital$casos_esperados <- df_capital$taxa_casos * df_capital$pop_rate_perc2

df_capital$taxa_mortes <- df_capital$mortes/df_capital$population
df_capital$mortes_esperadas <- df_capital$taxa_mortes * df_capital$pop_rate_perc2

df_ajuste_idade_capital <-
  df_capital %>% 
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, regiao, estado, codigo_ibge_6dig, nome) %>% 
  summarize(age_ajusted_hospitalizations = sum(casos_esperados) * 100000/sum(pop_rate_perc2), # chamamos de age ajusted
            hospitalizations = sum(casos) * 100000/sum(population), # chamamos de crude
            age_ajusted_deaths = sum(mortes_esperadas) * 100000/sum(pop_rate_perc2), 
            deaths = sum(mortes) * 100000/sum(population),
            lethality = deaths / hospitalizations,
            age_ajusted_lethality = sum(mortes_esperadas)/sum(casos_esperados)) 


# salvando como csv o dataframe filtrado
write.csv(df_ajuste_idade_capital, file = 'output/capital_age_ajusted_sih_aih_pneumo_2012_2021.csv')


##### REGIAO METROPOLITANA #####
# criando dataframe de capitais e municipios de regiao metropolitana
df_info_regiao_metropolitana <- 
  read.csv("input/cidades_classificacoes.csv", sep = ";") %>%
  filter(classificacao %in% c("Capital", "Periferia 1"))

# criando dataframe de pneumonia com apenas as capitais e municipios de regiao metropolitana
df_filtered_regiao_metropolitana <- left_join(x = df_info_regiao_metropolitana, y = df_pneumonia_filtered, by = c("codigo_ibge_6dig" = "munic_res",
                                                                                                                 "estado" = "estado"))

# criando dataframe de capitais com dados por idade
df_filtered_regiao_metropolitana_age <- 
  df_filtered_regiao_metropolitana %>%
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, idade_grupo_who2, regiao, estado, codigo_ibge_6dig, nome) %>% 
  summarize(casos = n(),
            mortes = sum(morte))


# criando dataframe com projeção do total da população por capital segundo o IGBE
df_pop_regiao_metropolitana <- 
  read.csv("input/populacao_municipio.csv") %>% 
  select(-c("id_estado","estado","regiao","no_regiao","macrorregiao","no_macro","pop0a4","pop5a9","pop10a14","pop15a19")) %>% 
  pivot_longer(c("pop20a24","pop25a29","pop30a34","pop35a39","pop40a44","pop45a49","pop50a54","pop55a59","pop60a64","pop65a69","pop70a74","pop75a79","pop80m"),
               names_to = "age_group",
               values_to = "population") %>% 
  mutate(idade_grupo_who2 = case_when(age_group %in% c("pop20a24","pop25a29") ~ "20-29",
                                      age_group %in% c("pop30a34","pop35a39") ~ "30-39",
                                      age_group %in% c("pop40a44","pop45a49") ~ "40-49",
                                      age_group %in% c("pop50a54","pop55a59") ~ "50-59",
                                      age_group %in% c("pop60a64","pop65a69") ~ "60-69",
                                      age_group %in% c("pop70a74","pop75a79") ~ "70-79",
                                      TRUE ~ "mais de 80 anos")) %>% 
  group_by(ano, codmun, idade_grupo_who2) %>% 
  summarize(population = sum(population))


# criando dataframe com os dados de 2020 replicados para 2021
df_pop_regiao_metropolitana_2021 <-
  df_pop_regiao_metropolitana %>% 
  filter(ano == 2020) %>% 
  mutate(ano = 2021)


# crianda dataframe de dados de regiao metropolitana até 2021
df_pop_regiao_metropolitana <- rbind(df_pop_regiao_metropolitana,df_pop_regiao_metropolitana_2021)


# juntando dataframe de pneumonia com dataframe de população brasileira
df_cases_pop_regiao_metropolitana <- left_join(x = df_filtered_regiao_metropolitana_age, y = df_pop_regiao_metropolitana, by = c("ano_inter" = "ano",
                                                                                                                                 "codigo_ibge_6dig" = "codmun",
                                                                                                                                 "idade_grupo_who2" = "idade_grupo_who2"))

# junto dataframe de pneumonia e população com população da OMS
df_regiao_metropolitana <- left_join(x = df_cases_pop_regiao_metropolitana, y = df_pop, by = c("idade_grupo_who2" = "idade_grupo_who2"))


df_regiao_metropolitana$taxa_casos <- df_regiao_metropolitana$casos/df_regiao_metropolitana$population
df_regiao_metropolitana$casos_esperados <- df_regiao_metropolitana$taxa_casos * df_regiao_metropolitana$pop_rate_perc2

df_regiao_metropolitana$taxa_mortes <- df_regiao_metropolitana$mortes/df_regiao_metropolitana$population
df_regiao_metropolitana$mortes_esperadas <- df_regiao_metropolitana$taxa_mortes * df_regiao_metropolitana$pop_rate_perc2

df_ajuste_idade_regiao_metropolitana <-
  df_regiao_metropolitana %>% 
  group_by(ano_inter, mes_inter, mes_inter_int, month_inter, regiao, estado, codigo_ibge_6dig, nome) %>% 
  summarize(age_ajusted_hospitalizations = sum(casos_esperados) * 100000/sum(pop_rate_perc2), # chamamos de age ajusted
            hospitalizations = sum(casos) * 100000/sum(population), # chamamos de crude
            age_ajusted_deaths = sum(mortes_esperadas) * 100000/sum(pop_rate_perc2), 
            deaths = sum(mortes) * 100000/sum(population),
            lethality = deaths / hospitalizations,
            age_ajusted_lethality = sum(mortes_esperadas)/sum(casos_esperados)) 


# salvando como csv o dataframe filtrado
write.csv(df_ajuste_idade_regiao_metropolitana, file = 'output/metropolitan_area_age_ajusted_sih_aih_pneumo_2012_2021.csv')

