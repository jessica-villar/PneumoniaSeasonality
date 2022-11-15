if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2', dep = TRUE); library(ggplot2)
if (!require('fpp3')) install.packages('fpp3'); library(fpp3)
if (!require('tsibble')) install.packages('tsibble'); library(tsibble)
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('tsfeatures')) install.packages('tsfeatures'); library(tsfeatures)
if (!require('seastests')) install.packages('seastests'); library(seastests)
if (!require('stats')) install.packages('stats'); library(stats)


################## PNEUMONIA COMUNIT√ÅRIA ################## 
##### BRASIL ##### 
# lendo arquivo filtrado de pneumonia
df_Brazil <- 
read.csv("output/brazil_age_ajusted_sih_aih_pneumo_2012_2021.csv")%>% 
  mutate(groupby_column = 'Brazil')


df_Brazil <-
  df_Brazil %>%
  mutate(month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month)


# gr·fico com hospitalizaÁıes e hospitalizaÁıes ajustadas por idade
ggplot(df_Brazil, aes(x = month)) +
  geom_line(aes(y = age_ajusted_hospitalizations, colour = "Age-adjusted hospitalization rate (per 100,000 population)")) +
  geom_line(aes(y = hospitalizations, colour = "Hospitalization rate (per 100,000 population)")) +
  labs(title = "Community adquired pneumonia hospitalizations",
       subtitle = "Brazil",
       y = "") +
  theme(legend.position = "bottom")


ggplot(df_Brazil, aes(x = month)) +
  geom_line(aes(y = age_ajusted_hospitalizations, colour = 'red')) +
  labs(x = "",
       y = "Age-adjusted hospitalization rate (per 100,000 population)") +
  scale_color_manual(values="blue") +
  theme_classic() +
  theme(legend.position = 'none')
  


# gr·fico com mortes e mortes ajustadas por idade
ggplot(df_Brazil, aes(x = month)) +
  geom_line(aes(y = age_ajusted_deaths, colour = "Age-adjusted in-hospital mortality (per 100,000 population)")) +
  geom_line(aes(y = deaths, colour = "In-hospital mortality (per 100,000 population)")) +
  labs(title = "Community adquired pneumonia in-hospital mortality",
       subtitle = "Brazil",
       y = "") +
  theme(legend.position = "bottom")


ggplot(df_Brazil, aes(x = month)) +
  geom_line(aes(y = age_ajusted_deaths, colour = 'red')) +
  labs(x = "",
       y = "Age-adjusted in-hospital mortality (per 100,000 population)") +
  scale_color_manual(values="blue") +
  theme_classic() +
  theme(legend.position = 'none')


# gr·fico com letalidade e letalidade ajustada por idade
ggplot(df_Brazil, aes(x = month)) +
  geom_line(aes(y = age_ajusted_lethality, colour = "Age-adjusted case-fatality rate (per 100,000 population)")) +
  geom_line(aes(y = lethality, colour = "Case-fatality rate (per 100,000 population)")) +
  labs(title = "Community adquired pneumonia case-fatality rate",
       subtitle = "Brazil",
       y = "") +
  theme(legend.position = "bottom")


ggplot(df_Brazil, aes(x = month)) +
  geom_line(aes(y = age_ajusted_lethality, colour = 'red')) +
  labs(x = "",
       y = "Age-adjusted case-fatality rate (per 100,000 population)") +
  scale_color_manual(values="blue") +
  theme_classic() +
  theme(legend.position = 'none')

df_Brazil_2012_2019 <- 
df_Brazil %>%
  filter(!(ano_inter %in% c(2020, 2021)))

# mean(df_Brazil_2012_2019$age_ajusted_hospitalizations)
# median(df_Brazil_2012_2019$age_ajusted_hospitalizations)
# quantile(df_Brazil_2012_2019$age_ajusted_hospitalizations, probs = 0.25)
# quantile(df_Brazil_2012_2019$age_ajusted_hospitalizations, probs = 0.75)
# min(df_Brazil_2012_2019$age_ajusted_hospitalizations)
# max(df_Brazil_2012_2019$age_ajusted_hospitalizations)
# sd(df_Brazil_2012_2019$age_ajusted_hospitalizations)


# boxplot com hospitalizaÁıes ajustadas por idade
df_Brazil_2012_2019$month_inter <- factor(df_Brazil_2012_2019$month_inter, #colocando os meses na ordem correta
                                          levels = month.abb)
ggplot() + 
  geom_boxplot(data = df_Brazil_2012_2019,
               aes(x = month_inter, y = age_ajusted_hospitalizations)) +
  geom_point(data = df_Brazil %>% 
               filter(ano_inter %in% c(2020, 2021)), 
             aes(x = month_inter, y = age_ajusted_hospitalizations, colour = factor(ano_inter))) +
  scale_colour_manual(name = "Pandemic year",
                      values = c("red", "darkorange3")) + 
  labs(title = "Community adquired pneumonia hospitalization rate",
       subtitle = "Brazil",
       x = "",
       y = "Age-ajusted hospitalization rate (per 100,000 population)") 


# plot anual de hospitalizaÁıes ajustadas por idade
df_Brazil %>%
  gg_season(age_ajusted_hospitalizations, labels = "both") +
  labs(x = "",
       y = "Age-ajusted hospitalization rate (per 100,000 population)")


# tabela com percentis de hospitalizacoes
df_desc_pneumonia_Brazil_series <- 
df_Brazil_2012_2019 %>% 
  group_by(groupby_column) %>% 
  summarise(q25 = quantile(age_ajusted_hospitalizations, probs = 0.25),
            q50 = quantile(age_ajusted_hospitalizations, probs = 0.5),
            q75 = quantile(age_ajusted_hospitalizations, probs = 0.75)) %>%
  mutate(age_ajusted_hospitalizations_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
  select(groupby_column, age_ajusted_hospitalizations_p50_iqr)
View(df_desc_pneumonia_Brazil_series)


Brazil_series_list <- 
df_Brazil_2012_2019 %>%
  arrange(groupby_column, ano_inter, month_inter) %>% 
  split(.$groupby_column) %>%
  map(~ts(pull(., age_ajusted_hospitalizations),
          start = c(2012, 01), end = c(2019, 12), # removendo anos da pneumonia
          frequency = 12))


# tabela com a forca da tendencia e da sazonalidade
df_strength_Brazil_series <- 
Brazil_series_list %>% 
  imap(~bind_cols(groupby_column = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                                              trend, seasonal_strength, peak, trough))) %>% 
  bind_rows()
View(df_strength_Brazil_series)


# tabela com teste de sazonalidade
df_pneumonia_Brazil_ts_seastest <- 
Brazil_series_list %>% 
  imap(~bind_cols(groupby_column = .y, 
                  kw_estimate = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$stat,
                  kw_pval = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$Pval)) %>% 
  bind_rows()


df_pneumonia_Brazil_ts_stats <- inner_join(df_desc_pneumonia_Brazil_series, df_strength_Brazil_series, by = c("groupby_column" = "groupby_column")) %>% 
  inner_join(df_pneumonia_Brazil_ts_seastest, c("groupby_column" = "groupby_column"))


write_csv(df_pneumonia_Brazil_ts_stats, "output/df_pneumonia_Brazil_ts_stats.csv")

##### REGIAO #####
# lendo arquivo filtrado de pneumonia
df_region <- read.csv("output/region_age_ajusted_sih_aih_pneumo_2012_2021.csv")

df_region2 <-
df_region %>%
  mutate(month = yearmonth(mes_inter_int)) %>%
  as_tsibble(index = month,
             key = c(regiao))


df_region_2012_2019 <- 
df_region %>%
  filter(!(ano_inter %in% c(2020, 2021)))


# df_region_2012_2019 %>% 
#   group_by(regiao) %>% 
#   summarize(mean = mean(age_ajusted_hospitalizations),
#             meadian = median(age_ajusted_hospitalizations),
#             q25 = quantile(age_ajusted_hospitalizations, probs = 0.25),
#             q75 = quantile(age_ajusted_hospitalizations, probs = 0.75),
#             minimum = min(age_ajusted_hospitalizations),
#             maximum = max(age_ajusted_hospitalizations),
#             sd = sd(age_ajusted_hospitalizations))


# boxplot com hospitalizacoes ajustadas por idade
df_region$month_inter <- factor(df_region$month_inter, #colocando os meses na ordem correta
                                levels = month.abb)
df_region$regiao <- factor(df_region$regiao, levels = c("N", "NE", "CO", "SE", "S"),
                           labels = c("North", "Northeast", "Center-West", "Southeast", "South"), ordered = T)
ggplot() + 
  geom_boxplot(data = df_region %>%
                filter(!(ano_inter %in% c(2020, 2021))),
              aes(x = month_inter, y = age_ajusted_hospitalizations, fill = regiao)) +
  guides(fill = FALSE)  +
  geom_point(data = df_region %>% 
               filter(ano_inter %in% c(2020, 2021)), 
             aes(x = month_inter, y = age_ajusted_hospitalizations, colour = factor(ano_inter))) +
  scale_colour_manual(name = "Pandemic year",
                      values = c("red", "darkorange3")) + 
  facet_wrap(~regiao, scales = "free") +
  labs(x = "",
       y = "Age-ajusted hospitalizations (per 100,000 population)")


# plot anual de hospitalizacoes ajustadas por idade
gg_season(df_region2, age_ajusted_hospitalizations) +
  labs(title = "Community adquired pneumonia hospitalization rate",
       subtitle = "Regions",
       x = "Month",
       y = "Age ajusted hospitalization rate (per 100,000 population)")


# tabela com percentis de hospitalizacoes
df_desc_pneumonia_region_series <- 
df_region_2012_2019 %>% 
  group_by(regiao) %>% 
  summarise(q25 = quantile(age_ajusted_hospitalizations, probs = 0.25),
            q50 = quantile(age_ajusted_hospitalizations, probs = 0.5),
            q75 = quantile(age_ajusted_hospitalizations, probs = 0.75)) %>%
  mutate(age_ajusted_hospitalizations_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
  select(regiao, age_ajusted_hospitalizations_p50_iqr)


region_series_list <- 
df_region %>%
  arrange(regiao, ano_inter, month_inter) %>% 
  split(.$regiao) %>%
  map(~ts(pull(., age_ajusted_hospitalizations),
          start = c(2012, 01), end = c(2019, 12), # removendo anos da pneumonia
          frequency = 12))


# tabela com a forca da tendencia e da sazonalidade
df_strength_all_region_series <- 
  region_series_list %>% 
  imap(~bind_cols(regiao = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                                      trend, seasonal_strength, peak, trough))) %>% 
  bind_rows()
View(df_strength_all_region_series)


# tabela com teste de sazonalidade
df_pneumonia_region_ts_seastest <- 
  region_series_list %>% 
  imap(~bind_cols(regiao = .y, 
                  kw_estimate = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$stat,
                  kw_pval = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$Pval)) %>% 
  bind_rows()


df_pneumonia_region_ts_stats <- inner_join(df_desc_pneumonia_region_series, df_strength_all_region_series, by = c("regiao" = "regiao")) %>% 
  inner_join(df_pneumonia_region_ts_seastest, c("regiao" = "regiao"))


write_csv(df_pneumonia_region_ts_stats, "output/df_pneumonia_region_ts_stats.csv")


##### CAPITAL ##### 
# lendo arquivo filtrado de pneumonia
df_capital <- read.csv("output/capital_age_ajusted_sih_aih_pneumo_2012_2021.csv") %>% 
  # filter(!(ano_inter %in% c("2020","2021"))) %>%
  arrange(regiao)


# boxplot com hospitalizaÁıes ajustadas por idade
df_capital$month_inter <- factor(df_capital$month_inter, #colocando os meses na ordem correta
                                levels = month.abb)
ggplot(df_capital, aes(x = month_inter, y = age_ajusted_hospitalizations, fill = nome)) + 
  geom_boxplot() +
  facet_wrap(~nome, scales = "free") +
  labs(x = "Month",
       y = "Age ajusted hospitalizations (per 100,000 population)")


# tabela com percentis de hospitalizaÁıes
df_desc_pneumonia_capital_series <- 
df_capital %>% 
  group_by(regiao,nome) %>% 
  summarise(q25 = quantile(age_ajusted_hospitalizations, probs = 0.25),
            q50 = quantile(age_ajusted_hospitalizations, probs = 0.5),
            q75 = quantile(age_ajusted_hospitalizations, probs = 0.75)) %>%
  mutate(age_ajusted_hospitalizations_p50_iqr = paste0(round(q50, 1), " [", round(q25, 1), ", ", round(q75, 1), "]")) %>% 
  select(regiao, nome, age_ajusted_hospitalizations_p50_iqr)


# tabela com a forÁa da tendÍncia e da sazonalidade
capital_series_list <- 
df_capital %>% 
  arrange(regiao, ano_inter, month_inter) %>% 
  split(.$nome) %>%
  map(~ts(pull(., age_ajusted_hospitalizations),
          start = c(2012, 01), end = c(2021, 12),
          frequency = 12))


# tabela com a forÁa da tendÍncia e da sazonalidade
df_strength_all_capital_series <- 
capital_series_list %>% 
  imap(~bind_cols(nome = .y, select(tsfeatures::tsfeatures(., features = "stl_features", s.window = "periodic", scale = FALSE),
                                    trend, seasonal_strength, peak, trough))) %>% 
  bind_rows()


# tabela com teste de sazonalidade
df_pneumonia_capital_ts_seastest <- 
capital_series_list %>% 
  imap(~bind_cols(nome = .y, 
                  kw_estimate = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$stat,
                  kw_pval = seastests::kw(x = ., diff = F, residuals = F, autoarima = F, freq = NA)$Pval)) %>% 
  bind_rows()


df_pneumonia_capital_ts_stats <- inner_join(df_desc_pneumonia_capital_series, df_strength_all_capital_series, by = c("nome" = "nome")) %>% 
  inner_join(df_pneumonia_capital_ts_seastest, c("nome" = "nome"))


write_csv(df_pneumonia_capital_ts_stats, "output/df_pneumonia_capital_ts_stats.csv")