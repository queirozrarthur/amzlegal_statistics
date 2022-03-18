## Análise das variáveis econômicas, sociais e ambientais da Amazônia Legal

# Limpar o ambiente
rm(list=ls(all=T))

# Definir o diretório
setwd('C:\\Users\\queir\\OneDrive\\Documents\\Academia\\Amazonia MADE\\Classificação Municípios\\amzlegal_statistics')

# Carregar pacotes
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(tmap)
library(geobr)
library(sf)
library(ggpubr)
library(haven)
library(labelled)

# Dados de Área Preservada
dados_preservada <- read.csv2('AreaPreservada_1985a2020.csv')

# Dados de Emissão de Gases
dados_emission <- read.csv2('EmissaoPerCapita_2012a2018.csv')  

# Dados de Complexidade
dados_complex <- read.xlsx('ECI_2006a2019.xlsx') %>% 
  mutate(ibge_id = as.character(ibge_id))

# Dados da População
dados_pop <- read.xlsx('POP_BASE_2000a2021.xlsx') %>% 
  gather('year', 'pop', 4:22) %>% 
  mutate(Cód. = as.character(Cód.),
         year = as.integer(year)) %>% 
  rename('geo_code' = 'Cód.') %>% 
  mutate(pop = as.numeric(pop))

# Dados da População Rural
dados_rural <- read.xlsx('Pop_Rural_2010.xlsx')

# Dados de Emprego Formal
dados_emp <- read.xlsx('Emp_2006e2020.xlsx') %>% 
  mutate(Município = as.character(Município),
         Total = as.numeric(Total),
         year = as.integer(year)) %>% 
  left_join(dados_complex, by = c('Município' = 'ibge_id')) %>% 
  select(ibge_id_7, year.x, Total) %>% 
  rename(year = year.x) %>% 
  unique() %>% 
  mutate(ibge_id_7 = as.character(ibge_id_7))

# Dados Fabrício
dados_fab <- read_dta('Amazonia.dta') %>% 
  select(code, MEI, 'NúmerodeempresasexcetoADMP') %>% 
  rename(mei = MEI,
         empresas = 'NúmerodeempresasexcetoADMP') %>% 
  remove_var_label() %>% 
  mutate(code = as.character(code))

# Juntar e organizar todas as bases para gerar os gráficos e mapas
df <- dados_preservada %>% 
  left_join(dados_emission, by = c('geo_code', 
                                   'state', 
                                   'city', 
                                   'year')) %>% 
  select(-c(pop, latitude, longitude)) %>% 
  left_join(dados_complex, by = c('geo_code' = 'ibge_id_7', 
                                  'year')) %>% 
  select(-c(uf_id, uf_name, ibge_id, mun_name)) %>%
  mutate(geo_code = as.character(geo_code),
         year = as.integer(year)) %>% 
  left_join(dados_pop, by = c('geo_code',
                              'year')) %>% 
  select(-c(Nível, Município)) %>%
  left_join(dados_rural, by = c('geo_code' = 'Cód.')) %>% 
  rename('rural2010' = 'rural') %>% 
  select(-c('Nível', 'Brasil,.Unidade.da.Federação.e.Município',
            'Situação.do.domicílio')) %>% 
  left_join(dados_emp, by = c('geo_code' = 'ibge_id_7',
                              'year')) %>% 
  rename(emp_mun = Total) %>%
  group_by(geo_code) %>%
  mutate(area_preservada1985 = case_when(
    year == 1985 ~ area_preservada,
    year != 1985 ~ area_preservada[year==1985]
  )) %>% 
  mutate(area_preservada2020 = case_when(
    year == 2020 ~ area_preservada,
    year != 2020 ~ area_preservada[year==2020]
  )) %>% 
  mutate(area_preservada2006 = case_when(
    year == 2006 ~ area_preservada,
    year != 2006 ~ area_preservada[year==2006]
  )) %>%
  ungroup() %>%
  mutate(var_preserv20.85 = 
           ((area_preservada2020-area_preservada1985)/area_preservada1985)*100,
         var_preserv20.06 = 
           ((area_preservada2020-area_preservada2006)/area_preservada2006)*100) %>% 
  group_by(geo_code) %>%
  mutate(prop_preservada1985 = case_when(
    year == 1985 ~ prop_preservada,
    year != 1985 ~ prop_preservada[year==1985]
  )) %>%
  mutate(prop_preservada2020 = case_when(
    year == 2020 ~ prop_preservada,
    year != 2020 ~ prop_preservada[year==2020]
  )) %>%
  mutate(prop_preservada2006 = case_when(
    year == 2006 ~ prop_preservada,
    year != 2006 ~ prop_preservada[year==2006]
  )) %>%
  ungroup() %>% 
  mutate(dif_proppreserv20.85 = 
           (prop_preservada2020-prop_preservada1985)*(-1),
         dif_proppreserv20.06 = 
           (prop_preservada2020-prop_preservada2006)*(-1)) %>%
  group_by(geo_code) %>%
  mutate(emi_pc2012 = case_when(
    year == 2012 ~ emission_pc,
    year != 2012 ~ emission_pc[year==2012]
  )) %>%
  mutate(emi_pc2018 = case_when(
    year == 2018 ~ emission_pc,
    year != 2018 ~ emission_pc[year==2018]
  )) %>%
  ungroup() %>% 
  mutate(var_emipc18.12 = 
          ((emi_pc2018-emi_pc2012)/emi_pc2012)*100) %>% 
  group_by(geo_code) %>%
  mutate(emp2006 = case_when(
    year == 2006 ~ emp_mun,
    year != 2006 ~ emp_mun[year==2006]
  )) %>%
  mutate(emp2020 = case_when(
    year == 2020 ~ emp_mun,
    year != 2020 ~ emp_mun[year==2020]
  )) %>% 
  ungroup() %>% 
  mutate(var_emp20.06 = 
           ((emp2020-emp2006)/emp2006)*100) %>%
  mutate(emp_pop = emp_mun/pop) %>% 
  group_by(geo_code) %>%
  mutate(emp_pop2006 = case_when(
    year == 2006 ~ emp_pop,
    year != 2006 ~ emp_pop[year==2006]
  )) %>%
  mutate(emp_pop2020 = case_when(
    year == 2020 ~ emp_pop,
    year != 2020 ~ emp_pop[year==2020]
  )) %>% 
  ungroup() %>%
  mutate(var_emppop20.06 = 
           ((emp_pop2020-emp_pop2006)/emp_pop2006)*100) %>% 
  group_by(geo_code) %>%
  mutate(eci2019 = case_when(
    year == 2019 ~ eci,
    year != 2019 ~ eci[year==2019]
  )) %>%
  mutate(eci2006 = case_when(
    year == 2006 ~ eci,
    year != 2006 ~ eci[year==2006]
  )) %>% 
  ungroup() %>% 
  mutate(var_eci19.06 = 
           ((eci2019-eci2006)/eci2006)*100) %>%
  left_join(dados_fab, by = c('geo_code' = 'code')) %>% 
  mutate(empresas_pop = empresas/pop) %>% 
  filter(year == 2020) %>% 
  select(-c(emission, emission_pc, eci, year))

# Mapas

# Puxar as informações dos estados
ufs <- read_state(year=2018) %>% 
  filter(str_sub(code_state, 1, 2) %in% c(11:17, 21, 51))

# Puxar as informações dos municípios e agrupar a base
munic <- read_municipality(year = 2018)
muns <- munic %>% 
  filter(str_sub(code_state, 1, 2) %in% c(11:17, 21, 51)) %>% 
  mutate(code_muni = as.character(code_muni)) %>% 
  left_join(df, by = c('code_muni' = 'geo_code')) %>% 
  select(-c(state, city))

# Criar a função para avaliar os quadrantes
median_analysis <- function(data, var1, var2){
  data %>%
    mutate(var1 = as.numeric(var1),
           var2 = as.numeric(var2)) %>% 
    filter(var1 != is.nan(var1),
           var2 != is.nan(var2)) %>% 
    mutate(class = case_when(
     var1 < median(var1) &
       var2 > median(var2) ~ "1",
     var1 < median(var1) &
       var2 < median(var2) ~ "2",
     var1 > median(var1) &
       var2 > median(var2) ~ "3",
     var1 > median(var1) &
        var2 < median(var2) ~ "4"))
  }

muns1 <- median_analysis(muns, muns$emi_pc2018, muns$prop_preservada2020) 

median(muns1$var1)
class(muns1$var1)

