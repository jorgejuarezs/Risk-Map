library(readr)
library(dplyr)
library(stringr)

################################################################################################
######################################## CARGA DE DATOS ########################################
################################################################################################

datos<-'/home/arosas/data_mecanismo/Agresiones_cut  2018.csv' %>% 
  read_csv(locale = locale(encoding = 'latin1')) %>%
  rename_all(tolower) %>% 
  rename_all(~str_replace_all(.,'ñ',"ni")) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate(municipio = municipio %>% str_replace_all('ñ', '_ni_') %>% 
           iconv(to="ASCII//TRANSLIT") %>% 
           str_replace_all('_ni_', 'ñ'))

claves_inegi<-'/home/arosas/data_mecanismo/ARCH971.CSV' %>% 
  read_csv(locale = locale(encoding = 'latin1')) %>% 
  rename_all(tolower) %>% 
  mutate_if(is.character, tolower) %>% 
  select(cve_ent, nom_ent, cve_mun, nom_mun) %>% 
  mutate(nom_mun = nom_mun %>% str_replace_all('ñ', '_ni_') %>% 
           iconv(to="ASCII//TRANSLIT") %>% 
           str_replace_all('_ni_', 'ñ')) %>% 
  distinct
