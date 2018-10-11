library(dplyr)
library(here)
library(purrr)
library(tidyr)
# set_here()
setwd(here('Pipeline'))

source("01_recode.R")

################################################################################################
##################################### PREPARACION DE DATOS #####################################
################################################################################################

load('data/baseprymunMX_r.rdata')

conapo<-baseprymunMX %>%
  set_names(c("renglon","year","nom_ent","id_ent","nom_mun","id_mun","cvegeo","sexo","edad","pob")) %>% 
  filter(year==2015) %>% 
  group_by(cvegeo, sexo) %>% 
  summarise(total = sum(pob)) %>% 
  spread(sexo, total) %>% 
  ungroup %>% 
  mutate(pob_total = Hombres + Mujeres)

datos_listos<-datos %>% 
  select(anio, estado, municipio, actor1, agresion, genero) %>% 
  left_join(claves_inegi %>% 
              select(cve_ent, cve_mun, nom_ent, nom_mun) %>% 
              distinct,
            by = c('estado'='nom_ent', 'municipio'='nom_mun')) %>%
  filter(!is.na(cve_mun)) %>%   # 160 obs perdidas
  count(estado, municipio, actor1, cve_ent, cve_mun) %>% 
  mutate(cvegeo = str_c(cve_ent, cve_mun)) %>% 
  left_join(conapo, by = 'cvegeo') %>% 
  mutate(tasa = (n/pob_total)*1000 )

rm(baseprymunMX)



