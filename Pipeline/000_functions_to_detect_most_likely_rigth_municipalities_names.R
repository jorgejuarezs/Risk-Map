library(dplyr)
library(stringr)
# install.packages('textreuse')
# install.packages('tidytext')
library(textreuse)
library(purrr)
library(tidytext)
library(here)
setwd(here('Pipeline'))

source("000_output_municipalities_to_clean.R")
################################################################################################
####################### DETECCION DE MUNICIPIOS CON SIMILITUD DE JACCARD #######################
################################################################################################

##################################### FUNCIONES AUXILIARES #####################################

sim_jaccard <- function(a, b){
  length(intersect(a, b)) / length(union(a, b))
}

shingle_chars <- function(string, lowercase = FALSE, k = 4){
  # produce shingles (con repeticiones)
  if(lowercase) {
    string <- str_to_lower(string)
  }
  shingles <- seq(1, nchar(string) - k + 1) %>%
    map_chr(function(x) substr(string, x, x + k - 1))
  shingles
}

similitudes<-municipios_para_limpiar %>% 
  select(estado, municipio) %>%
  left_join(claves_inegi, by=c('estado'='nom_ent')) %>% 
  filter(estado =='chiapas') %>% 
  mutate(tejas_municipio = municipio %>% map(shingle_chars, k=2),
         tejas_nom_mun = nom_mun %>% map(shingle_chars, k=2))

############################## REVISION BAJO SIMILITUD DE JACCARD ##############################

similitudes$jaccard<-1:nrow(similitudes) %>%
  map_chr(~sim_jaccard(similitudes$tejas_municipio[.][[1]] %>% unlist, 
                       similitudes$tejas_nom_mun[.][[1]] %>% unlist ))


similitudes<-similitudes %>% 
  arrange(desc(jaccard))




