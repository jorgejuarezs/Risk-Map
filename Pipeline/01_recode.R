library(readr)
library(dplyr)
library(here)

# set_here()
setwd(here('Pipeline'))

source("00_load.R")

################################################################################################
######################################## RECODIFICACIÓN ########################################
################################################################################################

#################################### ENTIDADES Y MUNICIPIOS ####################################
datos_1<-datos %>% 
  mutate(estado = estado %>% recode('bc' = 'baja california',
                                    'bcs' = 'baja california sur',
                                    'coahuila' = 'coahuila de zaragoza',
                                    'estado de méxico' = 'méxico',
                                    'mexico' = 'méxico',
                                    'michoacán' = 'michoacán de ocampo',
                                    'veracruz' = 'veracruz de ignacio de la llave')) %>% 
  mutate(municipio = ifelse(estado=='oaxaca', recode(municipio,
                                                     'santa maria el tule'='santa maria del tule',
                                                     'salinas cruz' = 'salina cruz',
                                                     'santiago juxtlahuaca de oaxaca'='santiago juxtlahuaca',
                                                     'san pablo ayutla'='san pedro y san pablo ayutla',
                                                     'tapanatepec'='san pedro tapanatepec',
                                                     'matias romero' = 'matias romero avendaño',
                                                     'juchitan de zaragoza'='heroica ciudad de juchitan de zaragoza',
                                                     'juchitan'='heroica ciudad de juchitan de zaragoza',
                                                     'huajuapan de leon'='heroica ciudad de huajuapan de leon',
                                                     'miahuatlan'='santa lucia miahuatlan',
                                                     'bahias huatulco'='santa maria huatulco',
                                                     'tuxtepec'='san juan bautista tuxtepec',
                                                     'puente madera'='san blas atempa'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='ciudad de méxico', recode(municipio,
                                                               'gustavo a madero'='gustavo a. madero',
                                                               'cuahutemoc' = 'cuauhtemoc',
                                                               'tkahuac' = 'tlahuac'
  ),municipio ))  %>% 
  mutate(municipio = ifelse(estado=='guerrero', recode(municipio,
                                                       'teoloapan'='teloloapan',
                                                       'huauxtitlan'='huamuxtitlan',
                                                       'iguala' = 'iguala de la independencia',
                                                       'tlapa' = 'tlapa de comonfort',
                                                       'zihuatanejo' = 'zihuatanejo de azueta',
                                                       'chilpancingo' = 'chilpancingo de los bravo',
                                                       'acapulco' = 'acapulco de juarez',
                                                       'barra vieja' = 'acapulco de juarez',
                                                       'chilapa' = 'chilapa de alvarez',
                                                       'taxco' = 'taxco de alarcon',
                                                       'ciudad altamirano' = 'pungarabato',
                                                       'ayotzinapa' = 'tixtla de guerrero'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='chihuahua', recode(municipio,
                                                        'cd. juarez'='juarez',
                                                        'ciudadjuarez' = 'juarez',
                                                        'ciudad juarez' = 'juarez',
                                                        'parral' = 'hidalgo del parral'),municipio ))  %>% 
  mutate(municipio = ifelse(estado=='quintana roo', recode(municipio,
                                                           'cancun'='benito juarez',
                                                           'playa del carmen' = 'solidaridad'),municipio ))  %>% 
  mutate(municipio = ifelse(estado=='tabasco', recode(municipio,
                                                      'villahermosa'='centro',
                                                      'villa hermosa' = 'centro'),municipio ))   %>% 
  mutate(municipio = ifelse(estado=='hidalgo', recode(municipio,
                                                      'huejutla'='huejutla de reyes',
                                                      'pachuca' = 'pachuca de soto',
                                                      'tula' = 'tula de allende'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='méxico', recode(municipio,
                                                     'san martin piramides'='san martin de las piramides',
                                                     'valle de chalco' = 'valle de chalco solidaridad',
                                                     'ecatepec' = 'ecatepec de morelos',
                                                     'los reyes la paz'='la paz'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='tamaulipas', recode(municipio,
                                                         'ciudad victoria'='victoria'),municipio ))  %>% 
  mutate(municipio = ifelse(estado=='nuevo león', recode(municipio,
                                                         'san pedro garza'='san pedro garza garcia',
                                                         'escobedo' = 'general escobedo'),municipio ))   %>% 
  mutate(municipio = ifelse(estado=='puebla', recode(municipio,
                                                     'ciudad serdan'='chalchicomula de sesma',
                                                     'cuetzalan' = 'cuetzalan del progreso',
                                                     'san juan tahictic' = 'zacapoaxtla',
                                                     'zacacopan' = 'eloxochitlan'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='veracruz de ignacio de la llave', recode(municipio,
                                                                              'jalancingo'='jalacingo',
                                                                              'poza rica' = 'poza rica de hidalgo'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='jalisco', recode(municipio, 
                                                      'guadalajara guadalajara'='guadalajara',
                                                      'san sebastian teponahuaxtlan' = 'mezquitic',
                                                      'temacapulin'= 'cañadas de obregon'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='morelos', recode(municipio, 
                                                      'ocotepec'='cuernavaca'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='tlaxcala', recode(municipio, 
                                                       'jose maria molestos buenavista'='tlaxco'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='querétaro', recode(municipio, 
                                                        'santa maria magdalena'='queretaro'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='nayarit', recode(municipio, 
                                                      'huajimic'='la yesca'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='michoacán de ocampo', recode(municipio, 
                                                                  'nueva italia'='mugica'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='guanajuato', recode(municipio, 
                                                         'silao'='silao de la victoria'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='durango', recode(municipio, 
                                                      'victoria'='guadalupe victoria'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='campeche', recode(municipio, 
                                                       'atasta'='carmen'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='baja california', recode(municipio, 
                                                              'rosarito'='playas de rosarito'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='baja california sur', recode(municipio, 
                                                                  'san jose del cabo'='los cabos'),municipio )) %>% 
  mutate(municipio = ifelse(estado=='sinaloa', recode(municipio, 
                                                      'maztlan'='mazatlan',
                                                      'los mochis'='ahome'),municipio ))

########################################### ACTORES ###########################################

datos_2<-datos_1 %>% 
  mutate(actor1 = recode(actor1,
                         'periodistas'='periodista',
                         'ciberinformador'='periodista',
                         'defensor' = 'defensor de dh',
                         'albergue' = 'osc'))

########################################## AGRESIONES ##########################################

datos_3<-datos_2 %>% 
  mutate(agresion = recode(agresion,
                           # AMENAZAS
                           'amenzas' = 'amenazas',
                           'amenazas e intimidacion' ='amenazas',
                           'amenazas digitales' ='amenazas',
                           'amenaza vía sms' ='amenazas',
                           'amenaza elecciones' ='amenazas',
                           'amenaza digital elecciones' ='amenazas',
                           'amenaza digital' ='amenazas',
                           'amenaza' ='amenazas',
                           'agreden y amenazan' ='amenazas',
                           'intimidación' ='amenazas',
                           'intimidación elecciones' ='amenazas',
                           'intimidan' ='amenazas',
                           'hostigamiento elecciones' ='amenazas',
                           # AGRESIONES
                           'agresion' = 'agresion fisica',
                           'agresion fisica elecciones' = 'agresion fisica',
                           'agresiones elecciones' = 'agresion fisica',
                           'agreden' = 'agresion fisica',
                           'agredido' = 'agresion fisica',
                           'agredidos' = 'agresion fisica',
                           'agresión' = 'agresion fisica',
                           'lesiones elecciones' = 'agresion fisica',
                           'golpeado' = 'agresion fisica',
                           'disparos' = 'agresion fisica',
                           'baleado' = 'agresion fisica',
                           'balazos' = 'agresion fisica',
                           'balean' = 'agresion fisica',
                           'posible ataque' = 'agresion fisica',
                           'tortura' = 'agresion fisica',
                           'intento de homicidio' = 'agresion fisica',
                           'intento de secuestro' = 'agresion fisica',
                           # DETENCIONES
                           'retención' = 'detencion',
                           'detención arbitraria' = 'detencion',
                           'detencion arbitraria' = 'detencion',
                           # ATAQUE A INMUEBLES
                           'ataque a inmuebles' = 'ataque a inmuble',
                           'ataque a organizaciones' = 'ataque a inmuble',
                           'ataque inmueble' = 'ataque a inmuble',
                           'quema de inmueble' = 'ataque a inmuble',
                           'daño a la propiedad' = 'ataque a inmuble',
                           'allanamiento' = 'ataque a inmuble',
                           'allanamiento y robo' = 'ataque a inmuble',
                           # DESPRESTIGIO
                           'desprestigio elecciones'='desprestigio',
                           'difamación'='desprestigio',
                           'difamacion'='desprestigio',
                           'campaña de criminalizacion'='desprestigio',
                           # DESAPARICION
                           'desaparición forzada' = 'desaparición',
                           'desaparecido' = 'desaparición',
                           # CENSURA
                           'censura elecciones' = 'censura'))

datos<-datos_3
rm(datos_1, datos_2, datos_3)

