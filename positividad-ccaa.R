library(tidyverse)
library(ggthemes)
library(extrafont)
library(egg)
library(zoo)

tests <- readr::read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_test_realizados.csv") %>%   #Leemos los datos
  mutate(ccaa_iso = factor(cod_ine,                                                                                                       #Creamos una nueva variable con los
                           levels = c(paste0("0",0:9),10:19),                                                                             #códigos ISO de cada CA para poder enlazar
                           labels = c("ES", "AN", "AR", "AS", "IB", "CN", "CB",                                                           #esta tabla a la que da el ISCiii
                                      "CL", "CM", "CT", "VC", "EX", "GA", "MD",
                                      "MC", "NC", "PV", "RI", "CE", "ML"))) %>%
  select(Fecha, cod_ine, ccaa_iso, CCAA, PCR)                                                                                             #Nos quedamos solo con las variables de interés

#Si cogemos el informe de Sanidad del 20 de agosto: https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_189_COVID-19.pdf
tests <- tests %>%
  add_row(data.frame(Fecha = rep(as.Date("2020-08-13"),20),                                           #Añadimos las filas que faltan con add_row, con la fecha correspondiente
                     cod_ine = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(cod_ine),    #y los datos del informe de Sanidad, que consiste en sumar a los datos de la
                     ccaa_iso = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(ccaa_iso),  #semana anterior los datos de nuevas PCR hechas en la semana del 7 al 13-ago
                     CCAA = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(CCAA),          #(Recordemos que en este punto la variable está en cifras acumulativas)
                     PCR = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(PCR) + 
                       c(41033, #Andalucia
                         23411, #Aragon
                         12201, #Asturias
                         17404, #Baleares
                         12641, #Canarias
                         8176, #Cantabria
                         25672, #Castilla y Leon
                         13866, #Castilla La Mancha
                         70661, #Catalunya
                         269, #Ceuta
                         66500, #Madrid
                         41828, #Com Valenciana
                         42998, #Pais Vasco
                         9212, #Extremadura
                         16629, #Galicia
                         3623, #La Rioja
                         47, #Melilla
                         12008, #Murcia
                         10355, #Navarra
                         428534 #Espana
                         ) ))

#Si cogemos la nota de prensa del 17 de agosto: https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/COVID-19_pruebas_diagnosticas_13_08_2020.pdf
#Disminuye en todas salvo en Asturias y Melilla
tests <- tests %>%
  add_row(data.frame(Fecha = rep(as.Date("2020-08-13"),20),
                     cod_ine = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(cod_ine),
                     ccaa_iso = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(ccaa_iso),
                     CCAA = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(CCAA),
                     PCR = tests %>% filter(Fecha == as.Date("2020-08-06")) %>% pull(PCR) + 
                       c(35985, #Andalucia
                         22856, #Aragon
                         13654, #Asturias
                         16121, #Baleares
                         10744, #Canarias
                         7422, #Cantabria
                         22574, #Castilla y Leon
                         12391, #Castilla La Mancha
                         59198, #Catalunya
                         225, #Ceuta
                         58601, #Madrid
                         38300, #Com Valenciana
                         39218, #Pais Vasco
                         7874, #Extremadura
                         14429, #Galicia
                         2893, #La Rioja
                         161, #Melilla
                         10614, #Murcia
                         10116, #Navarra
                         383376 #Espana
                       ) ))

#Dejamos aquí la cadena de caracteres para las fuentes usadas y así no hay que escribirlas y reescribirlas para cada gráfico
fuentes <- "Fuente: Datadista (https://github.com/datadista/datasets/tree/master/COVID%2019), Instituto de Salud Carlos III (https://cnecovid.isciii.es/covid19/),\nNota de Prensa del Ministerio de Sanidad del 17/08/2020 (https://www.mscbs.gob.es/gabinete/notasPrensa.do?id=5026)"

#Cargamos los datos de casos del ISCiii y nos quedamos con aquellos que lleguen hasta el último día que haya datos
pcr <- readr::read_csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv") %>%
  select(ccaa_iso, fecha, num_casos_prueba_pcr) %>% filter(fecha <= max(tests$Fecha))

#Discretizamos la variable de fecha por semanas, para así poder trabajar en las mismas unidades que los datos de Datadista
pcr$fecha_grup <- cut.Date(pcr$fecha, breaks = as.Date(c("2020-01-01",as.character(unique(tests$Fecha)))),
                           labels = c(as.character(unique(tests$Fecha))),
                           right = T)

#Sacamos así los nuevos casos por semana con group_by y summarise
pcr <- pcr %>% group_by(ccaa_iso, fecha_grup) %>% summarise(casos = sum(num_casos_prueba_pcr)) %>%
  mutate(fecha_grup = as.Date(fecha_grup, format = "%Y-%m-%d")) #Cambiamos tambien el tipo de variable de fecha_grup para que pase a ser Date

#### EVOLUCIÓN DE LA POSITIVIDAD EN EL CONJUNTO DE ESPAÑA ####

#Sacamos el número de tests y el número de casos de cada semana. Unimos ambas tablas y hacemos las sumas de cada semana.
deltas <- tests %>% rename(fecha_grup = Fecha) %>% 
                          left_join(pcr, c("fecha_grup","ccaa_iso")) %>%
                          filter(cod_ine != "00") %>% 
                          group_by(fecha_grup) %>% 
  summarise(pos.pcr = sum(casos,na.rm = T),
            pruebas.pcr = sum(PCR, na.rm=T))

#Hacemos que la variable pruebas.pcr pase a ser su delta primera (número de pruebas realizadas cada semana, en lugar del número acumulado de pruebas hasta el final de la semana)
#Lo hacemos a la vieja usanza con la función diff() (es lo que menos dolores de cabeza da)
deltas <- as.data.frame(deltas)
deltas[,3] <- c(deltas[1,3], diff(deltas[,3]))


deltas %>% 
  mutate(posit.pcr = pos.pcr/pruebas.pcr) %>%   #Obtenemos la "positividad" (o una cota inferior de la misma) dividiendo el número de casos entre el número de pruebas
  pivot_longer(-fecha_grup,"tipo","value") %>%  #Pasamos la tabla a formato largo utilizando la fecha como ID
  filter(tipo %in% c("posit.pcr") & fecha_grup > "2020-04-23") %>%  #Nos quedamos con la variable de positividad y los datos a partir del 23 de abril
                                                                    #(el dato del 23 de abril es el promedio de positividad desde el inicio de la pandemia, por lo que no refleja
                                                                    #la realidad de la situación a fecha de 23 de abril y conviene retirarlo)
  ggplot(aes(x = fecha_grup,  #Y ya hacemos un ggplot bastante simple
             y = value*100,   #No olvidemos multiplicar la proporción por 100, para pasarlo a porcentaje  
             label = 
               paste0(round(value*100, 2), "%"))) + 
  geom_line(size = 1.05) + 
  geom_point(size = 3) + 
  geom_label(nudge_y = 0.35, size = 5) +
  theme_bw(base_size = 14) +
  scale_y_continuous(breaks = 0:10) +
  scale_x_date(breaks = "7 days", date_labels = "%d %b") +
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el porcentaje",
       y = "Porcentaje de pruebas positivas (100 * casos diagnosticados/pruebas)",
       title = "Evolución del porcentaje de casos positivos (PCR+) sobre el total de pruebas PCR realizadas en el conjunto de España",
       subtitle = fuentes,
       caption = "Actualizado a 15/08/2020") +
  annotate("text",
           x = as.Date("2020-06-01"),
           y = 5,
           label = "@Picanumeros",
           size = 8, col = "grey") + 
  theme(text = element_text(family = "Liberation Sans"))
ggsave("todaespana.png", dpi = 300)

#### EVOLUCIÓN DE LA POSITIVIDAD POR COMUNIDADES AUTÓNOMAS ####

tests %>% rename(fecha_grup = Fecha) %>%                #Hacemos la misma operación de left_join para el dataset de pruebas y el de casos...
       left_join(pcr, c("fecha_grup","ccaa_iso")) %>%
       filter(cod_ine != "00") %>% 
       group_by(fecha_grup, cod_ine, CCAA) %>%          #... con la diferencia de que ahora hay que incluir las comunidades en el group_by
       summarise(pos.pcr = sum(casos,na.rm = T),        #De esta forma obtenemos los casos y pruebas por semana y comunidad autónoma
                 pruebas.pcr = sum(PCR, na.rm=T)) %>% ungroup() %>%
  group_by(CCAA) %>%                                        #Dado que los tests están en acumulado y los casos no, y a que en algunas CCAA hay semanas sin datos, hay que hacer varios trucos
  mutate(sum.pos.pcr = cumsum(pos.pcr)) %>% ungroup() %>%   #1) pasamos los casos a acumulado (con cumsum())
       group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%   #2) ordenamos por fecha y sacamos la delta primera de las pruebas (nº de pruebas por semana por CCAA)
       mutate(pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  filter(pruebas.pcr.dif >= 0) %>%                          #3) retiramos aquellas fechas cuya delta primera sea negativa y volvemos al paso 2)
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%
  mutate(pos.pcr = c(pos.pcr[1], diff(sum.pos.pcr)),        #El paso 2) lo aplicaremos también para el número de casos
         pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  mutate(posit.pcr = pos.pcr/pruebas.pcr.dif) %>%           #A partir de aquí el procedimiento es similar al del gráfico del conjunto de España
  reshape2::melt(id.vars = c("fecha_grup","CCAA")) %>%      #Nótese que aquí he tirado de reshape2::melt en lugar de pivot_longer(). De nuevo, ha sido por ahorrarme dolores de cabeza.
                                                            #Estoy más habituado a reshape2::melt y aquí al tener dos IDs me venía mejor. Si sabes hacerlo en pivot_longer, pa' alante.
  filter(variable %in% c("posit.pcr")) %>%              #Nos quedamos con los datos de positividad para graficarlos...
  filter(fecha_grup != "2020-04-23") %>%                #... y retiramos de nuevo los datos hasta el 23 de abril
  ggplot(aes(x = fecha_grup,                            #Y a graficar! Esta vez con un facet_wrap para hacer el panel por CCAA
             y = as.numeric(as.character(value))*100,
             label = round(as.numeric(as.character(value))*100, 2))) + 
  geom_line(size = 1.05) + geom_point(size = 3) + 
  facet_wrap(~CCAA
             , scales = "free_y"  #MUY IMPORTANTE: si quieres que todas las CCAA compartan escala en el eje Y (hay quien lo prefiere así para comparar mejor), elimina esta línea
             ) + 
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el porcentaje ",
       y = "Porcentaje de pruebas positivas (100 * casos diagnosticados/pruebas)",
       title = "Evolución del porcentaje de casos positivos (PCR+) sobre el total de pruebas PCR realizadas por CCAA",
       subtitle = fuentes,
       caption = "Actualizado a 15/08/2020 | Twitter: @Picanumeros") +
  scale_color_discrete(name = "Tipo de prueba",
                       labels = c("PCR", "Test rápidos")) +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"))
ggsave("posccaa.png", dpi = 300, height = 8, width = 16)

#### EVOLUCIÓN DE LAS TRES VARIABLES ESTANDARIZADAS EN UN MISMO PANEL POR CCAA ####

tests %>% rename(fecha_grup = Fecha) %>%            #Repetimos todo el procedimiento realizado para obtener la positividad por CCAA en el apartado anterior
  left_join(pcr, c("fecha_grup","ccaa_iso")) %>%
  filter(cod_ine != "00") %>% 
  group_by(fecha_grup, cod_ine, CCAA) %>% 
  summarise(pos.pcr = sum(casos,na.rm = T),
            pruebas.pcr = sum(PCR, na.rm=T)) %>% ungroup() %>%
  group_by(CCAA) %>%
  mutate(sum.pos.pcr = cumsum(pos.pcr)) %>% ungroup() %>%
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%
  mutate(pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  filter(pruebas.pcr.dif >= 0) %>%
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%
  mutate(pos.pcr = c(pos.pcr[1], diff(sum.pos.pcr)),
         pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  mutate(posit.pcr = pos.pcr/pruebas.pcr.dif) %>%
  filter(fecha_grup > "2020-04-30") %>%
  group_by(CCAA) %>%                                      #A partir de esta línea cambiamos
  mutate(posit.pcr = scale(posit.pcr),                    #Hacemos una estandarización mediante z-score (restamos la media y dividimos entre desv. típica) para las tres variables
         pos.pcr = scale(pos.pcr),
         pruebas.pcr.dif = scale(pruebas.pcr.dif)) %>%
  mutate(posit.dif = c(NA, diff(posit.pcr)),              #Y una vez realizado este procedimiento, sacamos las deltas primeras de nuevo (en realidad serían deltas segundas del acumulado)
         pos.dif = c(NA, diff(pos.pcr)),                  #para así poder visualizar si la variable está creciendo, está decreciendo, o se mantiene constante
         pruebas.dif = c(NA, diff(pruebas.pcr.dif))) %>% 
  ungroup() %>%                                           #Y de nuevo repetimos el procedimiento del gráfico anterior, añadiendo una línea en el 0 para marcar la frontera entre crecer y decrecer
  reshape2::melt(id.vars = c("fecha_grup","CCAA")) %>%
  filter(variable %in% c("posit.dif","pos.dif","pruebas.dif")) %>%
  # filter(CCAA %in% c("Andalucía", "Asturias", "Cantabria",                #Si queremos hacer "zoom" en ciertas CCAA, podemos añadir una línea como esta que filtre las deseadas
  #                    "Castilla y León", "Illes Balears", "Aragón")) %>%
  ggplot(aes(x = fecha_grup, 
             y = as.numeric(as.character(value)),
             col = variable, group = variable,
             label = round(as.numeric(as.character(value))*100, 2))) + 
  geom_line(size = 1.05) + geom_point(size = 3) + 
  facet_wrap(~CCAA
             , scales = "free_y"
  ) +
  geom_hline(yintercept = 0) +
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el valor de la variable",
       y = "Puntuación normalizada (z-score) de la variable correspondiente",
       title = "Evolución de la delta primera del número de nuevos casos (PCR+), pruebas PCR realizadas y\n% de positividad por CCAA, con valores estandarizados mediante z-score para así situar las tres variables en la misma escala",
       subtitle = fuentes,
       caption = "Actualizado a 15/08/2020 | Twitter: @Picanumeros") +
  theme_bw(base_size = 14) + 
  scale_color_discrete(name = "Variable", labels = c("% positividad", "Nuevos casos confirmados (PCR+)", "Pruebas PCR realizadas")) +
  theme(legend.position = "bottom",
        text = element_text(family = "Liberation Sans"))
ggsave("todojunto1.png", dpi = 300, height = 8, width = 18)

#### EVOLUCIÓN DE LAS PRUEBAS REALIZADAS SEMANALMENTE POR CCAA ####


tests %>% rename(fecha_grup = Fecha) %>%                          #Hacemos la misma operación de left_join para el dataset de pruebas y el de casos...
  left_join(pcr, c("fecha_grup","ccaa_iso")) %>%
  filter(cod_ine != "00") %>% 
  group_by(fecha_grup, cod_ine, CCAA) %>% 
  summarise(pos.pcr = sum(casos,na.rm = T),                       #... con la diferencia de que ahora hay que incluir las comunidades en el group_by
            pruebas.pcr = sum(PCR, na.rm=T)) %>% ungroup() %>%    #De esta forma obtenemos los casos y pruebas por semana y comunidad autónoma
  group_by(CCAA) %>%                                              #Dado que los tests están en acumulado y los casos no, y a que en algunas CCAA hay semanas sin datos, hay que hacer varios trucos
  mutate(sum.pos.pcr = cumsum(pos.pcr)) %>% ungroup() %>%         #1) pasamos los casos a acumulado (con cumsum())
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%              #2) ordenamos por fecha y sacamos la delta primera de las pruebas (nº de pruebas por semana por CCAA)
  mutate(pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  filter(pruebas.pcr.dif >= 0) %>%                                #3) retiramos aquellas fechas cuya delta primera sea negativa y volvemos al paso 2)
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%              #El paso 2) lo aplicaremos también para el número de casos
  mutate(pos.pcr = c(pos.pcr[1], diff(sum.pos.pcr)),              #A partir de aquí el procedimiento es similar al del gráfico del conjunto de España
         pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  mutate(posit.pcr = pos.pcr/pruebas.pcr.dif) %>%
  group_by(CCAA) %>%
  mutate(posit.dif = c(NA, diff(posit.pcr)),                      #Este es el paso en el que hacemos de nuevo diff() para obtener las deltas segundas que indican el crecimiento de la variable
         pos.dif = c(NA, diff(pos.pcr)),
         pruebas.dif = c(NA, diff(pruebas.pcr.dif))) %>% ungroup() %>%
  reshape2::melt(id.vars = c("fecha_grup","CCAA")) %>%            #Nótese que aquí he tirado de reshape2::melt en lugar de pivot_longer(). De nuevo, ha sido por ahorrarme dolores de cabeza.
                                                                  #Estoy más habituado a reshape2::melt y aquí al tener dos IDs me venía mejor. Si sabes hacerlo en pivot_longer, pa' alante.
  filter(variable %in% c("pruebas.dif")) %>%                      #Nos quedamos con los datos de positividad para graficarlos...
  filter(fecha_grup > "2020-04-30") %>%                           #... y retiramos de nuevo los datos hasta el 23 de abril
  ggplot(aes(x = fecha_grup,                                      #Y a graficar! Esta vez con un facet_wrap para hacer el panel por CCAA
             y = as.numeric(as.character(value)),
             #col = variable, group = variable,
             label = round(as.numeric(as.character(value))*100, 2))) + 
  geom_line(size = 1.05) + geom_point(size = 3) + 
  facet_wrap(~CCAA
             , scales = "free_y"
  ) +
  geom_hline(yintercept = 0) +
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el número de pruebas realizadas",
       y = "Diferencia intersemanal del número de pruebas PCR realizadas durante la semana",
       title = "Evolución de la delta primera del número de pruebas PCR realizadas durante la semana en cada CCAA",
       subtitle = fuentes,
       caption = "Twitter: @Picanumeros") +
  theme_bw(base_size = 14) + theme(text = element_text(family = "Liberation Sans"))
ggsave("pruebas1.png", dpi = 300, height = 8, width = 18)

#Los pasos a seguir son análogos para el gráfico de evolución de pruebas realizadas en cada semana, tan sólo quitando las lineas donde se calcula la delta segunda.
tests %>% rename(fecha_grup = Fecha) %>% 
  left_join(pcr, c("fecha_grup","ccaa_iso")) %>%
  filter(cod_ine != "00") %>% 
  group_by(fecha_grup, cod_ine, CCAA) %>% 
  summarise(pos.pcr = sum(casos,na.rm = T),
            pruebas.pcr = sum(PCR, na.rm=T)) %>% ungroup() %>%
  group_by(CCAA) %>%
  mutate(sum.pos.pcr = cumsum(pos.pcr)) %>% ungroup() %>%
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%
  mutate(pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  filter(pruebas.pcr.dif >= 0) %>%
  group_by(cod_ine,CCAA) %>% arrange(fecha_grup) %>%
  mutate(pos.pcr = c(pos.pcr[1], diff(sum.pos.pcr)),
         pruebas.pcr.dif = c(pruebas.pcr[1],diff(pruebas.pcr))) %>% ungroup() %>%
  filter(fecha_grup > "2020-04-23") %>%
  ggplot(aes(x = fecha_grup, 
             y = pruebas.pcr.dif,
             label = round(as.numeric(as.character(pruebas.pcr))*100, 2))) + 
  geom_line(size = 1.05) + geom_point(size = 3) + 
  facet_wrap(~CCAA
             , scales = "free_y"
  ) +
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el número de pruebas realizadas",
       y = "Número de pruebas PCR realizadas durante la semana",
       title = "Evolución del número de pruebas PCR realizadas durante la semana en cada CCAA",
       subtitle = fuentes,
       caption = "Twitter: @Picanumeros") +
  theme_bw(base_size = 14) + theme(text = element_text(family = "Liberation Sans"))
ggsave("pruebas0.png", dpi = 300, height = 8, width = 18)
