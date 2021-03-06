library(tidyverse)
library(extrafont)  #Modificar fuentes de los gráficos
library(tabulizer)  #Extraer tablas de los PDFs

#El procedimiento antes de correr el código ha sido guardar todos los PDFs de los informes disponibles desde el 15 de julio de la web:
#https://www.isciii.es/QueHacemos/Servicios/VigilanciaSaludPublicaRENAVE/EnfermedadesTransmisibles/Paginas/InformesCOVID-19.aspx
#Se puede hacer manualmente o bien copiando los enlaces en un vector y recorrerlo con download.file(). En ambos casos el trabajo hay que dárselo.
#Los PDFs los he guardado con el nombre "informe1.pdf", "informe2.pdf",... así hasta los que haya.

#Las fechas correspondientes a cada informe (tal y como se señalan en el título de cada uno) las guardo en este vector.
#Como se puede ver la periodicidad va cambiando un poco así que hay que utilizar varios seq().
#La utilidad de esto será obtener los casos diarios y situar cada incremento de casos en una fecha concreta.
fechas <- c(as.Date("2020-07-15"), seq(as.Date("2020-07-23"), as.Date("2020-09-03"), by = 7),
            as.Date("2020-09-09"), as.Date("2020-09-16"), seq(as.Date("2020-09-30"), as.Date("2020-10-28"), by = 7),
            as.Date("2020-11-06"), seq(as.Date("2020-11-12"), as.Date("2020-11-26"), by = 7),
            as.Date("2020-12-02"), as.Date("2020-12-09"), as.Date("2020-12-16"), 
            seq(as.Date("2020-12-22"), as.Date("2021-01-05"), by = 7), as.Date("2021-01-13"),
            as.Date("2021-01-20"), as.Date("2021-01-27"), as.Date("2021-02-03"), 
            as.Date("2021-02-10"), as.Date("2021-02-17"))

#Iniciamos la lista donde iremos introduciendo cada tabla
tabla_f <- list()

#Y recorremos cada uno de los pdfs.
for(i in 1:length(fechas)){
  
  #En el elemento "prueba" se almacenan todas las tablas que extract_tables consigue extraer del PDF.
  prueba <- extract_tables(paste0("informe", i, ".pdf"),
                           encoding = "UTF-8")
  
  #Después, localizamos en cuál de las tablas aparece la frase "Ámbito de posible exposición" con str_detect. 
  #El resultado será un vector donde cada elemento corresponde a cada tabla. Los que tengan un valor mayor que 0 (sólo debería ser uno) serán en las que aparece esa expresión.
  tabla <- unlist(lapply(sapply(1:length(prueba), function(i) str_detect(prueba[[i]][,1], "Ámbito de posible exposición")), sum))
  #Nos quedamos con el índice de la tabla donde se encuentran las frecuencias buscadas
  tabla <- which(tabla > 0)
  
  #Introducimos primero un if que nos diga si el informe está a fecha igual o superior al 19/11/2020.
  #Esta fue la fecha en la que se introdujo el ámbito "Social", lo cual quiere decir que en esos casos tendremos que rescatar una fila más.
  if(fechas[i] >= as.Date("2020-11-19")){
    #En el elemento tabla almacenaremos las 8 filas que vienen después de "Ámbito de posible exposición", en las que aparece la frecuencia de cada ámbito
    tabla <- prueba[[tabla]][(which(str_detect(prueba[[tabla]][,1], "Ámbito de posible exposición"))+1):
                               (which(str_detect(prueba[[tabla]][,1], "Ámbito de posible exposición"))+8),]
  }
  else{
    #En el elemento tabla almacenaremos las 7 filas que vienen después de "Ámbito de posible exposición", en las que aparece la frecuencia de cada ámbito
    tabla <- prueba[[tabla]][(which(str_detect(prueba[[tabla]][,1], "Ámbito de posible exposición"))+1):
                               (which(str_detect(prueba[[tabla]][,1], "Ámbito de posible exposición"))+7),]
  }
  
  #Finalmente, de las tablas de 7/8 filas que hemos guardado, nos quedamos con 3 columnas:
  #- ambito: ámbito de exposición. Le indicamos a R que guarde la columna cuyo primer elemento sea "Centro sociosanitario", ya que es en esa columna donde estarán almacenados los ámbitos
  #- num: número acumulado de casos hasta la fecha con ese ámbito de exposición. Este es más complicado porque las celdas tienen la frecuencia absoluta y entre paréntesis la relativa.
  #       La operación a realizar es: 1) Coger la 2ª columna que viene después del nombre de los ámbitos (la que empieza por "Centro sociosanitario")
  #                                   2) Recorrer todos sus elementos y aplicarles un str_split() con pattern = " " (que separe las palabras por los espacios)
  #                                   3) Del resultado del str_split() nos quedamos con el primer elemento, que es la frecuencia absoluta
  #- fecha: fecha del informe. Aquí directamente metemos el elemento correspondiente del vector de fechas que habíamos almacenado antes
  tabla_f[[i]] <- data.frame(ambito = tabla[,which(tabla[1,] == "Centro sociosanitario")],
                        num = sapply(1:nrow(tabla), function(i) str_split(tabla[,which(tabla[1,] == "Centro sociosanitario")+2], " ")[[i]][1]),
                        fecha = fechas[i])
}

#Y ahora podemos proceder a juntar todos los data frames con un do.call(rbind.data.frame, ...)
dat <- do.call(rbind.data.frame, tabla_f)

#Antes de proceder, pasamos la columna "fecha" a su formato correspondiente
dat$fecha <- as.Date(dat$fecha)

#También toca cambiar el ámbito "Desc" que aparece en algunos informes en lugar de "Desconocido"
dat$ambito[which(dat$ambito == "Desc")] <- "Desconocido"

#A partir de aquí, ya podemos graficar con un mínimo preprocesamiento idéntico para todos los gráficos
#Lo describo únicamente para el primero, para no repetirme
dat %>% left_join(data.frame(fecha = fechas, dif = c(NA, diff(fechas))), "fecha") %>%     #Añadimos una columna que nos diga cuánto tiempo ha pasado desde el informe anterior
                                                                                          #Para evitar problemas debido al formato largo de la tabla, lo hacemos con un left_join a un data frame donde cada fila sea una fecha
  mutate(num = as.numeric(as.character(num))) %>%           #Pasamos la columna "num" a formato numérico
  group_by(ambito) %>%  mutate(crec = c(NA, diff(num)),     #Agrupamos por ámbito y vamos sacando la primera diferencia de los acumulados (nuevos casos dentro de cada ámbito)
                               num = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, num), #Allí donde haya un crecimiento negativo, cambiamos el valor de "num" por NA
                                crec = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec)) %>% #en la semana inmediatamente anterior
  mutate(num = imputeTS::na_interpolation(num)) %>%         #Inmediatamente, interpolamos linealmente aquellos informes donde ha habido un crecimiento negativo (sólo se da una vez en un solo ámbito, no es grave)
  mutate(crec = c(NA, diff(num)),                 #Repetimos la operación de antes de forma definitiva, pero ahora sacamos también el promedio de nuevos casos diarios
         crec_dia = crec/dif) %>% ungroup() %>%   #Lo hacemos dividiendo los nuevos casos llegados desde el último informe entre el número de días transcurridos
  group_by(fecha) %>% mutate(prop_crec = crec_dia/sum(crec_dia, na.rm = T)) %>% ungroup() %>%     #Sacamos también la proporción que representa cada ámbito de exposición dentro de los nuevos casos de esa semana
                                                                                                  #Nos servirá para las gráficas donde se muestran los porcentajes
  mutate(ambito = as.character(ambito)) %>%       #Y con estas dos líneas modificamos la etiqueta de "Social" para dejar claro que esa clase aparece por primera vez el 19-nov
  mutate(ambito = ifelse(ambito == "Social^", "Social* (desglosado desde 'Otros' a partir del 19-nov)", ambito)) %>%
  ggplot(aes(x = fecha, y = num, col = ambito)) + 
  geom_hline(yintercept = 0) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 15) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", limits = c(as.Date("2020-07-23"), fechas[length(fechas)]),
               expand = c(0, 4)) +
  scale_y_continuous(breaks = seq(0, 500000, by = 50000)) +
  scale_color_brewer(palette = "Dark2", name = "Ámbito de posible\nexposición") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "#F0F0F0"),
        panel.grid = element_line(colour = "grey75"),
        text = element_text(family = "Liberation Sans")) +
  annotate("text", x = as.Date("2020-12-10"), y = 500000, label = "@Picanumeros", col = "grey", size = 8, family = "Liberation Sans") +
  labs(title = "Número acumulado de casos de COVID-19 notificados semanalmente a la RENAVE según ámbito de exposición",
       subtitle = "Fuente: Informes COVID-19, Instituto de Salud Carlos III (ISCiii)",
       x = "Fecha de incorporación a los datos del informe del ISCiii", 
       y = "Número acumulado de casos",
       caption = "Frecuencias de la Pregunta 7 de la Encuesta para notificación de casos confirmados de covid-19 a nivel estatal\n'Ámbito de posible exposición en los 10 días previos' (Ámbito en el que a juicio de la persona que valora el caso se ha producido la transmisión de la infección)\nCuestionario disponible en el Anexo del documento de Estrategia de Detección Precoz, Vigilancia y Control de COVID-19")
ggsave("grafico1.png", dpi = 300, width = 14.4, height = 8)


dat %>% left_join(data.frame(fecha = fechas, dif = c(NA, diff(fechas))), "fecha") %>%
  mutate(num = as.numeric(as.character(num))) %>%
  group_by(ambito) %>%  mutate(crec = c(NA, diff(num)),
                               num = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, num),
                                crec = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec)) %>%
  mutate(num = imputeTS::na_interpolation(num)) %>%
  mutate(crec = c(NA, diff(num)),
         crec_dia = crec/dif) %>% ungroup() %>%
  group_by(fecha) %>% mutate(prop_crec = crec_dia/sum(crec_dia, na.rm = T)) %>% ungroup() %>%
  mutate(ambito = as.character(ambito)) %>%
  mutate(ambito = ifelse(ambito == "Social^", "Social* (desglosado desde 'Otros' a partir del 19-nov)", ambito)) %>%
  ggplot(aes(x = fecha, y = crec_dia, col = ambito)) + 
  geom_hline(yintercept = 0) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 15) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", limits = c(as.Date("2020-07-23"), fechas[length(fechas)]),
               expand = c(0, 4)) +
  scale_y_continuous(breaks = seq(0, 10000, by = 1000)) +
  scale_color_brewer(palette = "Dark2", name = "Ámbito de posible\nexposición") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "#F0F0F0"),
        panel.grid = element_line(colour = "grey75"),
        text = element_text(family = "Liberation Sans")) +
  annotate("text", x = as.Date("2021-01-01"), y = 6000, label = "@Picanumeros", col = "grey", size = 8, family = "Liberation Sans") +
  labs(title = "Promedio diario de nuevos casos de COVID-19 notificados semanalmente a la RENAVE según ámbito de exposición",
       subtitle = "Fuente: Informes COVID-19, Instituto de Salud Carlos III (ISCiii)",
       x = "Fecha de incorporación a los datos del informe del ISCiii", 
       y = "Promedio de nuevos casos diarios",
       caption = "Frecuencias de la Pregunta 7 de la Encuesta para notificación de casos confirmados de covid-19 a nivel estatal\n'Ámbito de posible exposición en los 10 días previos' (Ámbito en el que a juicio de la persona que valora el caso se ha producido la transmisión de la infección)\nCuestionario disponible en el Anexo del documento de Estrategia de Detección Precoz, Vigilancia y Control de COVID-19")
ggsave("grafico2.png", dpi = 300, width = 14.4, height = 8)


dat %>% left_join(data.frame(fecha = fechas, dif = c(NA, diff(fechas))), "fecha") %>%
  mutate(num = as.numeric(as.character(num))) %>%
  group_by(ambito) %>%  mutate(crec = c(NA, diff(num)),
                                num = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, num),
                                crec = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec)) %>%
  mutate(num = imputeTS::na_interpolation(num)) %>%
  mutate(crec = c(NA, diff(num)),
                              crec_dia = crec/dif) %>% ungroup() %>%
  group_by(fecha) %>% mutate(prop_crec = crec_dia/sum(crec_dia, na.rm = T)) %>% ungroup() %>%
  mutate(ambito = as.character(ambito)) %>%
  mutate(ambito = ifelse(ambito == "Social^", "Social* (desglosado desde 'Otros' a partir del 19-nov)", ambito)) %>%
  ggplot(aes(x = fecha, y = prop_crec*100, col = ambito)) + 
  geom_hline(yintercept = 0) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 15) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", limits = c(as.Date("2020-07-23"), fechas[length(fechas)]),
               expand = c(0, 4)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_color_brewer(palette = "Dark2", name = "Ámbito de posible\nexposición") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "#F0F0F0"),
        panel.grid = element_line(colour = "grey75"),
        text = element_text(family = "Liberation Sans")) +
  annotate("text", x = as.Date("2021-01-01"), y = 55, label = "@Picanumeros", col = "grey", size = 8, family = "Liberation Sans") +
  labs(title = "Distribución del ámbito de posible exposición en los nuevos casos de COVID-19 notificados semanalmente a la RENAVE",
       subtitle = "Fuente: Informes COVID-19, Instituto de Salud Carlos III (ISCiii)",
       x = "Fecha de incorporación a los datos del informe del ISCiii", 
       y = "% de los casos incorporados desde el último informe",
       caption = "Frecuencias de la Pregunta 7 de la Encuesta para notificación de casos confirmados de covid-19 a nivel estatal\n'Ámbito de posible exposición en los 10 días previos' (Ámbito en el que a juicio de la persona que valora el caso se ha producido la transmisión de la infección)\nCuestionario disponible en el Anexo del documento de Estrategia de Detección Precoz, Vigilancia y Control de COVID-19")
ggsave("grafico3.png", dpi = 300, width = 14.4, height = 8)

dat %>% left_join(data.frame(fecha = fechas, dif = c(NA, diff(fechas))), "fecha") %>%
  mutate(num = as.numeric(as.character(num))) %>%
  group_by(ambito) %>%  mutate(crec = c(NA, diff(num)),
                               num = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, num),
                                crec = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec)) %>%
  mutate(num = imputeTS::na.interpolation(num)) %>%
  mutate(crec = c(NA, diff(num)),
         crec_dia = crec/dif) %>% ungroup() %>%
  group_by(fecha) %>% mutate(prop_crec = crec_dia/sum(crec_dia, na.rm = T)) %>% ungroup() %>%
  mutate(ambito = as.character(ambito)) %>%
  mutate(ambito = ifelse(ambito == "Social^", "Social* (desglosado desde 'Otros' a partir del 19-nov)", ambito)) %>%
  ggplot(aes(x = fecha, y = prop_crec*100)) + 
  geom_hline(yintercept = 0) +
  geom_line(size = 1.2) +
  theme_bw(base_size = 15) + 
  facet_wrap(~ambito) +
  scale_x_date(breaks = "14 days", date_labels = "%d\n%b", limits = c(as.Date("2020-07-23"), fechas[length(fechas)]),
               expand = c(0, 4)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme(legend.position = "bottom"#,
        #plot.background = element_rect(fill = "#F0F0F0"),
        #panel.grid = element_line(colour = "grey75")
  ) +
  annotate("text", x = as.Date("2020-12-24"), y = 55, label = "Twitter: @Picanumeros", col = "grey", size = 3, family = "Liberation Sans") +
  labs(title = "Distribución del ámbito de posible exposición en los nuevos casos de COVID-19 notificados semanalmente a la RENAVE",
       subtitle = "Fuente: Informes COVID-19, Instituto de Salud Carlos III (ISCiii)",
       x = "Fecha de incorporación a los datos del informe del ISCiii", 
       y = "% de los casos incorporados desde el último informe",
       caption = "Frecuencias de la Pregunta 7 de la Encuesta para notificación de casos confirmados de covid-19 a nivel estatal\n'Ámbito de posible exposición en los 10 días previos' (Ámbito en el que a juicio de la persona que valora el caso se ha producido la transmisión de la infección)\nCuestionario disponible en el Anexo del documento de Estrategia de Detección Precoz, Vigilancia y Control de COVID-19")
ggsave("grafico4.png", dpi = 300, width = 14.4, height = 8)

dat %>% left_join(data.frame(fecha = fechas, dif = c(NA, diff(fechas))), "fecha") %>%
  mutate(num = as.numeric(as.character(num))) %>%
  group_by(ambito) %>%  mutate(crec = c(NA, diff(num)),
                               num = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, num),
                                crec = ifelse(lead(crec) < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec)) %>%
  mutate(num = imputeTS::na.interpolation(num)) %>%
  mutate(crec = c(NA, diff(num)),
         crec_dia = crec/dif) %>% ungroup() %>%
  group_by(fecha) %>% mutate(prop_crec = crec_dia/sum(crec_dia, na.rm = T)) %>% ungroup() %>%
  mutate(ambito = as.character(ambito)) %>%
  mutate(ambito = ifelse(ambito == "Social^", "Social* (desglosado desde 'Otros' a partir del 19-nov)", ambito)) %>%
  ggplot(aes(x = fecha, y = prop_crec*100, fill = ambito)) + geom_area(col = "black", alpha = .9) +
  theme_minimal(base_size = 15) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0)) +
  scale_fill_brewer(palette = "Dark2", name = "Ámbito de posible\nexposición") +
  theme(panel.grid = element_line(colour = "grey25"), legend.position = "bottom") +
  annotate("text", x = as.Date("2020-12-24"), y = 80, label = "Twitter: @Picanumeros", col = "grey", size = 6, family = "Liberation Sans") +
  annotate("text", x = as.Date("2020-12-17"), y = 95, label = "Centro sociosanitario", col = "black", size = 4, family = "Liberation Sans") +
  annotate("text", x = as.Date("2020-10-15"), y = 75, label = "Desconocido", col = "black", size = 6, family = "Liberation Sans") +
  annotate("text", x = as.Date("2020-10-15"), y = 37, label = "Domicilio", col = "black", size = 6, family = "Liberation Sans") +
  annotate("text", x = as.Date("2020-10-15"), y = 12.5, label = "Laboral", col = "black", size = 5, family = "Liberation Sans") +
  annotate("text", x = as.Date("2020-10-15"), y = 5, label = "Otros", col = "black", size = 6, family = "Liberation Sans") +
  labs(title = "Distribución del ámbito de posible exposición en los nuevos casos de COVID-19 notificados semanalmente a la RENAVE",
       subtitle = "Fuente: Informes COVID-19, Instituto de Salud Carlos III (ISCiii)",
       x = "Fecha de incorporación a los datos del informe del ISCiii", 
       y = "% de los casos incorporados desde el último informe",
       caption = "Frecuencias de la Pregunta 7 de la Encuesta para notificación de casos confirmados de covid-19 a nivel estatal\n'Ámbito de posible exposición en los 10 días previos' (Ámbito en el que a juicio de la persona que valora el caso se ha producido la transmisión de la infección)\nCuestionario disponible en el Anexo del documento de Estrategia de Detección Precoz, Vigilancia y Control de COVID-19")
ggsave("grafico5.png", dpi = 300, width = 14.4, height = 8)

#### Casos sin contacto ####

#El procedimiento para extraer los datos es muy similar al del anterior apartado
tabla_f_cont <- list()
for(i in 1:length(fechas)){
  prueba <- extract_tables(paste0("informe", i, ".pdf"),
                           encoding = "UTF-8")
  
  vec <- 1:length(prueba)
  vec <- vec[which(sapply(vec, function(i) ncol(prueba[[i]])) > 1)] #Seleccionamos los elementos de las tablas extraídas que tengan más de una columna
  
  tabla <- unlist(lapply(sapply(vec, function(i) str_detect(prueba[[i]], "COVID1")), sum)) #De ellos, nos quedamos con los que contengan el término "COVID1"
                                                                                           #Ese es el término que aparece en la tabla de casos sin contacto
  #Aquí hacemos un pequeño parche para un informe con formato distinto al resto                         
  if(i == 2) tabla <- 4
  if(sum(tabla) > 0){
    if(i == 2){
      tabla <- 4
      columna <- 7
    }
    else{
      tabla <- vec[which(tabla > 0)][1] #Nos quedamos con la primera tabla en la que aparezca el término "COVID1" en todo el informe
    }
    
    #Sacamos la columna de esa tabla con la información que buscamos
    columna <- which(apply(prueba[[tabla]], 2, function(x) sum(x %in% c("COVID1", "con COVID1"))) > 0)[1]
    
    #Nos quedamos también con las filas que buscamos, utilizando "Andalucía" como indicador de la fila nº1
    tabla <- prueba[[tabla]][(which(prueba[[tabla]][,1] == "Andalucía")):
                               (which(prueba[[tabla]][,1] == "Andalucía")+19),]
    
    #Creamos la tabla que sacaremos.
    tabla_f_cont[[i]] <- data.frame(ccaa = tabla[,1],
                                    num = sapply(1:nrow(tabla), function(i) str_split(tabla[,columna], " ")[[i]][1]), #En "num", almacenamos el valor que viene antes del paréntesis
                                    por = str_remove_all(
                                      sapply(1:nrow(tabla), function(i) str_split(tabla[,columna], " ")[[i]][2]), #En "por", almacenaremos el porcentaje que hay DENTRO del paréntesis
                                      pattern = "\\(|\\)"),
                                    fecha = fechas[i])
    tabla_f_cont[[i]]$por <- as.numeric(str_replace_all(tabla_f_cont[[i]]$por, ",", "\\.")) #A "por" hay que cambiarle la coma por el punto y pasarlo a numérico, claro
    
    #De nuevo, parche para el informe que sale diferente al resto
    if(i == 2){
      tabla_f_cont[[i]][,1] <- as.character(tabla_f_cont[[i]][,1])
      tabla_f_cont[[i]][,2] <- as.character(tabla_f_cont[[i]][,2])
      tabla_f_cont[[i]][20,1] <- "Total España"
      tabla_f_cont[[i]][20,2] <- 16815
      tabla_f_cont[[i]][20,3] <- 47.4
    }
  }
}

dat_cont <- do.call(rbind.data.frame, tabla_f_cont)
dat_cont$num <- as.numeric(as.character(dat_cont$num))

#Sustituimos algunos nombres de CCAA que se leen mal (aunque luego cogeremos sólo el total nacional, pero por si se quisieran hacer análisis por comunidades)
dat_cont$ccaa[which(dat_cont$ccaa == "Castilla-La")] <- "Castilla-La Mancha"
dat_cont$ccaa[which(dat_cont$ccaa == "MCaasnticllhaa y  León")] <- "Castilla y León"
dat_cont$ccaa[which(dat_cont$ccaa == "CMaasnticllhaa y  León")] <- "Castilla y León"
dat_cont$ccaa[which(dat_cont$ccaa == "C, Valenciana")] <- "C. Valenciana"

dat_cont %>% mutate(totales = num/(por/100)) %>%                                    #IMPORTANTE: véase en esta línea que el total de casos acumulados se hace dividiendo
                                                                                    #el total de casos sin contacto conocido entre el porcentaje que éstos suponen del total de casos
                                                                                    #Esta operación da como resultado un número ligeramente inexacto (nada serio, eso sí)
  #De aquí para abajo, el tratamiento de datos es similar al de los gráficos anteriores
  left_join(data.frame(fecha = fechas, dif = c(NA, diff(fechas))), "fecha") %>%
  group_by(ccaa) %>%  mutate(crec = c(NA, diff(num)),
                             num = ifelse(crec < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, num),
                             crec = ifelse(crec < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec)) %>%
  #mutate(num = imputeTS::na_interpolation(num)) %>%
  mutate(crec = c(NA, diff(num)),
         crec_dia = crec/dif) %>% ungroup() %>%
  group_by(ccaa) %>%  mutate(crec_total = c(NA, diff(totales)),
                             crec_total = ifelse(crec_total < 0 & fecha > as.Date("2020-08-01") & fecha < as.Date("2021-02-12"), NA, crec_total),
                             crec_total_dia = crec_total/dif,
                             cociente = crec_dia/crec_total_dia) %>%
  ungroup() %>%
  filter(ccaa == "Total España") %>%
  ggplot(aes(x = fecha, y = cociente*100, label = paste0(round(cociente*100, 1), "%"))) + 
  geom_hline(yintercept = c(0,100), size = 1.05) +
  geom_line(size = 1.2, linetype = "dashed") +
  geom_point(size = 4) +
  geom_text(family = "Liberation Sans", size = 3.5, col = "blue", nudge_y = 3.5) +
  #ggrepel::geom_text_repel(family = "Liberation Sans", size = 3.5, col = "blue") +
  theme_minimal(base_size = 15) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "#F0F0F0"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.grid.minor = element_line(colour = "grey88"),
        text = element_text(family = "Liberation Sans")) +
  annotate("text", x = as.Date("2021-01-05"), y = 95, label = "@Picanumeros", col = "grey", size = 8, family = "Liberation Sans") +
  labs(title = "Porcentaje de casos sin contacto conocido con COVID entre los nuevos casos de COVID-19\nnotificados semanalmente a la RENAVE",
       subtitle = "Fuente: Informes COVID-19, Instituto de Salud Carlos III (ISCiii)",
       x = "Fecha de incorporación a los datos del informe del ISCiii", 
       y = "% de nuevos casos sin contacto conocido con COVID")
ggsave("grafico6.png", dpi = 300, width = 14.4, height = 8)
