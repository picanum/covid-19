library(tidyverse)
library(ggthemes)
library(sf)
library(patchwork)

#Cargamos los datos provinciales de @numeroteca
numeroteca <- readr::read_csv("https://code.montera34.com:4443/numeroteca/covid19/-/raw/master/data/output/spain/covid19-provincias-spain_consolidated.csv")

#Cargamos el shapefile por provincias españolas. Yo lo tengo guardado en el PC, habiendolo obtenido en la web gadm.org
load("dat.rds")

#Normalizamos los nombres de las provincias entre ambos datasets, ya que los usaremos para hacer la union
#(al no disponer el shapefile de los codigos del INE)
numeroteca$province <- as.factor(numeroteca$province)

levels(numeroteca$province)[which(levels(numeroteca$province) %in% levels(as.factor(dat$NAME_2))==F)] <-
  c("Alicante", "Álava","Baleares","Vizcaya","Castellón","A Coruña","Guipúzcoa","Las Palmas","La Rioja","Santa Cruz de Tenerife","Valencia")

#Nos quedamos con los ultimos datos disponibles de cada provincia
numeroteca <- numeroteca %>% group_by(province) %>% filter(date == max(date))

#Fusionamos ambos conjuntos con el comando 'merge', ya que hacerlo por pipelines da problemas con los poligonos
dat <- merge(dat, numeroteca, by.x = "NAME_2", by.y = "province")

#Guardamos los casos acumulados en una nueva variable denominada 'Casos'
#(este paso no es importante; yo lo hago para que concuerde con mis anteriores versiones)
dat$Casos <- dat$cases_accumulated

#A todas aquellas provincias que no cuenten con datos de casos acumulados se les imputa el dato de casos activos
#IMPORTANTE: hacer esto requiere revisar los datos y los resultados. De momento solo ocurre para Castilla-La Mancha y para ello esta preparado el script
dat$Casos[which(is.na(dat$Casos))] <- dat$activos[which(is.na(dat$Casos))]

#Obtenemos la incidencia acumulada. Yo divido entre una variable que contiene los datos del padron del INE a 1/1/2019.
#El dataset de numeroteca ya cuenta con esta incidencia. 
dat$Casosporcien <- dat$Casos*100000/dat$V2

#Las siguientes lineas sirven para añadir en el subtitulo en que fechas se actualizaron los datos de cada provincia.
#Hay que hacer dos subtitulos: uno para el mapa peninsular y otro para Canarias.
rowscan <- which(dat$NAME_2 %in% c("Las Palmas", "Santa Cruz de Tenerife"))

fechas_peninsula <- sort(table(format(dat$date[-rowscan], "%d-%m-%Y")))
fechas_canarias <- sort(table(format(dat$date[rowscan], "%d-%m-%Y")))

lista_peninsula <- lapply(1:length(fechas_peninsula), function(i)
  c(dat %>% filter(format(date, "%d-%m-%Y") == names(fechas_peninsula)[i] & (NAME_2 %in% c("Las Palmas", "Santa Cruz de Tenerife") == F)) %>% select(NAME_2))$NAME_2
  )
lista_canarias <- lapply(1:length(fechas_canarias), function(i)
  c(dat %>% filter(format(date, "%d-%m-%Y") == names(fechas_canarias)[i] & NAME_2 %in% c("Las Palmas", "Santa Cruz de Tenerife")) %>% select(NAME_2))$NAME_2
)

sub_peninsula <- paste("Actualizado:",
                       ifelse(length(fechas_peninsula)==1, names(fechas_peninsula)[1],
                              paste(
                                paste(sapply(1:(length(fechas_peninsula)-1), 
                                             function(i) paste(names(fechas_peninsula)[i], " (", paste(lista_peninsula[[i]], collapse=", ", sep="")
                                                               ,")", sep="")
                                )
                                ,collapse=", ")
                                , ", ",  names(fechas_peninsula)[length(fechas_peninsula)], " (Resto)", sep="")
                       ))

sub_canarias <- paste("Actualizado:",
                       ifelse(length(fechas_canarias)==1, names(fechas_canarias)[1],
                                paste(sapply(1:(length(fechas_canarias)), 
                                             function(i) paste(names(fechas_canarias)[i], " (", paste(lista_canarias[[i]], collapse=", ", sep="")
                                                               ,")", sep="")
                                )
                                ,collapse=", ")
                       ))

#Graficamos finalmente
g1 <- ggplot(dat[-rowscan,]) + geom_sf(aes(fill = Casosporcien)) + 
  scale_fill_viridis_c(name = "Casos por cada 100.000 habitantes",
                       begin = 1, end = 0.25, option = "magma", trans = "log1p", breaks = c(10, 100, 500, 1000)) + 
  geom_sf_text(aes(label = round(Casosporcien,2)), color = "black", size = 5) + 
  theme_fivethirtyeight() + 
  labs(title = "Incidencia acumulada (casos por 100.000 hab.) de COVID-19 por provincias\nen España peninsular, Ceuta, Melilla y Baleares", 
       subtitle = sub_peninsula, 
       caption = "Fuentes: recopilación 'ProvidencialData19' de numeroteca (https://code.montera34.com:4443/numeroteca/covid19),
       INE (Padrón municipal a 1 de enero de 2019), gadm.org") +
  #La siguiente anotación está sujeta a cambios, sobretodo si CLM vuelve a dar datos de casos acumulados o si alguna comunidad mas deja de darlos
  annotate("text", x = -2, y = 39, size = 3, 
           label = "*Para Castilla-La Mancha\nse toman casos activos") + 
  annotate("text",x = 2, y = 38, label = "Twitter:\n@Picanumeros", size = 10, col = "gray")

#Grafico para Canarias. Notese que los colores de la leyenda son distintos. Normalmente concuerdan bastante bien con los de la peninsula.
g2 <- ggplot(dat[rowscan,]) + geom_sf(aes(fill = Casosporcien)) + 
  scale_fill_viridis_c(name = "Casos por cada 100.000 habitantes",
                       begin = 1 - log(0.45+1)/log(35+1), end = 1 - log(1.65+1)/log(35+1), option = "magma", trans = "log1p"
  ) + 
  geom_sf_text(aes(label = round(Casosporcien,2)), color = "black", size = 5) + 
  theme_fivethirtyeight() + 
  labs(title = "Incidencia acumulada (casos por 100.000 hab.) de COVID-19 por provincias\nen las Islas Canarias", 
       subtitle = sub_canarias, 
       caption = "Fuentes: recopilación 'ProvidencialData19' de numeroteca (https://code.montera34.com:4443/numeroteca/covid19),
       INE (Padrón municipal a 1 de enero de 2019), gadm.org") + 
  annotate("text",x = -16, y = 29, label = "Twitter:\n@Picanumeros", size = 10, col = "gray")

#Esta operacion la podemos hacer gracias a patchwork
g1 / g2 + plot_layout(heights = c(5,2))

#Exportamos en pdf
ggsave("covid_again.pdf", width = 12, height = 18, device = "pdf")
#El proceso no acaba aquí, puesto que hago un retoque en Inkscape para que el color de fondo coincida con el color de cada panel.
