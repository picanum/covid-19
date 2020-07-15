library(tidyverse)
library(zoo)
library(ggthemes)

#### ANDALUCÍA ####

#Los datos se pueden obtener de la web:
#https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/41135?CodOper=b3_2314&codConsulta=41135

and <- read.csv("datos_and.csv")
colnames(and) <- c("fecha","0-14", "15-29", "30-44", "45-64", "65-84", ">=85")
and$fecha <- as.Date(and$fecha, format = "%d/%m")
and$fecha_gr <- cut(and$fecha, "3 days")
and$fecha_gr <- as.Date(as.character(and$fecha_gr), format = "%Y-%m-%d")

#Con agrupación
and %>% pivot_longer(cols = colnames(and)[2:7], names_to = "edad", values_to = "casos") %>%
  filter(fecha >= as.Date("2020-03-05")) %>%
  mutate(edad = factor(edad, levels = c("0-14", "15-29", "30-44", "45-64", "65-84", ">=85"))) %>%
  group_by(edad, fecha_gr) %>% summarise(casos = sum(casos)) %>% ungroup() %>%
  #Añadir estas lineas para sacar frecuencias relativas
  group_by(fecha_gr) %>% mutate(c_prop = casos/sum(casos)) %>%
  ggplot(aes(x = fecha_gr, y = edad, fill = casos)) + geom_tile() +
  scale_fill_viridis_c(name = "Nº casos", option = "inferno") + 
  scale_x_date(breaks = "6 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + theme_bw(base_size = 18) +
  labs(x = "Fecha de diagnóstico", y = "Grupo de edad", 
       title = "Mapa de calor del número de casos diarios de COVID-19 en Andalucía\npor grupo de edad según fecha de diagnóstico (actualizado a 14 de julio)",
       subtitle = "Cada celda representa el número de casos en periodos de 3 días para cada grupo de edad",
       caption = "Fuente: Informe COVID-19 en Andalucía, Instituto de Estadística y Cartografía de Andalucía (IECA).\nDatos disponibles en: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/COVID19.html\nTwitter: @Picanumeros (idea tomada a partir del gráfico para Florida de @zorinaq)")

#Sin agrupación
and %>% pivot_longer(cols = colnames(and)[2:7], names_to = "edad", values_to = "casos") %>%
  filter(fecha >= as.Date("2020-03-05")) %>%
  mutate(edad = factor(edad, levels = c("0-14", "15-29", "30-44", "45-64", "65-84", ">=85"))) %>%
  group_by(edad, fecha) %>% summarise(casos = sum(casos)) %>% ungroup() %>%
  ggplot(aes(x = fecha, y = edad, fill = casos)) + geom_tile() +
  scale_fill_viridis_c(name = "Nº casos", option = "inferno",
                       breaks = c(0, 50, 100, 150, 200, 250)) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + theme_bw(base_size = 18) +
  labs(x = "Fecha de diagnóstico", y = "Grupo de edad", 
       title = "Mapa de calor del número de casos diarios de COVID-19 en Andalucía\npor grupo de edad según fecha de diagnóstico (actualizado a 14 de julio)",
       subtitle = "Cada celda representa el número de casos en un día concreto para cada grupo de edad",
       caption = "Fuente: Informe COVID-19 en Andalucía, Instituto de Estadística y Cartografía de Andalucía (IECA).\nDatos disponibles en: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/COVID19.html\nTwitter: @Picanumeros (idea tomada a partir del gráfico para Florida de @zorinaq)")
ggsave("and_edad_celdas.png", dpi = 300)

#Areas apiladas
and %>% pivot_longer(cols = colnames(and)[2:7], names_to = "edad", values_to = "casos") %>%
  filter(fecha >= as.Date("2020-03-05")) %>%
  mutate(edad = factor(edad, levels = c("0-14", "15-29", "30-44", "45-64", "65-84", ">=85"))) %>%
  group_by(edad) %>% mutate(ma_14 = rollmeanr(casos, 14, na.pad = T)) %>% ungroup() %>%
  group_by(fecha) %>% mutate(c_prop = ma_14/sum(ma_14)) %>%
  ggplot(aes(x = fecha, y = c_prop*100, fill = edad)) + 
  geom_area(alpha = .95) +
  scale_fill_brewer(name = "Grupo\nde edad", palette = "Paired") +
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0)) + theme_bw(base_size = 18) +
  theme(panel.grid = element_line(colour = "black")) +
  labs(x = "Fecha de diagnóstico", y = "Porcentaje", 
       title = "¿A qué edades pertenecen los nuevos casos de COVID-19 (PCR+) en Andalucía?",
       subtitle = "Porcentaje que cada grupo de edad representa dentro de la media móvil (ventana de 14 días)\nde nuevos casos diarios por PCR+ en Andalucía según fecha de diagnóstico.",
       caption = "Fuente: Informe COVID-19 en Andalucía, Instituto de Estadística y Cartografía de Andalucía (IECA).\nDatos disponibles en: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/COVID19.html | @Picanumeros")
ggsave("and_edad_areas.png", dpi = 300)

#### CATALUÑA ####

dcat <- readr::read_csv("https://analisi.transparenciacatalunya.cat/api/views/qwj8-xpvk/rows.csv?accessType=DOWNLOAD&sorting=true")
colnames(dcat) <- c("fecha","codregion","nomregion",
                    "edad","codgenero","nomgenero",
                    "tipo","casos")

#Con agrupación
dcat %>% filter(tipo == "Positiu PCR") %>%
  mutate(edad = factor(edad, levels = 
                         c("0-9", "10-19", "20-29", "30-39",
                           "40-49", "50-59", "60-69",
                           "70-79", "80-89", "90+", "No classificat")),
         fecha = as.Date(fecha, format = "%d/%m/%Y"),
         fecha_gr = cut(fecha, "3 days")) %>%
  mutate(fecha_gr = as.Date(as.character(fecha_gr), format = "%Y-%m-%d")) %>%
  group_by(fecha_gr, edad, .drop = FALSE) %>% summarise(casos = sum(casos)) %>%
  filter(edad != "No classificat") %>%
  ggplot(aes(x = fecha_gr, y = edad, fill = casos)) + geom_tile() +
  scale_fill_viridis_c(name = "Nº casos", option = "inferno",
                       breaks = c(0, 200, 400, 600, 800, 1000)) + 
  scale_x_date(breaks = "6 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + theme_bw(base_size = 18) +
  labs(x = "Fecha de diagnóstico", y = "Grupo de edad", 
       title = "Mapa de calor del número de casos diarios (PCR+) de COVID-19 en Cataluña\npor grupo de edad según fecha de diagnóstico (actualizado a 14 de julio)",
       subtitle = "Cada celda representa el número de casos en periodos de 3 días para cada grupo de edad (de 10 años)",
       caption = "Fuente: registro RSAcovid19, Departament de Salut. Dades Obertes Catalunya.\nDatos disponibles en: https://analisi.transparenciacatalunya.cat/\nTwitter: @Picanumeros (idea tomada a partir del gráfico para Florida de @zorinaq)")
ggsave("cat_edad_celdas.png", dpi = 300)


#Sin agrupación
dcat %>% filter(tipo == "Positiu PCR") %>%
  mutate(edad = factor(edad, levels = 
                         c("0-9", "10-19", "20-29", "30-39",
                           "40-49", "50-59", "60-69",
                           "70-79", "80-89", "90+", "No classificat")),
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%
  group_by(fecha, edad, .drop = FALSE) %>% summarise(casos = sum(casos)) %>%
  filter(edad != "No classificat" & fecha >= "2020-03-05") %>%
  ggplot(aes(x = fecha, y = edad, fill = casos)) + geom_tile() +
  scale_fill_viridis_c(name = "Nº casos", option = "inferno",
                       breaks = c(0, 100, 200, 300, 400)) + 
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + theme_bw(base_size = 18) +
  labs(x = "Fecha de diagnóstico", y = "Grupo de edad", 
       title = "Mapa de calor del número de casos diarios (PCR+) de COVID-19 en Cataluña\npor grupo de edad según fecha de diagnóstico (actualizado a 14 de julio)",
       caption = "Fuente: registro RSAcovid19, Departament de Salut. Dades Obertes Catalunya.\nDatos disponibles en: https://analisi.transparenciacatalunya.cat/\nTwitter: @Picanumeros (idea tomada a partir del gráfico para Florida de @zorinaq)")

#Areas apiladas
dcat %>% filter(tipo == "Positiu PCR") %>%
  mutate(edad = factor(edad, levels = 
                         c("0-9", "10-19", "20-29", "30-39",
                           "40-49", "50-59", "60-69",
                           "70-79", "80-89", "90+", "No classificat")),
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%
  group_by(fecha, edad, .drop = FALSE) %>% summarise(casos = sum(casos)) %>%
  filter(edad != "No classificat" & fecha >= "2020-03-05") %>%
  group_by(edad) %>% mutate(ma_14 = rollmeanr(casos, 14, na.pad = T)) %>% ungroup() %>%
  group_by(fecha) %>% mutate(c_prop = ma_14/sum(ma_14)) %>%
  ggplot(aes(x = fecha, y = c_prop*100, fill = edad)) + 
  geom_area(alpha = .95) +
  scale_fill_brewer(name = "Grupo\nde edad", palette = "Paired") +
  scale_x_date(breaks = "7 days", date_labels = "%d\n%b", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0)) + theme_bw(base_size = 18) +
  theme(panel.grid = element_line(colour = "black")) +
  labs(x = "Fecha", y = "Porcentaje", 
        title = "¿A qué edades pertenecen los nuevos casos de COVID-19 (PCR+) en Cataluña?",
        subtitle = "Porcentaje que cada grupo de edad representa dentro de la media móvil (ventana de 14 días)\nde nuevos casos diarios por PCR+ en Cataluña.",
        caption = "Fuente: registro RSAcovid19, Departament de Salut. Dades Obertes Catalunya.\nDatos disponibles en: https://analisi.transparenciacatalunya.cat/ | @Picanumeros")
ggsave("cat_edad_areas.png", dpi = 300)
