library(tidyverse)
library(extrafont)
library(zoo)

datos <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#Por si quieres hacerlo con los datos de fallecimientos en lugar de los de casos:
#datos <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

datos %>% mutate(pais = `Country/Region`) %>%
  select(pais, colnames(datos[5:ncol(datos)])) %>%
  group_by(pais) %>% summarise_if(is.numeric, sum) %>%
  pivot_longer(cols = colnames(datos[5:ncol(datos)]),
               names_to = "dia",
               values_to = "casos")

#El dataset pob1.csv está descargado de la web del Banco Mundial. Antes se podía obtener con la librería WDI pero por algun motivo ha dejado de funcionarme.
pob <- readr::read_csv("pob1.csv", skip = 3) %>%
  mutate(SP.POP.TOTL = `2018`) %>%
  select(`Country Name`, `Country Code`, SP.POP.TOTL) %>%
  left_join(readr::read_csv("pobmeta.csv"), by = "Country Code")
pob[which(pob[,2]=="IRN"),1] <- "Iran"
pob[which(pob[,2]=="RUS"),1] <- "Russia"
pob[which(pob[,2]=="EGY"),1] <- "Egypt"
pob[which(pob[,2]=="SVK"),1] <- "Slovakia"
pob[which(pob[,2]=="USA"),1] <- "US"
pob[which(pob[,2]=="KOR"),1] <- "Korea, South"
pob[which(pob[,2]=="CZE"),1] <- "Czechia"
pob[which(pob[,2]=="COD"),1] <- "Congo (Kinshasa)"
pob[which(pob[,2]=="COG"),1] <- "Congo (Brazzaville)"
pob[which(pob[,2]=="VEN"),1] <- "Venezuela"
pob[which(pob[,1]=="Eritrea"),"SP.POP.TOTL"] <- 6050000

#### GRÁFICO ####

datos %>% mutate(pais = `Country/Region`) %>% select(pais, colnames(datos[5:ncol(datos)])) %>% 
  group_by(pais) %>% summarise_if(is.numeric, sum) %>% ungroup() %>% 
  pivot_longer(cols = colnames(datos[5:ncol(datos)]), names_to = "dia", values_to = "casos") %>% 
  left_join(pob[,c(1,3)], by = c("pais" = "Country Name")) %>% 
  mutate(casoshab = casos*100000/SP.POP.TOTL, 
         dia = as.Date(dia, format = "%m/%d/%y")) %>% 
  group_by(pais) %>% 
  mutate(
    dif1 = c(NA, diff(casoshab)), 
    dif2 = c(NA, NA, diff(casoshab, differences = 2))
    ) %>%
  filter(dif1 >= 0 & dif1 < 160) %>%
  mutate(diastr = c(NA, diff(dia)),
         dif1 = c(NA, diff(casoshab))/diastr,
         dif2 = c(NA, diff(dif1))) %>%
  mutate(
    ma_dif1 = rollmeanr(dif1, 14, na.pad = T),
    dif2_suav = c(NA, diff(ma_dif1)),
    suav_dif2_suav = rollmeanr(dif2_suav, 14, na.pad = T),
    suav_ma_dif1 = rollmeanr(ma_dif1, 14, na.pad = T),
    ma_dif2 = rollmeanr(dif2, 14, na.pad = T),
    suav_ma_dif2 = rollmeanr(ma_dif2, 14, na.pad = T)
  ) %>% filter(SP.POP.TOTL > 1000000) %>%
  filter(casoshab >= 10) %>%
  mutate(diadesde10 = 1:n()) %>%
  filter(pais %in% levels(as.factor(pais[which(casoshab > 100)]))) %>%
  ggplot(aes(x = diadesde10, y = suav_dif2_suav, group = pais,
             col = ifelse(pais == "Spain", "Spain", "Other"))) + 
           geom_line(alpha = .75, size = 1.1) +
           scale_color_manual(values = c("grey", "red")) + 
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 16) +
  labs(x = "Días desde el primero en el que se superan los 10 casos por cada 100.000 habitantes",
       y = "Semana móvil de las deltas en la semana móvil\ndel nº de casos nuevos x 100.000 hab.",
       title = "Delta segunda suavizada del número de casos confirmados por cada 100.000 habitantes acumulados por país",
       subtitle = "Fuente: Banco Mundial (2018) y Center for Systems Science and Engineering (CSSE), Universidad Johns Hopkins",
       caption = "Sólo países con más de 1 millón de habitantes que hayan superado los 100 casos por 100.000 habitantes en esta pandemia") +
  theme(legend.position = "none", text = element_text(family = "Liberation Sans")) +
  annotate("text", x = 21.5, y = 0.95, label = "España", 
           size = 5, family = "Liberation Sans", col = "red") +
  annotate("text", x = 30, y = -1, label = "@Picanumeros",
           size = 8, col = "grey")
ggsave("deltasegunda.png", dpi = 300)

#### CONOCER Nº DE DIAS HASTA BAJAR DE 10 CASOS X CADA 100.000 HAB. AL DÍA ####

datos %>% mutate(pais = `Country/Region`) %>% select(pais, colnames(datos[5:ncol(datos)])) %>% 
  group_by(pais) %>% summarise_if(is.numeric, sum) %>% ungroup() %>% 
  pivot_longer(cols = colnames(datos[5:ncol(datos)]), names_to = "dia", values_to = "casos") %>% 
  left_join(pob[,c(1,3)], by = c("pais" = "Country Name")) %>% 
  mutate(casoshab = casos*100000/SP.POP.TOTL, 
         dia = as.Date(dia, format = "%m/%d/%y")) %>% 
  group_by(pais) %>% 
  mutate(
    dif1 = c(NA, diff(casoshab)), 
    dif2 = c(NA, NA, diff(casoshab, differences = 2))
    ) %>%
  filter(dif1 >= 0 & dif1 < 160) %>%
  mutate(diastr = c(NA, diff(dia)),
         dif1 = c(NA, diff(casoshab))/diastr,
         dif2 = c(NA, diff(dif1))) %>%
  mutate(
    ma_dif1 = rollmeanr(dif1, 14, na.pad = T),
    dif2_suav = c(NA, diff(ma_dif1)),
    suav_dif2_suav = rollmeanr(dif2_suav, 14, na.pad = T),
    suav_ma_dif1 = rollmeanr(ma_dif1, 14, na.pad = T),
    ma_dif2 = rollmeanr(dif2, 14, na.pad = T),
    suav_ma_dif2 = rollmeanr(ma_dif2, 14, na.pad = T)
  ) %>% filter(SP.POP.TOTL > 1000000) %>%
  filter(casoshab >= 10) %>%
  mutate(diadesde10 = 1:n()) %>%
  filter(pais %in% levels(as.factor(pais[which(casoshab > 100)]))) %>%
  group_by(pais) %>% 
  summarise(distancia = diadesde10[which(ma_dif1 < ma_dif1[1])[1]] - diadesde10[which.max(ma_dif1)]) %>%
  filter(distancia > 0) %>% arrange(distancia)
  
  #### CONOCER Nº DE DIAS DESDE EL PICO HASTA QUE SE BAJA A UN RITMO DE CRECIMIENTO CORRESPONDIENTE AL 20% DEL MÁXIMO (EL RITMO QUE HABÍA EN EL PICO ####
  
  datos %>% mutate(pais = `Country/Region`) %>% select(pais, colnames(datos[5:ncol(datos)])) %>% 
  group_by(pais) %>% summarise_if(is.numeric, sum) %>% ungroup() %>% 
  pivot_longer(cols = colnames(datos[5:ncol(datos)]), names_to = "dia", values_to = "casos") %>% 
  left_join(pob[,c(1,3)], by = c("pais" = "Country Name")) %>% 
  mutate(casoshab = casos*100000/SP.POP.TOTL, 
         dia = as.Date(dia, format = "%m/%d/%y")) %>% 
  group_by(pais) %>% 
  mutate(
    dif1 = c(NA, diff(casoshab)), 
    dif2 = c(NA, NA, diff(casoshab, differences = 2))
    ) %>%
  filter(dif1 >= 0 & dif1 < 160) %>%
  mutate(diastr = c(NA, diff(dia)),
         dif1 = c(NA, diff(casoshab))/diastr,
         dif2 = c(NA, diff(dif1))) %>%
  mutate(
    ma_dif1 = rollmeanr(dif1, 14, na.pad = T),
    dif2_suav = c(NA, diff(ma_dif1)),
    suav_dif2_suav = rollmeanr(dif2_suav, 14, na.pad = T),
    suav_ma_dif1 = rollmeanr(ma_dif1, 14, na.pad = T),
    ma_dif2 = rollmeanr(dif2, 14, na.pad = T),
    suav_ma_dif2 = rollmeanr(ma_dif2, 14, na.pad = T)
  ) %>% filter(SP.POP.TOTL > 1000000) %>%
  filter(casoshab >= 10) %>%
  mutate(diadesde10 = 1:n()) %>%
  filter(pais %in% levels(as.factor(pais[which(casoshab > 100)]))) %>%
  group_by(pais) %>% 
  summarise(distancia = ifelse(length(diadesde10 > diadesde10[which.max(ma_dif1)]) > 0, 
                               diadesde10[which(diadesde10 > diadesde10[which.max(ma_dif1)] & 
                                           ma_dif1 < max(ma_dif1)/5)] - diadesde10[which.max(ma_dif1)],
                               NA)) %>%
  arrange(distancia) %>% print(., n = 20)
