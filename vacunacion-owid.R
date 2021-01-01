library(tidyverse)
library(extrafont)
library(WDI)

#Importamos el .csv del repositorio de Our World In Data
dat <- read.csv(
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
  ) 

#Importamos las cifras de población del Banco Mundial (indicador SP.POP.TOTL) a través del paquete WDI.
#A pesar de que el .csv ya incluye las cifras normalizadas, las ofrece sólo con 2 decimales. Con este procedimiento ganamos más exactitud.
pobl <- WDI(indicator = "SP.POP.TOTL", start = 2017, end = 2020, extra = TRUE) %>% drop_na(SP.POP.TOTL) %>%
  group_by(iso3c) %>% filter(year == max(year)) %>% ungroup() %>%                   #Importamos todos los datos que tengan de 2017 a 2020 y nos quedamos con el más reciente
  mutate(country = ifelse(country == "Russian Federation", "Russia", country)) %>%  #A Rusia hay que cambiarle el nombre (aparece como Russian Federation)
  filter(country %in% dat$location) %>%
  mutate(location = country) %>% select(location, SP.POP.TOTL, year)

#Modificamos el dataset inicial para: 
#1) cambiar las fechas a formato fecha
#2) quitar los "huecos" en las series temporales de cada país;
#3) sacar el número total de días que transcurren entre actualizaciones por países
#4) añadirle los datos de población
dat <- dat %>% mutate(date = as.Date(date)) %>% drop_na(total_vaccinations) %>%
  group_by(location) %>% mutate(dias_tr = c(NA, diff(date))) %>%
  left_join(pobl, "location")

#Nos quedamos con el vector de países que tienen 2 o más datos en la serie temporal
paises <- dat %>% group_by(location) %>% summarise(cuenta = sum(is.na(total_vaccinations)==F)) %>% 
  filter(cuenta > 1 & location != "World") %>% pull(location)

#Los almacenamos junto a los nombres en español para luego meterlo en as_labeller()
nom_paises <- c("Bahrein", "Bulgaria", "Canadá", "Chile", "China",
                "Dinamarca", "Inglaterra", "Estonia", "Alemania", "Hungría",
                "Israel", "Italia", "México", "Irlanda del Norte", "Omán",
                "Polonia", "Portugal", "Rumanía", "Rusia", "Escocia",
                "Reino Unido", "Estados Unidos", "Gales")
names(nom_paises) <- paises

#Gráfico de número acumulado de dosis. Nada fuera de lo común: quitamos las naciones de UK (no hay datos de población en el Banco Mundial) y luego ggplot
#Nótese que cogemos la variable del núm. total de vacunaciones (total_vaccinations) y la dividimos entre SP.POP.TOTL, tras multiplicarla por 100
dat %>% filter(location %in% paises) %>%
  filter(location %in% c("England", "Scotland", "Northern Ireland", "Wales") == F) %>%
  ggplot(aes(x = date, y = total_vaccinations*100/SP.POP.TOTL)) + 
  geom_hline(yintercept = 0, col = "red") +
  theme_bw(base_size = 15) +
  geom_line(size = 1.05) + geom_point(size = 3, pch = 21, fill = "blue") + 
  facet_wrap(~location, labeller = as_labeller(nom_paises)) +
  scale_x_date(breaks = "4 days", date_labels = "%d\n%b") +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  theme(text = element_text(family = "Liberation Sans")) +
  labs(x = "Día", y = "Número de dosis por cada 100 habitantes",
       title = "Número acumulado de dosis de vacunas contra la COVID-19 administradas por país por cada 100 habitantes",
       subtitle = "Fuente: Our World in Data (https://ourworldindata.org/covid-vaccinations), Banco Mundial (2019, datos de población)",
       caption = "Sólo incluidos países con 2 o más datos disponibles | Twitter: @Picanumeros")
ggsave("vacunas_acum_nofree.png", dpi = 300, height = 9, width = 13.5)

#Aquí lo mismo, pero dejando el eje Y libre para poder apreciar mejor la evolución de cada país
dat %>% filter(location %in% paises) %>%
  filter(location %in% c("England", "Scotland", "Northern Ireland", "Wales") == F) %>%
  ggplot(aes(x = date, y = total_vaccinations*100/SP.POP.TOTL)) + 
  geom_hline(yintercept = 0, col = "red") +
  theme_bw(base_size = 15) +
  geom_line(size = 1.05) + geom_point(size = 3, pch = 21, fill = "blue") + 
  facet_wrap(~location, labeller = as_labeller(nom_paises), scales = "free_y") +
  scale_x_date(breaks = "4 days", date_labels = "%d\n%b") +
  theme(text = element_text(family = "Liberation Sans")) +
  labs(x = "Día", y = "Número de dosis por cada 100 habitantes",
       title = "Número acumulado de dosis de vacunas contra la COVID-19 administradas por país por cada 100 habitantes",
       subtitle = "Fuente: Our World in Data (https://ourworldindata.org/covid-vaccinations), Banco Mundial (2019, datos de población)",
       caption = "Sólo incluidos países con 2 o más datos disponibles | Twitter: @Picanumeros")
ggsave("vacunas_acum_free.png", dpi = 300, height = 9, width = 13.5)

#Para sacar el promedio diario de dosis administradas, hay que sacar las diferencias en la serie de acumulados (lo que hacemos en el cálculo de la variable dif1)
#y luego dividimos esa cifra entre los días transcurridos entre actualizaciones (vac_pd). El resto es todo igual
dat %>% filter(location %in% paises) %>%
  filter(location %in% c("England", "Scotland", "Northern Ireland", "Wales") == F) %>%
  group_by(location) %>%
  mutate(dif1 = c(NA, diff(total_vaccinations*100/SP.POP.TOTL)),
         vac_pd = dif1/dias_tr) %>%
  ggplot(aes(x = date, y = vac_pd)) + 
  geom_hline(yintercept = 0, col = "red") +
  theme_bw(base_size = 15) +
  geom_line(size = 1.05) + geom_point(size = 3, pch = 21, fill = "blue") + 
  facet_wrap(~location, labeller = as_labeller(nom_paises)) +
  scale_x_date(breaks = "4 days", date_labels = "%d\n%b") +
  theme(text = element_text(family = "Liberation Sans")) +
  labs(x = "Día", y = "Promedio diario de dosis administradas por cada 100 habitantes",
       title = "Promedio diario de dosis de vacunas contra la COVID-19 administradas por país por cada 100 habitantes",
       subtitle = "Fuente: Our World in Data (https://ourworldindata.org/covid-vaccinations), Banco Mundial (2019, datos de población)",
       caption = "Sólo incluidos países/naciones constituyentes con 2 o más datos disponibles | Twitter: @Picanumeros")
ggsave("vacunas_diarias_nofree.png", dpi = 300, height = 9, width = 13.5)

#Aquí lo mismo, pero dejando el eje Y libre para poder apreciar mejor la evolución de cada país
dat %>% filter(location %in% paises) %>%
  filter(location %in% c("England", "Scotland", "Northern Ireland", "Wales") == F) %>%
  group_by(location) %>%
  mutate(dif1 = c(NA, diff(total_vaccinations*100/SP.POP.TOTL)),
         vac_pd = dif1/dias_tr) %>%
  ggplot(aes(x = date, y = vac_pd)) + 
  geom_hline(yintercept = 0, col = "red") +
  theme_bw(base_size = 15) +
  geom_line(size = 1.05) + geom_point(size = 3, pch = 21, fill = "blue") + 
  facet_wrap(~location, labeller = as_labeller(nom_paises), scales = "free_y") +
  scale_x_date(breaks = "4 days", date_labels = "%d\n%b") +
  theme(text = element_text(family = "Liberation Sans")) +
  labs(x = "Día", y = "Promedio diario de dosis administradas por cada 100 habitantes",
       title = "Promedio diario de dosis de vacunas contra la COVID-19 administradas por país por cada 100 habitantes",
       subtitle = "Fuente: Our World in Data (https://ourworldindata.org/covid-vaccinations), Banco Mundial (2019, datos de población)",
       caption = "Sólo incluidos países/naciones constituyentes con 2 o más datos disponibles | Twitter: @Picanumeros")
ggsave("vacunas_diarias_free.png", dpi = 300, height = 9, width = 13.5)

#Este es el script que utilizo para responder a la pregunta "A este ritmo, cuanto tardará cada país en llegar a X dosis por 100 habitantes?"
#(aquí entendemos ritmo como promedio diario de dosis administradas por cada 100 habitantes)
#Si asumimos ese ritmo constante, se trata de resolver la incógnita "tiempo" en la ecuación: nº dosis ya administradas + ritmo*tiempo = X
#Por tanto, tiempo = (X - nºdosis ya administradas)/ritmo
#Nótese que la cifra va variando ya que el ritmo no es homogéneo. A medida que se tengan más datos se podrán hacer las medias móviles u otros modelos.
dat %>% filter(location %in% paises) %>%
  filter(location %in% c("England", "Scotland", "Northern Ireland", "Wales") == F) %>%
  group_by(location) %>%
  mutate(dif1 = c(NA, diff(total_vaccinations*100/SP.POP.TOTL)),
         vac_pd = dif1/dias_tr,
         hasta60 = (60 - total_vaccinations*100/SP.POP.TOTL)/dif1,
         hasta100 = (100 - total_vaccinations*100/SP.POP.TOTL)/dif1,
         hasta60_m = hasta60/30,
         hasta100_m = hasta100/30) %>%
  select(location, date, hasta60, hasta100, hasta60_m, hasta100_m)
