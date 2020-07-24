library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(extrafont)

getPalette = colorRampPalette(brewer.pal(12, "Paired"))

datos <- readr::read_csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv")

datos %>% select(ccaa_iso, fecha, num_casos_prueba_pcr) %>%
  group_by(ccaa_iso) %>%
  mutate(casos = num_casos_prueba_pcr,
          ma_7 = c(rep(NA,6), zoo::rollmeanr(casos, 7))) %>% ungroup() %>%
  group_by(fecha) %>% mutate(ma_7_porc = ma_7/sum(ma_7),
                             casos_porc = casos/sum(casos)) %>% ungroup() %>%
  ggplot(aes(x = fecha, y = ma_7_porc*100, fill = ccaa_iso)) +
  geom_area(col = "black", alpha = .925) + 
  scale_fill_manual(name = "CCAA\n(código ISO)", values = getPalette(19)) +
  annotate("text", x = as.Date("2020-05-01"), y = 27.5, label = "Madrid", size = 6) +
  annotate("text", x = as.Date("2020-06-01"), y = 57.5, label = "Cataluña", size = 6) + 
  annotate("text", x = as.Date("2020-06-26"), y = 87, label = "Aragón", size = 4) +
  annotate("text", x = as.Date("2020-03-28"), y = 97.5, label = "Andalucía", size = 5) +
  annotate("text", x = as.Date("2020-04-15"), y = 85, label = "Castilla y León", size = 6) + 
  annotate("text", x = as.Date("2020-06-07"), y = 79, label = "Castilla-La Mancha", size = 4, angle = 30) +
  annotate("text", x = as.Date("2020-04-01"), y = 10, label = "País Vasco", size = 4) +
  annotate("text", x = as.Date("2020-03-15"), y = 2.5, label = "Com. Valenciana", size = 4) +
  annotate("text", x = as.Date("2020-04-01"), y = 46.5, label = "Galicia", size = 4, angle = -25) +
  labs(x = "Fecha", y = "Porcentaje de casos",
       title = "¿De qué Comunidades Autónomas proceden los nuevos casos diarios de COVID-19? ¿En qué porcentaje lo hacen?",
       subtitle = "Porcentaje que cada CA representa dentro de la media móvil (ventana 7 días) de nuevos casos diarios por PCR+ en España. Fuente: https://cnecovid.isciii.es/covid19/",
       caption = "Actualizado a 24/07/2020 | @Picanumeros") +
  scale_x_date(breaks = "14 days", date_labels = "%d %b", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0)) +
  theme_minimal(base_size = 14) + 
  theme(panel.grid = element_line(colour = "black"),
        text = element_text(family = "Liberation Sans"))
ggsave("desglose_ccaa.png", dpi = 300, width = 15.2, height = 8)

datos <- readr::read_csv("https://cnecovid.isciii.es/covid19/resources/datos_provincias.csv")

datos %>% select(provincia_iso, fecha, num_casos_prueba_pcr) %>%
  drop_na(provincia_iso) %>%
  group_by(provincia_iso) %>%
  mutate(casos = num_casos_prueba_pcr,
         ma_7 = c(rep(NA,6), zoo::rollmeanr(casos, 7))) %>% ungroup() %>%
  group_by(fecha) %>% mutate(ma_7_porc = ma_7/sum(ma_7),
                             casos_porc = casos/sum(casos)) %>% ungroup() %>%
  ggplot(aes(x = fecha, y = ma_7_porc*100, fill = provincia_iso)) +
  geom_area(col = "black", alpha = .925) + scale_fill_manual(name = "Provincia\n(código ISO)", values = getPalette(52)) +
  labs(x = "Fecha", y = "Porcentaje de casos",
       title = "¿De qué provincias proceden los nuevos casos diarios de COVID-19? ¿En qué porcentaje lo hacen?",
       subtitle = "Porcentaje que cada provincia representa dentro de la media móvil (ventana de 7 días) de nuevos casos diarios por PCR+ en España. Fuente: https://cnecovid.isciii.es/covid19/",
       caption = "Actualizado a 24/07/2020 | @Picanumeros") +
  scale_x_date(breaks = "14 days", date_labels = "%d %b", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0,0)) +
  theme_minimal(base_size = 14) + 
  theme(panel.grid = element_line(colour = "black"),
        text = element_text(family = "Liberation Sans")) +
  annotate("text", x = as.Date("2020-05-01"), y = 35, label = "Madrid", size = 8) +
  annotate("text", x = as.Date("2020-07-01"), y = 50, label = "Lleida", size = 6) + 
  annotate("text", x = as.Date("2020-04-30"), y = 87, label = "Barcelona", size = 7) +
  annotate("text", x = as.Date("2020-06-25"), y = 64, label = "Huesca", size = 5, angle = 30) +
  annotate("text", x = as.Date("2020-06-27"), y = 20, label = "Málaga", size = 3) +
  annotate("text", x = as.Date("2020-07-10"), y = 2, label = "Zaragoza", size = 4) +
  annotate("text", x = as.Date("2020-07-13"), y = 98.5, label = "Almería", size = 3) +
  annotate("text", x = as.Date("2020-07-07"), y = 27.5, label = "Lugo", size = 3, angle = -15)
ggsave("desglose_prov.png", dpi = 300, width = 15.8, height = 8)
