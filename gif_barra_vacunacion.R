library(tidyverse)
library(extrafont)
library(gganimate)

pob <- 47351567 #Fuente: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176951&menu=ultiDatos&idp=1254735572981
pobobjetivo <- 40129822 #Fuente: https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_GIV_comunicacion_20210403.pdf

dat <- read.csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/original/vacunas/estado_vacunacion_.csv")

g <- dat %>% filter(ccaa == "Totales") %>% select(date_pub, Dosis.administradas, Total.pauta.completada) %>% 
  mutate(fecha_graf = date_pub,
    date_pub = as.Date(date_pub, "%e/%m/%y")) %>%
  mutate(Total.pauta.completada = ifelse(is.na(Total.pauta.completada), 0, Total.pauta.completada)) %>%
  ggplot(aes(x = 0, y = (Dosis.administradas-Total.pauta.completada)/pob,
             label = round(100*(Dosis.administradas-Total.pauta.completada)/pob, 1))) +
  geom_col(aes(x = 0, y = 1), fill = "black", width = 0.4, col = "black") +
  geom_col(aes(x = 0, y = pobobjetivo/pob), fill = "grey40", width = 0.4, col = "black") +
  geom_col(fill = "lightcyan", width = 0.4, col = "black") +
  geom_text(nudge_x = 0.275, nudge_y = 0.02) +
  geom_text(aes(x = 0, y = (Total.pauta.completada)/pob,
                label = round(100*(Total.pauta.completada)/pob, 1)),
            nudge_x = 0.275, nudge_y = -0.01) +
  geom_col(aes(x = 0, y = Total.pauta.completada/pob), fill = "royalblue1", width = 0.4, col = "black") +
  scale_y_continuous(limits = c(0, 1)) + 
  scale_x_continuous(limits = c(-1, 1)) +
  geom_text(aes(x = 0.5, y = 0.5, 
                label = fecha_graf,
                ), size = 15, family = "Impact") +
  coord_flip() + 
  transition_states(date_pub) + 
  theme_void(base_size = 14) +
  labs(title = "Vacunación COVID-19 en España",
       subtitle = "Azul claro: % con al menos una dosis. Azul oscuro: % con dos dosis.\nGris: población a vacunar. Negro: población total.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(family = "Verdana")) +
  ggplot2::annotate("text", x = -0.5, y = 0.5, 
                label = "Fuente: Ministerio de Sanidad,\nrecopilado en el proyecto Escovid19data (@escovid19data).\nEstilo del gráfico basado en @VacunacionEs.\n\nTwitter/Instagram: @Picanumeros", size = 5, family = "Verdana")

animate(g, nframes = 150, height = 1200, width = 1200, res = 150)
