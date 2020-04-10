library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)

confirmados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

conf2 <- reshape2::melt(confirmados[,-c(1,3:4)]
                        , id.vars = colnames(confirmados)[2]
                        )
conf2$Fechas <- as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y")

dat_graf <- conf2 %>%
  group_by(Fechas, `Country/Region`) %>%
  summarise(value = sum(value)) %>%
  group_by(`Country/Region`) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)/24))

dat_graf2 <- dat_graf %>% 
  filter(`Country/Region` %in% c("Spain", "Italy")) 

dat_graf2[which(dat_graf2[,2]=="Italy" &
                  dat_graf2$value>=20),"diasdesdeexp"] <-
  1:length(which(dat_graf2[,2]=="Italy" & dat_graf2$value>=20))

dat_graf2[which(dat_graf2[,2]=="Spain" &
                  dat_graf2$value>=5),"diasdesdeexp"] <-
  1:length(which(dat_graf2[,2]=="Spain" & dat_graf2$value>=5))

dat_graf2[min(which(dat_graf2[,2]=="Italy" &
                  dat_graf2$value>=20))-2,"diasdesdeexp"] <- 0
dat_graf2[min(which(dat_graf2[,2]=="Spain" &
                      dat_graf2$value>=5))-2, "diasdesdeexp"] <- 0

dat_graf2 %>%
  filter((cifra_dif == 0 & Fechas > as.POSIXct("2020-03-10 00:00:00"))==F) %>%
  group_by(`Country/Region`) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)/24)) %>%
  ggplot(aes(x = diasdesdeexp, y = cifra_dif/fechas_dif,
             col = `Country/Region`
             #, group = aconte
  )) +
  geom_point(size = 3
             , alpha = .25
             ) + 
  geom_line(size = 1.05
            , alpha = .25
            ) +
  stat_smooth(se = F, size = 1.1, method = "gam",
              formula = y ~ s(x, k = 7)) +
  labs(title = paste("Número medio de nuevos casos confirmados por día del COVID-19\ndesde el inicio del crecimiento significativo en España e Italia a", format(Sys.time(), "%d-%m%-%Y")),
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CSSE) de la Universidad Johns Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Días desde el inicio del crecimiento significativo continuado\n(En Italia: 21 de febrero; en España: 25 de febrero)", y = "Casos confirmados por día transcurrido",
       caption = "Curvas obtenidas con modelos GAM con k = 7 | @Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
        plot.background = element_rect(fill = "#BFD5E3")) + 
  scale_color_viridis_d(name = "País", labels = c("Italia", "España"), begin = 0.1, end = 0.6)
ggsave("crec_bruto_0.png", dpi = 150, width = 12, height = 8)


dat_graf2 <- dat_graf2 %>% 
  filter((cifra_dif == 0 & Fechas > as.POSIXct("2020-03-10 00:00:00"))==F) %>%
  group_by(`Country/Region`) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)/24))

dat_graf2[which(dat_graf2[,2]=="Spain"),"cifra_dif"] <- dat_graf2[which(dat_graf2[,2]=="Spain"),"cifra_dif"]*100000/(47100396-c(0,unlist(c(dat_graf2[which(dat_graf2[,2]=="Spain")[-length(which(dat_graf2[,2]=="Spain"))],"value"]))))

dat_graf2[which(dat_graf2[,2]=="Italy"),"cifra_dif"] <- dat_graf2[which(dat_graf2[,2]=="Italy"),"cifra_dif"]*100000/(60317000-c(0,unlist(c(dat_graf2[which(dat_graf2[,2]=="Italy")[-length(which(dat_graf2[,2]=="Italy"))],"value"]))))

dat_graf2 %>% 
  ggplot(aes(x = diasdesdeexp, y = cifra_dif/fechas_dif,
             col = `Country/Region`
             #, group = aconte
  )) +
  geom_point(size = 3
             , alpha = .25
  ) + 
  geom_line(size = 1.05
            , alpha = .25
  ) +
  stat_smooth(se = F, size = 1.1, method = "gam",
              formula = y ~ s(x, k = 7)) +
  labs(title = paste("Número medio de nuevos casos confirmados (por cada 100.000 hab.) por día\ndel COVID-19 desde el inicio del crecimiento significativo en España e Italia a", format(Sys.time(), "%d-%m%-%Y")),
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CSSE) de la Universidad Johns Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Días desde el inicio del crecimiento significativo continuado\n(En Italia: 21 de febrero; en España: 25 de febrero)", 
       y = "Casos confirmados por cada 100.000 habitantes\npor día transcurrido",
       caption = "Curvas obtenidas con modelos GAM con k = 7 | @Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
        plot.background = element_rect(fill = "#BFD5E3")) + scale_color_viridis_d(name = "País", labels = c("Italia", "España"), begin = 0.1, end = 0.6)
ggsave("crec_neto_0.png", dpi = 150, width = 12, height = 8)

dat_graf2 <- dat_graf %>% filter(`Country/Region` %in% c("Spain", "Italy"))
dat_graf2[which(dat_graf2[,2]=="Italy" &
                  dat_graf2$value>=100),"diasdesdeexp"] <-
  1:length(which(dat_graf2[,2]=="Italy" & dat_graf2$value>=100))

dat_graf2[which(dat_graf2[,2]=="Spain" &
                  dat_graf2$value>=100),"diasdesdeexp"] <-
  1:length(which(dat_graf2[,2]=="Spain" & dat_graf2$value>=100))

dat_graf2[min(which(dat_graf2[,2]=="Italy" &
                      dat_graf2$value>=100))-2,"diasdesdeexp"] <- 0
dat_graf2[min(which(dat_graf2[,2]=="Spain" &
                      dat_graf2$value>=100))-2, "diasdesdeexp"] <- 0

dat_graf2 %>% 
  filter((cifra_dif == 0 & Fechas > as.POSIXct("2020-03-10 00:00:00"))==F) %>%
  group_by(`Country/Region`) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)/24)) %>%
  ggplot(aes(x = diasdesdeexp, y = cifra_dif/fechas_dif,
             col = `Country/Region`
             #, group = aconte
  )) +
  geom_point(size = 3
             , alpha = .25
  ) + 
  geom_line(size = 1.05
            , alpha = .25
  ) +
  stat_smooth(se = F, size = 1.1, method = "gam",
              formula = y ~ s(x, k = 7)) +
  labs(title = paste("Número medio de nuevos casos confirmados por día del COVID-19\ndesde el día con >100 casos acumulados en España e Italia a", format(Sys.time(), "%d-%m%-%Y")),
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CSSE) de la Universidad Johns Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Días desde el primero con más de 100 casos acumulados\n(En Italia: 23 de febrero; en España: 2 de marzo)", y = "Casos confirmados por día transcurrido",
       caption = "Curvas obtenidas con modelos GAM con k = 7 | @Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
        plot.background = element_rect(fill = "#BFD5E3")) + scale_color_viridis_d(name = "País", labels = c("Italia", "España"), begin = 0.1, end = 0.6)
ggsave("crec_bruto_100.png", dpi = 150, width = 12, height = 8)
  
dat_graf2 <- dat_graf2 %>% 
  filter((cifra_dif == 0 & Fechas > as.POSIXct("2020-03-10 00:00:00"))==F) %>%
  group_by(`Country/Region`) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)/24))

dat_graf2[which(dat_graf2[,2]=="Spain"),"cifra_dif"] <- dat_graf2[which(dat_graf2[,2]=="Spain"),"cifra_dif"]*100000/(47100396-c(0,unlist(c(dat_graf2[which(dat_graf2[,2]=="Spain")[-length(which(dat_graf2[,2]=="Spain"))],"value"]))))

dat_graf2[which(dat_graf2[,2]=="Italy"),"cifra_dif"] <- dat_graf2[which(dat_graf2[,2]=="Italy"),"cifra_dif"]*100000/(60317000-c(0,unlist(c(dat_graf2[which(dat_graf2[,2]=="Italy")[-length(which(dat_graf2[,2]=="Italy"))],"value"]))))

dat_graf2 %>% 
  ggplot(aes(x = diasdesdeexp, y = cifra_dif/fechas_dif,
             col = `Country/Region`
             #, group = aconte
  )) +
  geom_point(size = 3
             , alpha = .25
  ) + 
  geom_line(size = 1.05
            , alpha = .25
  ) +
  stat_smooth(se = F, size = 1.1, method = "gam",
              formula = y ~ s(x, k = 7)) +
  labs(title = paste("Número medio de nuevos casos confirmados (por cada 100.000 hab.) por día\ndel COVID-19 desde el día con >100 casos acumulados en España e Italia a", format(Sys.time(), "%d-%m%-%Y")),
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CSSE) de la Universidad Johns Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Días desde el primero con más de 100 casos acumulados\n(En Italia: 23 de febrero; en España: 2 de marzo)",
       y = "Casos confirmados por cada 100.000 habitantes\npor día transcurrido",
       caption = "Curvas obtenidas con modelos GAM con k = 7 | @Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
        plot.background = element_rect(fill = "#BFD5E3")) + scale_color_viridis_d(name = "País", labels = c("Italia", "España"), begin = 0.1, end = 0.6)
ggsave("crec_neto_100.png", dpi = 150, width = 12, height = 8)
