library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(mgcv)

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
  filter(`Country/Region` %in% c("Spain"))
  
dat_graf2[which(dat_graf2[,2]=="Spain" & dat_graf2$value>=5),"diasdesdeexp"] <- 1:length(which(dat_graf2[,2]=="Spain" & dat_graf2$value>=5))

dat_graf2[min(which(dat_graf2[,2]=="Spain" & dat_graf2$value>=5))-2, "diasdesdeexp"] <- 0

dat_graf2 %>% mutate(ma_dif1 = zoo::rollmeanr(cifra_dif/fechas_dif, 14, na.pad = T),
                     dif2 = c(NA, diff(ma_dif1)), 
                     ma_dif2 = zoo::rollmeanr(dif2, 14, na.pad = T)) %>%
    mutate(estado = ifelse(diasdesdeexp <= 20 & diasdesdeexp > 0, 1,
                           ifelse(diasdesdeexp > 115, 2, 3))) %>% filter(estado != 3) %>%
    ggplot(aes(x = diasdesdeexp, y = ma_dif1)) + geom_line(col = "red", size = 1.1) + 
    facet_wrap(~estado, scales = "free_x", labeller = as_labeller(c("1" = "25 febrero - 15 marzo", "2" = "19 junio - 8 julio"))) + 
    labs(x = "Días desde el inicio del crecimiento continuado (desde el 25 de febrero)", 
    y = "Número medio de casos diarios de los 14 días previos", 
    title = "Comparativa de la media móvil (ventana de 14 días) de nuevos casos diarios\nde COVID-19 en España entre el 25 de febrero y el 15 de marzo, y entre el 19 de junio y el 8 de julio", 
    subtitle = "Fuente: Center for Systems Science and Engineering (CSSE), Universidad Johns Hopkins", caption = "@Picanumeros") + 
    theme_bw(base_size = 15) + geom_hline(yintercept = 0) + scale_y_continuous(breaks = seq(0, 600, by = 50)) + scale_x_continuous(breaks = seq(0, 140, by = 2))
ggsave("comparativa1.png", dpi = 300)

dat_graf2 %>% mutate(ma_dif1 = zoo::rollmeanr(cifra_dif/fechas_dif, 14, na.pad = T),
                     dif2 = c(NA, diff(ma_dif1)), 
                     ma_dif2 = zoo::rollmeanr(dif2, 14, na.pad = T)) %>%
    mutate(estado = ifelse(diasdesdeexp <= 20 & diasdesdeexp > 0, 1,
                           ifelse(diasdesdeexp > 115, 2, 3))) %>% filter(estado != 3) %>%
    ggplot(aes(x = diasdesdeexp, y = ma_dif2)) + geom_line(col = "red", size = 1.1) + 
    facet_wrap(~estado, scales = "free_x", labeller = as_labeller(c("1" = "25 febrero - 15 marzo", "2" = "19 junio - 8 julio"))) + 
    labs(x = "Días desde el inicio del crecimiento continuado (desde el 25 de febrero)", 
    y = "Número medio de casos diarios de los 14 días previos", 
    title = "Comparativa de la media móvil (ventana de 14 días) de la delta segunda de nuevos casos diarios\nde COVID-19 en España entre el 25 de febrero y el 15 de marzo, y entre el 19 de junio y el 8 de julio", 
    subtitle = "Fuente: Center for Systems Science and Engineering (CSSE), Universidad Johns Hopkins", caption = "@Picanumeros") + 
    theme_bw(base_size = 15) + geom_hline(yintercept = 0) + scale_y_continuous() + scale_x_continuous(breaks = seq(0, 140, by = 2))
ggsave("comparativa2.png", dpi = 300)
