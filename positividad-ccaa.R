library(tidyverse)
library(zoo)

tests <- readr::read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_test_realizados.csv") %>%
  mutate(ccaa_iso = factor(cod_ine,
                           levels = c(paste0("0",0:9),10:19),
                           labels = c("ES", "AN", "AR", "AS", "IB", "CN", "CB",
                                      "CL", "CM", "CT", "VC", "EX", "GA", "MD",
                                      "MC", "NC", "PV", "RI", "CE", "ML"))) %>%
  select(Fecha, cod_ine, ccaa_iso, CCAA, PCR)

pcr <- readr::read_csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv") %>%
  select(ccaa_iso, fecha, num_casos_prueba_pcr) %>% filter(fecha <= max(tests$Fecha))

pcr$fecha_grup <- cut.Date(pcr$fecha, breaks = as.Date(c("2020-01-01",as.character(unique(tests$Fecha)))),
                           labels = c(as.character(unique(tests$Fecha))),
                           right = T)

pcr <- pcr %>% group_by(ccaa_iso, fecha_grup) %>% summarise(casos = sum(num_casos_prueba_pcr)) %>%
  mutate(fecha_grup = as.Date(fecha_grup, format = "%Y-%m-%d"))

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
  mutate(posit.pcr = pos.pcr/pruebas.pcr.dif) %>%
  reshape2::melt(id.vars = c("fecha_grup","CCAA")) %>%
  filter(variable %in% c("posit.pcr")) %>%
  filter(fecha_grup != "2020-04-23") %>%
  ggplot(aes(x = fecha_grup, 
             y = as.numeric(as.character(value))*100,
             label = round(as.numeric(as.character(value))*100, 2))) + 
  geom_line(size = 1.05) + geom_point(size = 3) + 
  facet_wrap(~CCAA
             #, scales = "free_y"   #COMENTAMOS O NO ESTA LÍNEA EN FUNCIÓN DE SI QUEREMOS TODOS LSO FACETS EN LA MISMA ESCALA 
             ) + 
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el porcentaje ",
       y = "Porcentaje de pruebas positivas (100 * casos diagnosticados/pruebas)",
       title = "Evolución del porcentaje de casos positivos (PCR+) sobre el total de pruebas PCR realizadas por CCAA",
       subtitle = "Fuente: Datadista (https://github.com/datadista/datasets/tree/master/COVID%2019), Instituto de Salud Carlos III (https://cnecovid.isciii.es/covid19/)",
       caption = "Twitter: @Picanumeros") +
  scale_color_discrete(name = "Tipo de prueba",
                       labels = c("PCR", "Test rápidos")) +
  theme_bw(base_size = 16)
ggsave("posccaa1.png", dpi = 300, height = 8, width = 16)

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
  mutate(posit.pcr = pos.pcr/pruebas.pcr.dif) %>%
  filter(fecha_grup > "2020-04-30") %>%
  group_by(CCAA) %>%
  mutate(posit.pcr = scale(posit.pcr),
         pos.pcr = scale(pos.pcr),
         pruebas.pcr.dif = scale(pruebas.pcr.dif)) %>%
  mutate(posit.dif = c(NA, diff(posit.pcr)),
         pos.dif = c(NA, diff(pos.pcr)),
         pruebas.dif = c(NA, diff(pruebas.pcr.dif))) %>% 
  #SI QUEREMOS MOSTRAR CASOS EN BRUTO EN LUGAR DE MEDIAS MÓVILES, SE OMITE EL SIGUIENTE MUTATE
  mutate(posit.dif = rollmeanr(posit.dif, 2, na.pad = T),
         pos.dif = rollmeanr(pos.dif, 2, na.pad = T),
         pruebas.dif = rollmeanr(pruebas.dif, 2, na.pad = T)) %>%
  ungroup() %>%
  reshape2::melt(id.vars = c("fecha_grup","CCAA")) %>%
  filter(variable %in% c("posit.dif","pos.dif","pruebas.dif")) %>%
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
       title = "Evolución de la media móvil (ventana de 2 observaciones) de la delta primera del número de nuevos casos (PCR+), pruebas PCR realizadas y % de positividad por CCAA,\ncon valores estandarizados mediante z-score para así situar las tres variables en la misma escala",
       subtitle = "Fuente: Datadista (https://github.com/datadista/datasets/tree/master/COVID%2019), Instituto de Salud Carlos III (https://cnecovid.isciii.es/covid19/)",
       caption = "Twitter: @Picanumeros") +
  theme_bw(base_size = 14) + 
  scale_color_discrete(name = "Variable", labels = c("% positividad", "Nuevos casos confirmados (PCR+)", "Pruebas PCR realizadas")) +
  theme(legend.position = "bottom")

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
  mutate(posit.pcr = pos.pcr/pruebas.pcr.dif) %>%
  group_by(CCAA) %>%
  mutate(posit.dif = c(NA, diff(posit.pcr)),
         pos.dif = c(NA, diff(pos.pcr)),
         pruebas.dif = c(NA, diff(pruebas.pcr.dif))) %>% ungroup() %>%
  reshape2::melt(id.vars = c("fecha_grup","CCAA")) %>%
  filter(variable == "pruebas.dif") %>%  #EN ESTA LÍNEA SUSTITUIMOS EL "pruebas.dif" POR LA VARIABLE QUE SE NOS ANTOJE VISUALIZAR
  filter(fecha_grup > "2020-04-30") %>%
  ggplot(aes(x = fecha_grup, 
             y = as.numeric(as.character(value)),
             label = round(as.numeric(as.character(value))*100, 2))) + 
  geom_line(size = 1.05) + geom_point(size = 3) + 
  facet_wrap(~CCAA
             , scales = "free_y"
  ) +
  geom_hline(yintercept = 0) +
  labs(x = "Fechas correspondientes al último día de la semana en la que se computa el número de pruebas realizadas",
       y = "Diferencia intersemanal del número de pruebas PCR realizadas durante la semana",
       title = "Evolución de la delta primera del número de pruebas PCR realizadas durante la semana en cada CCAA",
       subtitle = "Fuente: Datadista (https://github.com/datadista/datasets/tree/master/COVID%2019), Instituto de Salud Carlos III (https://cnecovid.isciii.es/covid19/)",
       caption = "Twitter: @Picanumeros") +
  theme_bw(base_size = 14)
