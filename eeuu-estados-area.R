library(tidyverse)

datos <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

datos %>% select(Province_State, colnames(datos)[12:161]) %>%
  group_by(Province_State) %>% summarise_all(sum) %>%
  pivot_longer(cols = colnames(datos)[12:161],
               names_to = "Fechas", values_to = "casos") %>%
  mutate(Fechas = as.Date(Fechas, format = "%m/%d/%y")) %>%
  arrange(Fechas) %>% group_by(Province_State) %>% 
  mutate(dif = c(NA, diff(casos))) %>%
  mutate(ma_7 = c(rep(NA,6), zoo::rollmeanr(dif, 7))) %>%
  filter(Fechas > as.Date("2020-03-01")) %>%
  ggplot(aes(x = Fechas, y = ma_7, fill = Province_State)) +
  geom_area(col = "black") +
  scale_fill_viridis_d() + 
  annotate("text", x = as.Date("2020-04-15"), y = 9000,
           label = "Nueva York", size = 8, alpha = .7) +
  annotate("text", x = as.Date("2020-04-18"), y = 15500,
           label = "Nueva Jersey", size = 7, alpha = .7, angle = -20) +
  annotate("text", x = as.Date("2020-05-15"), y = 17000, col = "white",
           label = "Illinois", size = 5, alpha = .5, angle = -15) +
  annotate("text", x = as.Date("2020-06-05"), y = 18500, col = "white",
           label = "California", size = 6, alpha = .5, angle = -15) +
  annotate("text", x = as.Date("2020-06-15"), y = 15000, col = "white",
           label = "Florida", size = 4, alpha = .5, angle = 15) +
  annotate("text", x = as.Date("2020-06-12"), y = 2500,
           label = "Texas", size = 5, alpha = .7) +
  annotate("text", x = as.Date("2020-04-15"), y = 3500,
           label = "Pensilvania", size = 4, alpha = .7) +
  annotate("text", x = as.Date("2020-04-22"), y = 19000, col = "white",
           label = "Massachusetts", size = 4, alpha = .5, angle = -15) +
  theme_minimal(base_size = 16) +
  labs(x = "Fechas", y = "Número de nuevos casos confirmados diarios",
       title = "Evolución de la media móvil (con ventana de 7 días) del número de\nnuevos casos confirmados diarios en Estados Unidos desglosando por estados",
       subtitle = "Fuente: Center for Systems Science and Engineering (CSSE), Universidad Johns Hopkins",
       caption = "@Picanumeros") +
  scale_x_date(breaks = "7 days",
               date_labels = "%d %b",
               expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 36000, by = 2000)) + 
  theme(legend.position = "none")

ggsave("eeuu.png", dpi = 300)
