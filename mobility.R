#### Datos de movilidad de Google para Uruguay
## Martín Opertti

install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("writexl")
install.packages("data.table")
install.packages("extrafont")

library(plyr)
library(ggplot2)
library(tidyverse)
library(writexl)
library(data.table)
library(extrafont)

rm(list=ls())

## Primero leemos los datos para Uruguay
setwd()
google_mobility <- read.csv("Global_Mobility_Report.csv")

# La base de datos "uru" contiene los cambios en movilidad respecto al 3-6 de febrero para Uruguay en su conjunto y cada departamento por separado
uru <-google_mobility %>%
  filter(country_region_code == "UY")

# Elimino variable vacía
uru$sub_region_2<-NULL

# Cambio de formato ancho a largo la base  
uru_reshape <- melt(setDT(uru), id.vars = c("date", "country_region","country_region_code","sub_region_1"), variable.name = "tipo")

# Recodifico los "tipos" de movilidad
uru_reshape$tipo <- revalue(uru_reshape$tipo, c("grocery_and_pharmacy_percent_change_from_baseline"="Tiendas de comestibles", 
                                "parks_percent_change_from_baseline"="Parques y plazas", 
                                "residential_percent_change_from_baseline" = "Residencias",
                                "retail_and_recreation_percent_change_from_baseline" = "Otras tiendas", 
                                "transit_stations_percent_change_from_baseline" = "Tránsito",
                                "workplaces_percent_change_from_baseline" = "Lugares de trabajo"))

# Traducir los nombres de las variables
uru_reshape <- uru_reshape %>% rename(fecha=date, movilidad = value)
colnames(uru_reshape)

# Paso la variable de fecha a formato fecha
uru_reshape$fecha <- as.Date(uru_reshape$fecha, "%Y-%m-%d")

# La base uru_gral contiene tiene información solo para Uruguay en su conjunto (sin data por departamento)
uru_gral <- uru_reshape %>% filter(sub_region_1=="")

# Gráfico de movilidad para el total de Uruguay
plot1 <- ggplot(uru_gral, aes(fecha, movilidad, colour = tipo)) +
  geom_line(linetype = "solid", size=1.3) +
  geom_point(data = uru_gral[uru_gral$fecha == max(uru_gral$fecha),],
             aes(x =fecha, color = tipo), show.legend = FALSE, size=3)+
  geom_text(data = uru_gral[uru_gral$fecha == max(uru_gral$fecha),],
            aes(x =fecha, label=movilidad,
                vjust = -2, fontface = "bold"))+ 
  facet_wrap(~ tipo) + theme_minimal(base_size = 20) + 
  theme(legend.position="none", plot.margin=unit(c(2,2,2.5,2.2),"cm"),
        text=element_text(size=16,  family="Times New Roman"), 
        plot.title = element_text(size=28, face="bold"), 
        axis.line = element_line(colour = "grey50", size = 1),
        strip.text = element_text(size=16),
        axis.title  = element_text(size=20),
        plot.subtitle = element_text(size=22),
        plot.caption = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16)) + 
  geom_hline(yintercept=0, linetype="dashed",) +
  geom_vline(xintercept=as.numeric(uru_gral$fecha[19]), size=1) +  ylim(-90, 90) +
  scale_x_date(date_labels = "%b/%d", date_breaks = "2 weeks") +
  labs(x = "Fecha", y = "Movilidad respecto a línea de base", 
       title = "Movilidad en Uruguay",
       subtitle = "Línea de base = 03-01 a 06-02",
       caption = "Fuente: Google Mobility Reports")
plot1 + scale_color_brewer(palette="Dark2")

# Exportar el gráfico
setwd()
ggsave(file="movilidad_uruguay.png",width = 40, height = 28, units = "cm")

# Exportar base "uru_gral" en excel
write_xlsx(uru_gral, "uruguay_movilidad.xlsx")

# La base mvd contiene la información para el departamento de Montevideo únicamente 
mvd <- uru_reshape %>% filter(sub_region_1 == "Montevideo Department")

# Grafico de movilidad para Montevideo
plot2 <- ggplot(mvd, aes(fecha, movilidad, colour = tipo)) +
  geom_line(linetype = "solid", size=1.3) +
  geom_point(data = mvd[mvd$fecha == max(mvd$fecha),],
             aes(x =fecha, color = tipo), show.legend = FALSE, size=3)+
  geom_text(data = mvd[mvd$fecha == max(mvd$fecha),],
            aes(x =fecha, label=movilidad,
                vjust = -1, fontface = "bold"))+ 
  facet_wrap(~ tipo) + theme_minimal(base_size = 16) + 
  theme(legend.position="none", plot.margin=unit(c(2,2,2.5,2.2),"cm"),
        text=element_text(size=16,  family="Times New Roman"), 
        plot.title = element_text(size=28, face="bold"), 
        axis.line = element_line(colour = "grey50", size = 1),
        strip.text = element_text(size=16),
        axis.title  = element_text(size=20),
        plot.subtitle = element_text(size=22),
        plot.caption = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16)) + 
  geom_hline(yintercept=0, linetype="dashed",) +
  geom_vline(xintercept=as.numeric(uru_gral$fecha[19]), size=1) +  ylim(-90, 90) +
  scale_x_date(date_labels = "%b/%d", date_breaks = "2 weeks") +
  labs(x = "Fecha", y = "Movilidad respecto a línea de base", 
       title = "Movilidad en Montevideo",
       subtitle = "Línea de base = 03-01 a 06-02",
       caption = "Fuente: Google Mobility Reports")

plot2 + scale_color_brewer(palette="Dark2")

# Exportar el gráfico
ggsave(file="movilidad_montevideo.png",width = 40, height = 28, units = "cm")

# Exportar base "mvd" en excel
write_xlsx(mvd, "mvd_movilidad.xlsx")

# Subset de la base solo para movilidad a lugares de trabajo
uru_trab <- uru_gral %>% filter(tipo=="Lugares de trabajo")
# Quito fines de semana, turismo, 1 de mayo y carnaval
uru_trab$entresemana <- !(weekdays(as.Date(uru_trab$fecha)) %in% c('Saturday','Sunday')) 
uru_trab <- uru_trab %>% filter(entresemana=="TRUE")
uru_trab <- uru_trab %>% filter(!fecha=="2020-05-01")
uru_trab <- uru_trab %>% filter(!fecha=="2020-02-24")
uru_trab <- uru_trab %>% filter(!fecha=="2020-02-25")
uru_trab <- uru_trab %>% filter(!fecha=="2020-04-09")
uru_trab <- uru_trab %>% filter(!fecha=="2020-04-10")


# Ahora dividimos en tres etapas: normalidad, cuarentena y nueva normalidad
uru_trab$etapa <- "Cuarentena no obligatoria"
uru_trab$etapa <- ifelse(uru_trab$fecha < as.POSIXct("2020-03-13"), "Normalidad", uru_trab$etapa)
uru_trab$etapa <- ifelse(uru_trab$fecha > as.POSIXct("2020-04-17"), "Nueva normalidad", uru_trab$etapa)
uru_trab$etapa <- factor(uru_trab$etapa, c("Normalidad", "Cuarentena no obligatoria", "Nueva normalidad"), ordered=TRUE)

# Gráfico de movilidad laboral con tendencia
plot3 <- ggplot(uru_trab, aes(fecha, movilidad, color=etapa)) +
  geom_point(aes(colour=etapa, alpha = 0.5, size = 4)) +
  geom_smooth(colour="Grey",alpha=.25,method = "loess",  se = T)+
  theme_minimal() + 
  theme(legend.position="none", plot.margin=unit(c(2,2,2.5,2.2),"cm"),
        text=element_text(size=16,  family="Times New Roman"), 
        plot.title = element_text(size=28, face="bold"), 
        axis.line = element_line(colour = "grey50", size = 1),
        strip.text = element_text(size=16),
        axis.title  = element_text(size=20),
        plot.subtitle = element_text(size=22),
        plot.caption = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16)) + 
  geom_hline(yintercept=0, linetype="dashed",) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-13"))) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-04-17"))) +
  annotate(geom="text",x=as.Date("2020-02-25"), y=29,label="Normalidad", size=8, family="Times New Roman") +
  annotate(geom="text",x=as.Date("2020-03-30"), y=29,label="Cuarentena no obligatoria", size=8, family="Times New Roman") +
  annotate(geom="text",x=as.Date("2020-04-28"), y=29,label="Nueva normalidad", size=8, family="Times New Roman") +
  scale_x_date(date_labels = "%b/%d", date_breaks = "1 week") +
  labs(x = "Fecha", y = "Movilidad respecto a línea de base", 
       title = "Movilidad hacia lugares de trabajo en Uruguay en días hábiles",
       subtitle = "Línea de base = 03-01 a 06-02",
       caption = "Fuente: Google Mobility Reports")

plot3 + scale_color_manual(values=c("darkgreen", "sienna2", "orange")) 

# Exportar el gráfico
ggsave(file="movilidad_etapas.png",width = 40, height = 28, units = "cm")

plot4 <- ggplot(uru_trab, aes(fecha, movilidad, color=etapa)) +
  geom_point(aes(colour=etapa, alpha = 0.5, size = 4)) +
  geom_smooth(aes(color=etapa, fill=etapa),alpha=.25,method = "loess",  se = T)+
  theme_minimal(base_size = 16) + 
  theme(legend.position="none", plot.margin=unit(c(2,2,2.5,2.2),"cm"),
        text=element_text(size=16,  family="Times New Roman"), 
        plot.title = element_text(size=28, face="bold"), 
        axis.line = element_line(colour = "grey50", size = 1),
        strip.text = element_text(size=16),
        axis.title  = element_text(size=20),
        plot.subtitle = element_text(size=22),
        plot.caption = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16)) + 
  geom_hline(yintercept=0, linetype="dashed",) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-13"))) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-04-17"))) +
  annotate(geom="text",x=as.Date("2020-02-25"), y=29,label="Normalidad", size=8, family="Times New Roman") +
  annotate(geom="text",x=as.Date("2020-03-30"), y=29,label="Cuarentena no obligatoria", size=8, family="Times New Roman") +
  annotate(geom="text",x=as.Date("2020-04-28"), y=29,label="Nueva normalidad", size=8, family="Times New Roman") +
  scale_x_date(date_labels = "%b/%d", date_breaks = "1 week") +
  labs(x = "Fecha", y = "Movilidad respecto a línea de base", 
       title = "Movilidad hacia lugares de trabajo en Uruguay en días hábiles",
       subtitle = "Línea de base = 03-01 a 06-02",
       caption = "Fuente: Google Mobility Reports")

plot4 + scale_color_manual(values=c("darkgreen", "sienna4", "darkorange3")) + 
  scale_fill_manual(values=c("olivedrab3", "firebrick1", "darkorange1"))

# Exportar el gráfico
ggsave(file="movilidad_etapas2.png",width = 40, height = 28, units = "cm")
