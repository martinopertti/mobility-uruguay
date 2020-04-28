#### Datos de movilidad de Google para Uruguay
## Martín Opertti

install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")
remotes::install_github("kjhealy/covdata")
install.packages("writexl")

library(plyr)
library(covdata)
library(ggplot2)
library(tidyverse)
library(writexl)

rm(list=ls())

## Primero descargamos los datos para Uruguay
# La base de datos "uru" contiene los cambios en movilidad respecto al 3-6 de febrero para Uruguay en su conjunto y cada departamento por separado
uru <-google_mobility %>%
  filter(country_region_code == "UY")

# Traducir nombres de variables y categorías
uru <- uru %>% 
  rename(movilidad = pct_diff, tipo = type, fecha = date, pais = country_region, codigo_pais = country_region_code)

uru$tipo <- revalue(uru$tipo, c("grocery"="Tiendas de comestibles", "parks"="Parques y plazas", "residential" = "Residencias",
                                "retail" = "Otras tiendas", "transit" = "Tránsito", "workplaces" = "Lugares de trabajo"))

# La base uru_gral contiene tiene información solo para Uruguay en su conjunto (sin data por departamento)
uru_gral <- uru %>% filter(is.na(sub_region_1))

# Gráfico de movilidad para el total de Uruguay
plot1 <- ggplot(uru_gral, aes(fecha, movilidad, colour = tipo)) +
  geom_line(linetype = "solid", size=1.3) +
  geom_point(data = uru_gral[uru_gral$fecha == max(uru_gral$fecha),],
             aes(x =fecha, color = tipo), show.legend = FALSE, size=3)+
  geom_text(data = uru_gral[uru_gral$fecha == max(uru_gral$fecha),],
            aes(x =fecha, label=movilidad,
            vjust = -2, fontface = "bold"))+ 
  facet_wrap(~ tipo) + theme_minimal(base_size = 16) + 
  theme(legend.position="none", plot.margin=unit(c(2,2,2.5,2.2),"cm")) + ylim(-80, 80)
plot1 + scale_color_brewer(palette="Dark2")

# Exportar el gráfico
setwd()
ggsave(file="movilidad_uruguay.png",width = 40, height = 28, units = "cm")

# Exportar base "uru_gral" en excel
write_xlsx(uru_gral, "uruguay_movilidad.xlsx")

# La base mvd contiene la información para el departamento de Montevideo únicamente 
mvd <- uru %>% filter(sub_region_1 == "Montevideo Department")

# Grafico de movilidad para Montevideo
plot2 <- ggplot(mvd, aes(fecha, movilidad, colour = tipo)) +
  geom_line(linetype = "solid", size=1.3) +
  geom_point(data = mvd[mvd$fecha == max(mvd$fecha),],
             aes(x =fecha, color = tipo), show.legend = FALSE, size=3)+
  geom_text(data = mvd[mvd$fecha == max(mvd$fecha),],
            aes(x =fecha, label=movilidad,
                vjust = -2, fontface = "bold"))+ 
  facet_wrap(~ tipo) + theme_minimal(base_size = 16) + 
  theme(legend.position = "none", plot.margin=unit(c(2,2,2.5,2.2),"cm")) + ylim(-80, 80)
plot2 + scale_color_brewer(palette="Dark2")

# Exportar el gráfico
ggsave(file="movilidad_montevideo.png",width = 40, height = 28, units = "cm")

# Exportar base "mvd" en excel
write_xlsx(mvd, "mvd_movilidad.xlsx")
