# mobility-uruguay
## Script de R para descargar y graficar datos de Google de movilidad en Uruguay

### Instalar y cargar paquetes 
`````````
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")
remotes::install_github("kjhealy/covdata")

library(plyr)
library(covdata)
library(ggplot2)
library(tidyverse)
`````````

El paquete "covdata" nos permite descargar la información directamente desde R. 
La base de datos "uru" contiene los cambios en movilidad respecto a la mediana de actividad (de cada día de la semana) entre el 3 de enero y el 6 de febrero para Uruguay en su conjunto y cada departamento por separado

``
uru <-google_mobility %>%
  filter(country_region_code == "UY")
``

![](movilidad_uruguay.png)
