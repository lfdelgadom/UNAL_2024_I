# ---- CARGA DE DATOS DE CO2 ----
# Carga las bibliotecas necesarias para la manipulación y visualización de datos.
library(readr) # Para leer archivos de datos.
library(tidyverse) # Para manipulación de datos y gráficos.

# Lee los datos del archivo CO2.csv, asumiendo que los datos están delimitados por tabuladores.
Co2 <- read_delim("data/CO2.csv", 
                  delim = "\t", 
                  escape_double = FALSE, 
                  trim_ws = TRUE)

# Utiliza View para inspeccionar los datos en un formato tabular dentro del RStudio.
View(Co2)

# ---- VISUALIZACIÓN DE DATOS DE CO2 ----
# Crea un gráfico boxplot para visualizar la distribución de la absorción (uptake) de CO2,
# agrupado por tratamiento (Treatment), y luego divide los datos en facetas basadas en el tipo (Type).
Co2 %>%
  ggplot(aes(x = Treatment, y = uptake)) + # Define las estéticas: x como tratamiento y y como absorción.
  geom_boxplot() + # Añade un boxplot para mostrar la distribución de los datos.
  geom_jitter(width = 0.1) + # Añade puntos de datos individuales como "jitter" para evitar superposición.
  facet_wrap(~Type) # Divide el gráfico en paneles separados para cada tipo.



