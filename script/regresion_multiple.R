# Load necessary libraries
library(readxl)
library(tidyverse)
library(agriutilities) #install first

# Load data from Excel
aji_regresion_multiple <- read_excel("Data/aji regresion multiple.xlsx")

# Visualize the dataframe
View(aji_regresion_multiple)

# Display the names of the dataframe columns
names(aji_regresion_multiple)

# Attach the dataframe to the R search path
attach(aji_regresion_multiple)

# Compute correlations among dataframe variables
aji_regresion_multiple %>% 
  cor() %>% 
  corrplot(
    type = 'upper', 
    order = 'hclust',
    tl.col = 'black', 
    tl.srt = 45
  )

# Create a scatterplot matrix
aji_regresion_multiple %>% 
  pairs()

# Load additional libraries for performance analysis
library(PerformanceAnalytics)
library(psych)

# Chart correlation with histogram using PerformanceAnalytics
chart.Correlation(aji_regresion_multiple, histogram = TRUE, pch = 19)

# Plot pairwise panels with Pearson correlation
pairs.panels(
  aji_regresion_multiple,
  method = "pearson",
  density = FALSE,
  ellipses = FALSE,
  smooth = FALSE
)

# Shapiro-Wilk test for normality
shapiro.test(hcancmm)
hist(hcancmm)  # Plot histogram for visual normality check

shapiro.test(hmanchcm)
hist(hmanchcm)  # Plot histogram for visual normality check

# Lilliefors test for normality
library(nortest)
lillie.test(hcancmm)
lillie.test(hmanchcm)

# Multiple regression models
RLM_vacio <- lm(rto ~ 1, data = aji_regresion_multiple)  # Empty model
summary(RLM_vacio)

RLM_Completo <- lm(rto ~ ., data = aji_regresion_multiple)  # Full model
summary(RLM_Completo)

# Stepwise model selection approaches
RLM_Forward <- step(RLM_vacio, scope = list(lower = RLM_vacio, upper = RLM_Completo), direction = "forward")
summary(RLM_Forward)

RLM_Backward <- step(RLM_Completo, scope = list(lower = RLM_vacio, upper = RLM_Completo), direction = "backward")
summary(RLM_Backward)

RLM_Stepwise <- step(RLM_vacio, scope = list(lower = RLM_vacio, upper = RLM_Completo), direction = "both")
summary(RLM_Stepwise)

#modelo sin variables significativas
RLM_propio <- lm(rto ~ pesofrugr + nfru + diasfl + diasfr)
summary(RLM_propio)




#--------------------------------------------------------
#-------------Regresion con variables cualitativas-------
#--------------------------------------------------------

library(readxl)
Data_cuali <- read_excel("Data/Regresion cualitativa.xlsx")
View(Data_cuali)

attach(Data_cuali)
names(Data_cuali)
library(ggplot2)
x11()
ggplot(
  data = Data_cuali,
  mapping = aes(
    x = Hora_Entrenamiento,
    y = Tiempo_pn
  )
) +
  geom_smooth(method = "lm", formula = y ~ x, se = F, col = "dodgerblue1") +
  geom_point(size = 4, aes(shape = Experiencia)) +
  theme_light()

# MODELO CON VARIABLE DUMMY
model.matrix(Tiempo_pn ~ Hora_Entrenamiento + Experiencia)
Regresion <- lm(Tiempo_pn ~ Hora_Entrenamiento + Experiencia, data = Data_cuali)
summary(Regresion)

# Modelo general
"
Tiempo_pn = 26.3630 - 0.6438*Hora_Entrenamiento -6.6678*Dummy_SI_EXP
"

# modelo de los que NO tienen experiencia; Dummy_No_EXP = 1, Dummy_SI_EXP = 0
"
Tiempo_pn = 26.3630 - 0.6438*Hora_Entrenamiento -6.6678*0
Tiempo_pn = 26.3630 - 0.6438*Hora_Entrenamiento
"

# modelo de los que SI tienen experiencia; Dummy_No_EXP = 0, Dummy_SI_EXP = 1
" 
Tiempo_pn = 26.3630 - 0.6438*Hora_Entrenamiento-6.6678*1
Tiempo_pn = (26.3630-6.6678) - 0.6438*Hora_Entrenamiento
Tiempo_pn = 19.6952 - 0.6438*Hora_Entrenamiento
"

x11()
ggplot(
  data = Data_cuali,
  mapping = aes(
    x = Hora_Entrenamiento,
    y = Tiempo_pn,
    color = Experiencia
  )
) +
  geom_abline(intercept = 26.3630, slope = -0.6438, color = "red") +
  geom_abline(intercept = 19.6952, slope = -0.6438, color = "green") +
  geom_point(size = 4) +
  theme_bw()



#--------------------------------------------------------
#-------------Regresion con variables cualitativas--------
#--------------------------------------------------------
library(readxl)
Data_cuali_venta <- read_excel("Data/Regresion cualitativa.xlsx", sheet = 2)
View(Data_cuali_venta)
attach(Data_cuali_venta)

model.matrix(Ventas ~ Publicidad + Sede)

# Ajuste del modelo sin variables cualitativas
modelo <- lm(Ventas ~ Publicidad, data = Data_cuali_venta)
b0 <- modelo$coefficients[1] ### intercepto en y
b1 <- modelo$coefficients[2] # Pendiente
summary(modelo)
# modelo -> Ventas = 1363.8510 - Publicidad*1.023

library(ggplot2)
x11()
ggplot(
  data = Data_cuali_venta,
  mapping = aes(
    x = Publicidad,
    y = Ventas
  )
) +
  geom_abline(intercept = 1363.8510, slope = -1.023) +
  geom_point(size = 3) +
  geom_text(aes(label = paste("Ventas = 1363.8510 - Publicidad*1.023")),
    x = 200, y = 600
  ) +
  theme_light()
# MODELO CON VARIABLE DUMMY
model.matrix(Ventas ~ Publicidad + Sede)
Regresion_venta <- lm(Ventas ~ Publicidad + Sede, data = Data_cuali_venta)
summary(Regresion_venta)
anova(Regresion_venta)


# Modelo de regresion lineal general

y <- 834.9929 + Publicidad * 3.0388 - SedeB * 684.5002 - SedeC * 1338.6270

# modelo para SedeB = 1, SedeA =0, SedeC = 0
y <- 834.9929 + Publicidad * 3.0388 - SedeB * 684.5002 - SedeC * 1338.6270
y <- 834.9929 + Publicidad * 3.0388 - 1 * 684.5002
y <- (834.9929 - 684.5002) + Publicidad * 3.0388
y <- 150.4927 + Publicidad * 3.0388

# modelo para SedeB = 0, SedeA =0, SedeC = 1
y <- 834.9929 + Publicidad * 3.0388 - SedeB * 684.5002 - SedeC * 1338.6270
y <- 834.9929 + Publicidad * 3.0388 - 0 * 684.5002 - 1 * 1338.6270
y <- (834.9929 - 1338.6270) + Publicidad * 3.0388
y <- -503.6341 + Publicidad * 3.0388

# modelo para SedeB = 0, SedeA =1, SedeC = 0
y <- 834.9929 + Publicidad * 3.0388

# Hacer cada grafica de cada modelo con geom_abline y sus respectivos datos.

x11()
ggplot(
  data = Data_cuali_venta,
  mapping = aes(
    x = Publicidad,
    y = Ventas,
    color = Sede
  )
) +
  geom_abline(intercept = 834.99209, slope = 3.0388, color = "red") +
  geom_abline(intercept = 150.4927, slope = 3.0388, color = "green") +
  geom_abline(intercept = -503.6341, slope = 3.0388, color = "blue") +
  geom_text(aes(label = paste("y = 834.99209 + 3.0388Pub")),
    x = 140, y = 1600
  ) +
  geom_text(aes(label = paste("y = 150.4927 + 3.0388Pub")),
    x = 120, y = 600
  ) +
  geom_text(aes(label = paste("y = -503.6341 + 3.0388Pub")),
    x = 400, y = 600
  ) +
  geom_point(size = 4) +
  theme_light()

data(wine)


library("HDclassif")
data("wine")
