#Ejemplo: Un ingeniero esta interesado en determinar las condiciones
#de operaci?n que maximizan el rendimiento de un proceso.Dos variables 
#controlables influyen en el rendimiento del proceso:  El tiempo de reacci?n
#Temperatura de reacci?n.El ingeniero opera actualmente el proceso con un 
#tiempo de reacci?n de 35 minutos y una temperatura de 155?F, que dan como 
#resultado rendimientos de cerca de 40%. 

#Cargar datos
library(readxl)
(datos <- read_excel("Data/datos_rsm_ccc.xlsx", sheet = 1))
View(datos)

# Codificamos los factores
#install.packages("rsm")
library(rsm)
?rsm # ayuda
datos_ccc <-  coded.data(data = datos, 
                         x1 ~ (tiempo - 35)/5, x2 ~ (temperatura - 155)/5)


# Grafico del dise?o
plot(datos_ccc[ ,c(2:3)], pch = 19, main = "Diseno factorial 2^2")

# Modelo de primer orden
modelo1 <- rsm(y1 ~ FO(x1, x2) + TWI(x1, x2), data = datos_ccc)
summary(modelo1)

# interaccion no significativa
# Modelo de primer orden sin interaccion
modelo2 <- rsm(y1 ~ FO(x1, x2), data = datos_ccc)
summary(modelo2)

# Superficie 3D 
par(mfrow = c(1,2))
persp(modelo2, x2 ~ x1, 
      zlab = "Rendimiento(%)", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(modelo2$canonical$xs)),
      theta = -30, # coordenadas graficas
      phi = 10)

# Grafico de contornos
contour(modelo2, ~ x1 + x2, image = TRUE) 
points(datos$tiempo, datos$temperatura)
dev.off()

#Analisis (modelo de segundo orden)
#Cargar datos
datos_2 <- read_excel("Data/datos_rsm_ccc.xlsx", sheet = 2)
datos_2

# Codificamos los factores
datos_cc2 <- coded.data(data = datos_2, 
                        x1 ~ (tiempo - 85)/5, x2 ~ (temperatura - 175)/5)
as.data.frame(datos_cc2)

# Grafico del dise?o
plot(datos_cc2[ , c(2:3)], pch = 16, main = "Diseno CCC")
abline(h = c(1, -1), col = "lightgrey")
abline(v = c(1, -1) , col = "lightgrey")

# Modelo de primer orden
modelo1.y1 <- rsm(y1 ~ FO(x1, x2) + TWI(x1, x2), data = datos_cc2)
summary(modelo1.y1)

# Modelo polinomico de segundo grado
modelo2.y1 <- rsm(y1 ~ FO(x1, x2) + TWI(x1, x2) + PQ(x1, x2), data = datos_cc2)
summary(modelo2.y1)

# Modelo sin interacciones
modelo2.y1.sinI <- rsm(y1 ~ FO(x1, x2) + PQ(x1, x2), data = datos_cc2)
summary(modelo2.y1.sinI)


# Superficie 3D 
pdf(file = "rsm_single.pdf",
    width = 6, # The width of the plot in inches
    height = 4)

par(mfrow = c(1,2)) 
persp(modelo2.y1.sinI, x2 ~ x1, 
      zlab = "Rendimiento(%)", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(modelo2.y1.sinI$canonical$xs)),
      theta = -35, # coordenadas graficas
      phi = 20)

# Grafico de contornos
contour(modelo2.y1.sinI, ~ x1 + x2, image = TRUE)
points(datos_2$tiempo, datos_2$temperatura)
dev.off()



# Valor predicho de rendimiento (%) en el punto estacionario
predict(modelo2.y1.sinI, coded.data(data.frame(tiempo = 86.8, 
                                               temperatura = 176.28),
                                    x1 ~ (tiempo - 85)/5, 
                                    x2 ~ (temperatura - 175)/5))
