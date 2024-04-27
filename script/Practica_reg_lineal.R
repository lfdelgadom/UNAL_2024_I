# Load required libraries
library(tidyverse)
library(ggpubr)

# Load and inspect data
Datos_temp <- read.csv("../Reg_lineal/Data/Temperatura_suelo.csv", sep = ";")
print(head(Datos_temp))

# Compute and plot correlation matrix
Datos_temp %>%
  cor() %>%
  round(2) %>%
  print()

plot(Datos_temp)

# Basic scatter plot with ggplot2
ggplot(data = Datos_temp, aes(x = Cobertura, y = Temperatura)) +
  geom_point(size = 3, colour = "red", shape = 14) +
  geom_text(aes(label = paste("r =", round(cor(Cobertura, Temperatura), 3)), x = 90, y = 30)) +
  geom_text(aes(label = "Author: Luis Delgado"), x = 60, y = 20, size = 3) +
  labs(x = "Cobertura (%)", y = "Temperatura (C)", title = "Temp vs Cobertura") +
  theme_bw()

# Linear model
modelo <- lm(Temperatura ~ Cobertura, data = Datos_temp)
summary(modelo)
anova(modelo)

# fitted values
modelo$fitted.values

# errores
modelo$residuals

# Coeficientes
b0 = modelo$coefficients[1]###intercepto en y
b1 = modelo$coefficients[2]#Pendiente

#inpresion de los coeficientes
print(b0)
print(b1)


# Coefficients and predictions
coef(modelo)
Datos_temp <- Datos_temp %>%
  bind_cols(predict(modelo, interval = "prediction", level = 0.9))

# Plot with predictions and confidence intervals
Datos_temp %>% ggplot(aes(x = Cobertura, y = Temperatura)) +
  geom_point(size = 3) +
  geom_line(aes(y = fit), color = "blue") +
  geom_point(aes(y = fit), size = 2, color = 'red') +
  geom_segment(aes(xend = Cobertura, yend = fit), color = 'blue') +
  geom_line(aes(y = lwr), color = 'red') +
  geom_line(aes(y = upr), color = 'red') +
  geom_text(aes(label = paste("R^2=", round(cor(Cobertura, Temperatura)^2, 3))),
            x = 80, y = 30) +
  geom_text(aes(label = "Author: Luis Delgado"), x = 60, y = 20, size = 3) +
  labs(x = "Cobertura (%)", y = "Temperatura (C)", title = "Temp vs Cobertura") +
  theme_minimal()

# Save plot
ggsave(paste0("images/COR_ggplot", Sys.Date(), ".png"), 
       units = "in", dpi = 300, width = 6, height = 4)

# Advanced scatter plot with regression line and Pearson correlation
st <- ggscatter(Datos_temp, x = "Cobertura", y = "Temperatura",
                add = "reg.line", conf.int = TRUE,
                shape = 21, add.params = list(color = "blue", fill = "lightgray")) +
  stat_cor(method = "pearson", label.x = 60, label.y = 35)

st

# Save scatter plot
ggsave(paste0("images/COR", Sys.Date(), ".png"),
       plot = st, units = "in", dpi = 300, width = 6, height = 4)

 
# Predict temperature at 80% coverage
predicted_temp <- coef(modelo)[1] + coef(modelo)[2] * 80
predicted_temp


# Tarea, hacer este mismo proceso con los datos de materia seca de cassava.








