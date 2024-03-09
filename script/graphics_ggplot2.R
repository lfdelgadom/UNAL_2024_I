# ---- INSTALL GGPLOT2 ----
  #install.packages("ggplot2")
  library(ggplot2)

# IRIS DATASET ----

data(iris)
summary(iris) # resumen de la base de datos
head(iris, 10) # muestra las primeras 6 entradas de la base de datos
tail(iris)
dim(iris)

# PLOT IRIS ----
## plot pequeno ----
plot(iris)
plot(iris$Petal.Length, iris$Sepal.Width)

# GGPLOT IRIS ----

# ggplot(data_set, mapping(aes(x = varx, y = vary))) +
#geometrias

#point
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point()

#jitter <- ruido aleatorio
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_jitter()

#facets
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species)

# statistics
#facets
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species) +
  geom_smooth(method = "lm", se = F, col = "red")

# coordinates
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species) +
  geom_smooth(method = "lm", se = F, col = "red") +
  scale_y_continuous("Sepal Width", breaks = seq(0,5, 0.5))

# theme
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species) +
  geom_smooth(method = "lm", se = F, col = "red") +
  scale_y_continuous("Sepal Width", breaks = seq(0,5, 0.5)) +
  theme_bw()


?geom_point

# color
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species, size = Petal.Width)) +
  geom_point()

ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species, 
                 size = Petal.Width, shape = Species)) +
  geom_point()

grafico_completo <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species, 
                                     size = Petal.Width, shape = Species, alpha = Sepal.Length)) +
  geom_point()
grafico_completo

count(iris, Species)

#install.packages("plotly")
library(plotly)

#volver interactico el grafico completo
plotly::ggplotly(grafico_completo)


# BAR AND BOXPLOT ----
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean")

ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_bar(stat = "summary", fun.y = "mean")

ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue")

ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "#14d8a6", col = "black") +
  geom_boxplot() 

#Las diferentes formas de los puntos de R

d=data.frame(p=c(0:25))
ggplot() +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_identity() +
  geom_point(data=d, mapping=aes(x=p%%16, y=p%/%16, shape=p), size=5, fill="red") +
  geom_text(data=d, mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), size=3)


ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "#ff0080", col = "black") +
  geom_jitter(size = 2, shape = 8)

myPlot <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "#8cb87b", col = "black") +
  geom_jitter(size = 4, aes(shape = Species, color = Species))

myPlot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(colour = "black", fill = NA, size = 0.2))

?theme

myPlot 
myPlot + theme_bw()
myPlot + theme_classic()
myPlot + theme_dark()
myPlot + theme_gray()
myPlot + theme_light()
myPlot + theme_linedraw() + theme(panel.background = element_rect(fill = "blue"))
myPlot + theme_minimal()
myPlot + theme_void()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(col = "black", notch = FALSE) +
  geom_jitter(with = 0.1, alpha = 0.5) 

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(col = "black", notch = TRUE) +
  geom_jitter(with = 0.1, alpha = 0.5)  +
  theme_bw() +
  labs(title = "Iris's boxplot", caption = "Universidad Nacional de Colombia",
       y = "Longitud de sepalo (cm)") +
  theme(plot.title = element_text(hjust = 0.5))

# FINISHING TOUCHES ----
myPlot + 
  theme_classic() +
  labs(x = "", y = "Sepal length (mm)") +
  ggtitle("Sepal length by iris species") +
  theme(plot.title = element_text(hjust = 0.5))

# SAVING OUR PLOT ----
#setwd("~/Documents")
ggsave("Graph 1.png", width = 8, height = 5)

# FACTORIAL ----

data(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)
?ToothGrowth

ggplot(ToothGrowth, aes(supp, len, fill = as.factor(dose))) +
  geom_bar(stat = "summary", fun.y = "median", col = "black", position = "dodge") +
  geom_point(position = position_dodge(0.9))

ggplot(ToothGrowth, aes(as.factor(dose), len, group = supp, col = supp)) +
  geom_line(stat = "summary", fun.y = "mean") +
  geom_smooth(method = "lm")

