# ---- INSTALACIÓN DE GGPLOT2 ----
# La siguiente línea, si se descomenta, instalará ggplot2, una biblioteca para la visualización de datos.
#install.packages("ggplot2")
library(ggplot2) # Carga la biblioteca ggplot2.

# ---- CONJUNTO DE DATOS IRIS ----

data(iris) # Carga el conjunto de datos iris.
summary(iris) # Muestra un resumen estadístico del conjunto de datos iris.
head(iris, 10) # Muestra las primeras 10 filas del conjunto de datos iris.
tail(iris) # Muestra las últimas 6 filas del conjunto de datos iris.
dim(iris) # Muestra las dimensiones del conjunto de datos iris.

# ---- GRÁFICOS DEL CONJUNTO DE DATOS IRIS ----
# Gráficos básicos usando las funciones de plot de base en R.
plot(iris) # Crea un gráfico de pares para todas las variables en el conjunto de datos iris.
plot(iris$Petal.Length, iris$Sepal.Width) # Gráfico de dispersión entre longitud del pétalo y ancho del sépalo.
plot(iris$Petal.Width, iris$Sepal.Length, 
     ylab = "Longitud del sépalo (cm)", 
     xlab = "Ancho del pétalo (cm)", 
     col = "purple") # Gráfico de dispersión personalizado con colores y etiquetas.

# ---- VISUALIZACIÓN CON GGPLOT2 ----

# Gráfico de puntos básico.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point()

# Gráfico de puntos con jitter (ruido aleatorio) para evitar superposiciones.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_jitter(width = 0.3, height = 0.5)

# Gráfico de puntos con facetas para cada especie del conjunto de datos iris.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species)

# Añadiendo líneas de tendencia lineal por especie sin bandas de confianza en color rojo.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species) +
  geom_smooth(method = "lm", se = FALSE, col = "red")

# Ejemplo de cómo personalizar escalas y temas en ggplot.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species) +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  scale_y_continuous("Ancho del Sépalo", breaks = seq(0, 5, 1)) +
  theme_bw() # Aplica un tema en blanco y negro.

# ---- PERSONALIZACIÓN DEL COLOR ----

# Gráfico de puntos coloreados por especie.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, color = Species)) +
  geom_point()

# Gráfico de puntos con tamaño de puntos basado en el ancho del pétalo.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, color = Species, size = Petal.Width)) +
  geom_point()

# Gráfico de puntos con jitter, coloreado por especie, tamaño basado en el ancho del pétalo y forma basada en la especie.
plot_1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, color = Species, size = Petal.Width, shape = Species)) +
  geom_jitter()

# ---- GUARDAR EL ÚLTIMO GRÁFICO ----
ggsave(
  filename = "images/scatter_plot_iris.png",
  plot = plot_1, # Guarda el gráfico almacenado en plot_1.
  width = 8, height = 6, units = "in", dpi = 300
)


# ---- CREACIÓN Y VISUALIZACIÓN DE UN GRÁFICO COMPLEJO CON GGPLOT2 ----
grafico_completo <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, 
                                     col = Species, size = Petal.Width, 
                                     shape = Species, alpha = Sepal.Length)) +
  geom_point() # Gráfico de puntos con varias estéticas personalizadas.
grafico_completo # Muestra el gráfico completo.

# ---- GUARDAR EL GRÁFICO COMPLETO ----
ggsave(
  filename = "images/scatter_complete_iris.png",
  plot = grafico_completo, # Guarda el gráfico completo en un archivo.
  width = 8, height = 6, units = "in", dpi = 300
)

# ---- CONTAR LA FRECUENCIA DE ESPECIES EN EL DATASET IRIS ----
#count(iris, Species) # Cuenta la frecuencia de cada especie en el conjunto de datos iris.

# ---- HACER INTERACTIVO EL GRÁFICO COMPLETO CON PLOTLY ----
#install.packages("plotly") # Instala el paquete Plotly si es necesario.
library(plotly) # Carga la biblioteca Plotly.
plotly::ggplotly(plot_1) # Convierte el gráfico de ggplot2 en una versión interactiva con Plotly.

# ---- BARRAS Y BOXPLOTS CON GGPLOT2 Y DPLYR ----
library(dplyr) # Carga la biblioteca dplyr para manipulación de datos.

# Resumen de longitud de sépalos y pétalos por especie.
resumen <- iris %>%
  group_by(Species) %>%
  summarise(mean_sepal_length = mean(Sepal.Length), mean_petal_length = mean(Petal.Length))

# Gráfico de columnas para longitud media de sépalos por especie.
ggplot(resumen, aes(x = Species, y = mean_sepal_length)) +
  geom_col()

# Diferentes formas de crear gráficos de barras para la longitud media de sépalos por especie.
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean")
ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_bar(stat = "summary", fun.y = "mean")
ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue")
ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "#00166e", col = "black") + 
  geom_boxplot()

# Creación y guardado de un boxplot con jitter para longitud de sépalos por especie.
bxp <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)
ggsave(paste0("images/", "bxp.png"), plot = bxp, dpi = 300, height = 6, width = 8)

# ---- DEMOSTRACIÓN DE LAS DIFERENTES FORMAS DE PUNTOS EN R ----
d = data.frame(p = c(0:25))
ggplot() +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  scale_shape_identity() +
  geom_point(data = d, mapping = aes(x = p%%16, y = p%/%16, shape = p), size = 5, fill = "red") +
  geom_text(data = d, mapping = aes(x = p%%16, y = p%/%16 + 0.25, label = p), size = 3)

# Personalización de temas y combinación de geométricos.

# ---- CREACIÓN DE UN GRÁFICO PERSONALIZADO CON GGPLOT2 ----
# Este bloque crea un gráfico de barras que muestra la longitud media de los sépalos por especie en el conjunto de datos iris,
# y añade un gráfico de dispersión (jitter) para visualizar la distribución de los datos individuales.
myPlot <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "#8cb87b", col = "black") +
  geom_jitter(size = 4, aes(shape = Species, color = Species))

# Personalización del tema del gráfico para mejorar la estética.
myPlot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(colour = "black", fill = NA, size = 0.2))

# Aplicación de diferentes temas a `myPlot` para explorar estilos visuales.
myPlot + theme_bw()
myPlot + theme_classic()
myPlot + theme_dark()
myPlot + theme_gray()
myPlot + theme_light()
myPlot + theme_linedraw() + theme(panel.background = element_rect(fill = "blue"))
myPlot + theme_minimal()
myPlot + theme_void()

# Creación de un boxplot para la longitud de los sépalos por especie en el conjunto de datos iris, incluyendo una visualización de los datos individuales con jitter.
# Se corrige la línea con el parámetro 'notch' incompleto en la versión original.
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(col = "black", notch = TRUE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_bw() +
  labs(title = "Boxplot del conjunto de datos Iris", caption = "Universidad Nacional de Colombia",
       y = "Longitud de sépalo (cm)") +
  theme(plot.title = element_text(hjust = 0.5))

# ---- TOQUES FINALES ----
# Se aplican toques finales al gráfico `myPlot`, incluyendo un cambio de tema y ajustes de etiquetas y título.
myPlot + 
  theme_classic() +
  labs(x = "", y = "Longitud del sépalo (mm)") +
  ggtitle("Longitud del sépalo por especie de Iris") +
  theme(plot.title = element_text(hjust = 0.5))

# ---- GUARDAR EL GRÁFICO ----
ggsave("Graph 1.png", width = 8, height = 5)

# ---- VISUALIZACIÓN DEL CONJUNTO DE DATOS TOOTHGROWTH ----
# Carga y revisión preliminar del conjunto de datos ToothGrowth.
data(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)

# Creación de gráficos para explorar el efecto del suplemento y la dosis en el crecimiento del diente.
# Primer gráfico: Barras representando la mediana de longitud de diente por tipo de suplemento, con diferenciación por dosis.
ggplot(ToothGrowth, aes(supp, len, fill = as.factor(dose))) +
  geom_bar(stat = "summary", fun.y = "median", col = "black", position = "dodge") +
  geom_point(position = position_dodge(0.9))

# Segundo gráfico: Líneas representando la media de longitud de diente por dosis, diferenciadas por tipo de suplemento.
ggplot(ToothGrowth, aes(as.factor(dose), len, group = supp, col = supp)) +
  geom_line(stat = "summary", fun.y = "mean") +
  geom_smooth(method = "lm")
