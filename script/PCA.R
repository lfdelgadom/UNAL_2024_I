# Cargar las bibliotecas necesarias
library(tidyverse)
library(readr)
library(factoextra)
library(FactoMineR)
library(agriutilities)

# Leer datos del archivo CSV
data_soil <- read_csv("data/data_soil.csv")

# Crear una matriz de correlación y guardar la visualización
data_soil_cor <- gg_cor(
  data = data_soil,
  colours = c("red", "white", "blue"),  # Colores para la matriz
  label_size = 1.5  # Tamaño de etiqueta en la matriz de correlación
)

data_soil_cor

# Guardar la imagen de la matriz de correlación
ggsave("images/corplot_soils.png", plot = data_soil_cor, width = 12, height = 10, dpi = 300)

# Realizar el análisis de componentes principales (PCA)
pca_data_soil <- data_soil %>%
  dplyr::select(-Finca) %>%  # Excluir la columna 'Finca'
  column_to_rownames("Descripcion") %>>%  # Establecer 'Descripcion' como nombres de filas
  PCA(scale.unit = TRUE, graph = FALSE)  # Ejecutar PCA con escalado y sin gráficos automáticos

# Acceder a los valores propios y contribuciones de variables del PCA
pca_data_soil$eig
pca_data_soil$var$contrib
pca_data_soil$var

# Visualizar las variables del PCA y guardar la imagen
var_plot <- fviz_pca_var(pca_data_soil, col.var = "contrib", repel = TRUE) +
  labs(title = "PCA Soil Variables")
ggsave("images/PCA_var_soil.png", plot = var_plot, units = "in", dpi = 300, width = 8, height = 6)

# Visualizar y mostrar el gráfico de las variables del PCA
print(var_plot)

# Visualizar y mostrar el gráfico de los individuos en el PCA
IND_soil <- fviz_pca_ind(pca_data_soil, repel = TRUE, alpha.ind = 0.3, 
                         col.ind = "grey20", labelsize = 2) +
  ggtitle("Individuals - PCA - Soil")
print(IND_soil)

# Crear y mostrar un biplot del PCA
BI_soil <- fviz_pca_biplot(pca_data_soil, repel = TRUE, alpha.ind = 0.3, 
                           col.ind = "grey30", labelsize = 3, 
                           col.var = "black", geom = c("point")) +
  ggtitle("Biplot - PCA - Soil")
print(BI_soil)

# Realizar PCA en otro conjunto de datos excluyendo 'Descripcion'
pca_finca <- data_soil %>%
  dplyr::select(-Descripcion) %>%
  PCA(scale.unit = TRUE, graph = FALSE, quali.sup = 1)

# Visualizar y mostrar los resultados del PCA con individuos de 'Finca'
ind_finca <- fviz_pca_ind(pca_finca, label = "none", habillage = 1, addEllipses = TRUE)
print(ind_finca)
