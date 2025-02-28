# Cargar bibliotecas
if (!require("cluster")) install.packages("cluster")
if (!require("mice")) install.packages("mice")
library(cluster)
library(mice)

library(readr)
dataset <- read_csv("C:/Users/Dell/OneDrive - Universidad Politecnica Salesiana/Escritorio/Experimentation DATA 2025/Datasets/Medianos/Water.csv")

# Seleccionar las columnas numéricas para el análisis
X <- dataset[, 1:9]
imp <- mice(X, method = "pmm", m = 5, maxit = 50, seed = 500)
X <- complete(imp, 1)
y <- dataset$Potability

# Definir el número de elementos en cada clúster
k = length(table(y))

# Calcular la matriz de distancias
if (!require("proxy")) install.packages("proxy")
library(proxy)
D <- proxy::dist(as.matrix(X), method = "cosine")
D = as.matrix(D)
distancia = D
D1 = as.dist(distancia)
#===========================================================================================================
#PROCESO ALGORITMO AHC

r <- nrow(D) # Número total de documentos
hc <- hclust(D1, method = "complete")
label_pred <- cutree(hc, k)
#================================================================================================================
silhouette_values <- silhouette(x = label_pred, dist = as.dist(distancia))
mean_silhouette <- mean(silhouette_values[, "sil_width"])

# Mostrar el promedio del coeficiente de silueta
print(paste("Promedio del coeficiente de silueta:", mean_silhouette))

# Cargar las bibliotecas necesarias
#=================================================================================================
# Calcular y mostrar el tamaño de los grupos reales
size_real <- table(y)
cat("Tamaño de los Grupos Real:\n")
print(size_real)

# Calcular y mostrar el tamaño de los grupos calculados
size_calc <- table(label_pred)
cat("Tamaño de los Grupos Calculado:\n")
print(size_calc)
#=============================================================================================
if (!require("aricode")) install.packages("aricode")

# Cargar bibliotecas necesarias
library(aricode)

# Calcular ARI
ARI <- ARI(y, label_pred)
cat("Adjusted Rand Index (ARI): ", ARI, "\n")

# Calcular AMI
AMI <- AMI(y, label_pred)
cat("Adjusted Mutual Information (AMI): ", AMI, "\n")

# Calcular NMI
NMI <- NMI(y, label_pred)
cat("Normalized Mutual Information (NMI): ", NMI, "\n")

