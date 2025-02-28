# Cargar bibliotecas
if (!require("cluster")) install.packages("cluster")
library(cluster)

# Cargar conjunto de datos iris
data(iris)
dataset = iris

# Seleccionar las columnas numéricas para el análisis
X <- iris[, 1:4]
y <- iris$Species

# Definir el número de elementos en cada clúster
k = length(table(y))

# Calcular la matriz de distancias
if (!require("proxy")) install.packages("proxy")
library(proxy)
D <- proxy::dist(as.matrix(X), method = "cosine")
D = as.matrix(D)
distancia = D

#===========================================================================================================
#PROCESO ALGORITMO KMEDOIDS

r <- nrow(D) # Número total de documentos

cl = pam(X, k)
label_pred = cl$cluster
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

