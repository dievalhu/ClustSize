# Cargar bibliotecas
if (!require("cluster")) install.packages("cluster")
library(cluster)

library(readr)
dataset <- read_csv("C:/Users/Dell/OneDrive - Universidad Politecnica Salesiana/Escritorio/Experimentation DATA 2025/Datasets/Grandes/turkiye.csv")

# Seleccionar las columnas numéricas para el análisis
X <- dataset[, 3:33]
y <- dataset$class

# Definir el número de elementos en cada clúster
E <- c(775,1444,3601)
K = length(E)
cl = pam(X, K)
C = cl$id.med

# Calcular la matriz de distancias
if (!require("proxy")) install.packages("proxy")
library(proxy)
D <- proxy::dist(as.matrix(X), method = "cosine")
D = as.matrix(D)

###########################################################################

SC_medoids <- function(D, k, E, C = NULL) {
  if (is.null(C)) {
    C <- sample(1:nrow(D), k)
  }
  
  # Inicialmente, asigna cada punto al medoide más cercano
  cl <- max.col(-D[, C, drop = FALSE])
  
  # Ordena los puntos por su distancia al medoide más cercano
  sorted_points <- order(apply(D[, C, drop = FALSE], 1, min))
  
  # Asigna los primeros E[i] puntos a cada grupo i
  for (i in 1:k) {
    cl[sorted_points[1:E[i]]] <- i
    sorted_points <- sorted_points[-(1:E[i])]
  }
  
  # Asigna los puntos restantes al medoide más cercano
  for (point in sorted_points) {
    cl[point] <- which.min(D[point, C])
  }
  
  # Asignar etiquetas de cluster a cada punto
  labels <- numeric(nrow(D))
  for (i in 1:k) {
    ii <- which(cl == i)
    labels[ii] <- i
  }
  
  # Devolver resultados: medoides, clustering y etiquetas
  return(list(medoids = C, clustering = cl, labels = labels))
}

result <- SC_medoids(D, K, E, C)
label_pred <- result$labels  # La asignación de puntos a clusters

###########################################################################
# Calcular y mostrar el tamaño de los grupos reales
size_real <- table(y)
cat("Tamaño de los Grupos Real:\n")
print(size_real)

# Calcular y mostrar el tamaño de los grupos calculados
size_calc <- table(label_pred)
cat("Tamaño de los Grupos Calculado:\n")
print(size_calc)

###########################################################################
# Calculo de la silueta
silhouette_values <- silhouette(x = label_pred, dist = as.dist(D))
mean_silhouette <- mean(silhouette_values[, "sil_width"])

# Mostrar el promedio del coeficiente de silueta
print(paste("Promedio del coeficiente de silueta:", mean_silhouette))

###########################################################################
# Normalized Mutual Information(NMI) y Adjusted Mutual Information(AMI). 
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