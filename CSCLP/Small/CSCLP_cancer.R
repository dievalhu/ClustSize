# Cargar bibliotecas
if (!require("cluster")) install.packages("cluster")
library(cluster)

library(readr)
dataset <- read_csv("C:/Users/Dell/OneDrive - Universidad Politecnica Salesiana/Escritorio/Experimentation DATA 2025/Datasets/Pequeños/cancer.csv")

# Seleccionar las columnas numéricas para el análisis
X <- dataset[, 3:32]
y <- dataset$diagnosis

# Definir el número de elementos en cada clúster
n2 <-c(356, 212)
k = length(n2)

# Calcular la matriz de distancias
if (!require("proxy")) install.packages("proxy")
library(proxy)
D <- proxy::dist(as.matrix(X), method = "cosine")
D = as.matrix(D)
distancia = D

#===========================================================================================================
#PRCESO ALGORITMO PLCSC

r <- nrow(D) # Número total de documentos

cl = kmeans(X, k)
c = cl$size

n2 <- matrix(n2 - 1, ncol = 1) #(sin centroides)
v <- 1:r #vector del número total de documentos
v <- v[!v %in% c] #Elimino los centroides del dataset
D <- D[-v, ] #Elimina todas las filas menos la de los centroides
f1 <- as.vector(t(D)) # convierto las filas en un vector
D <- D[, -c] #Elimino las columnas de los centroides

f <- as.vector(t(D))# convierto las filas en un vector funcion depurada
DDD = as.data.frame(f)
# Construcción de matrices para restricciones
Aeq1 <- matrix(0, nrow = k, ncol = length(f))
for (i in 1:ncol(D)) {
  for (j in 1:k) {
    Aeq1[j, i + (j - 1) * ncol(D)] <- 1
  }
}

Aeq2 <- diag(ncol(D))
for (i in 1:(k - 1)) {
  Aeq2 <- cbind(Aeq2, diag(ncol(D)))
}

Aeq <- rbind(Aeq1, Aeq2)
beq <- c(n2, rep(1, ncol(D))) # n1 = tamaño igual, n2 = tamaño diferente

# Modelo de Programación Lineal Binaria
if (!require("lpSolve")) install.packages("lpSolve")
library(lpSolve)

intcon <- seq_along(f)
lb <- rep(0, length(f))
ub <- rep(1, length(f))

# Suponiendo que no hay restricciones de desigualdad en este caso
A <- Aeq
dir <- c(rep("==", nrow(Aeq)))  # Cambiar según las restricciones de desigualdad
rhs <- beq

# Llamada a la función lp()
result <- lp(direction = "min", objective.in = f, const.mat = A, const.dir = dir, const.rhs = rhs, all.bin = TRUE)

x <- result$solution
fval <- result$objval

s1 <- matrix(x, nrow = k, byrow = TRUE)
s2 <- diag(k)
st <- matrix(0, nrow = k, ncol = r)
st[, c] <- s2
st[, v] <- s1

label_pred <- numeric(r)
for (i in 1:k) {
  ind <- which(st[i, ] == 1)
  label_pred[ind] <- i
}

# vector de indices de los documentos ordenados (solución PLCSC)
index <- unlist(lapply(1:k, function(i) which(label_pred == i)))
#================================================================================================================
# distancia <- mat_dist_cos(X)
silhouette_values <- silhouette(x = label_pred, dist = as.dist(distancia))
mean_silhouette <- mean(silhouette_values[, "sil_width"])

# Mostrar el promedio del coeficiente de silueta
print(paste("Promedio del coeficiente de silueta:", mean_silhouette))

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

