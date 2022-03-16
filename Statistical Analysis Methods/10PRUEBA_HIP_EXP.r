# Sea X1,...Xn una m.a. de una exp(theta), 
# se quiere contrastar
#
#    H0: theta >= theta0 vs H1: theta < theta0
theta <- 4
n <- 100
x <- rexp(n, theta) # == rgamma(n, 1, theta)

# Supongamos que
theta0 <- 4.1
aalfa <- 0.05

# ESTADÍSICA DE PRUEBA
T0 <- sum(x)

# REGLA DE DECISIÓN VÍA ESTADÍSTICA DE PRUEBA
if(T0 > qgamma(1 - aalfa, n, theta0)){
  cat(paste0("EXISTE EVIDENCIA PARA RECHAZAR H0 AL NIVEL DE SIGNIFICANCIA alfa = ", aalfa))
}else{
  cat(paste0("NO EXISTE EVIDENCIA PARA RECHAZAR H0 AL NIVEL DE SIGNIFICANCIA alfa = ", aalfa))
}
  
# p-valor 
pvalor <- 1 - pgamma(T0, n, theta0) 

# REGLA DE DECISIÓN VÍA p-valor
if(pvalor < aalfa){
  cat(paste0("EXISTE EVIDENCIA PARA RECHAZAR H0 AL NIVEL DE SIGNIFICANCIA alfa = ", aalfa))
}else{
  cat(paste0("NO EXISTE EVIDENCIA PARA RECHAZAR H0 AL NIVEL DE SIGNIFICANCIA alfa = ", aalfa))
}