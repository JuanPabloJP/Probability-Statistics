# Functions and Datasets for Books by Julian Faraway
library(faraway) 


#############
# Ejemplo 1 #
#############
# ===> Nativas americanas Pima
# ¿El índice de masa corporal (IMC) de diabéticas 
# es mayor que el IMC de las no diabéticas?
head(pima)

# Estadística básica
table(pima$test)

tapply(pima$bmi, pima$test, mean)

x <- pima$bmi[pima$test == 0] # no tienen diabetes
y <- pima$bmi[pima$test == 1] # tienen diabetes

par(mfrow = c(1, 2))
hist(x, main = "", border = "white", xlab = "Índice de masa corporal (negativo diabetes)")
box(lwd = 2)
hist(y, main = "", border = "white", xlab = "Índice de masa corporal (positivo diabetes)")
box(lwd = 2)


# No es suficiente para romar una decisión 
# pues estamos usando una muestra, se tiene que
# considerar esa incertidumbre

# ¿Cómo podemos afirmar que existe alguna diferencia?
#     Queremos determinar si
#
#       mu_diabetes > mu_no_diabetes
#
# Hay tres casos
#
# 1) mu_diabetes > mu_no_diabetes ==>  mu_diabetes - mu_no_diabetes > 0     
# 2) mu_diabetes = mu_no_diabetes ==>  mu_diabetes - mu_no_diabetes = 0
# 3) mu_diabetes < mu_no_diabetes ==>  mu_diabetes - mu_no_diabetes < 0
#
# Vamos a obtener el intervalo de confianza para: mu_diabetes - mu_no_diabetes

n <- length(x)
m <- length(y)
x_bar <- mean(x)
y_bar <- mean(y)
s2_x <- var(x)
s2_y <- var(y)
s_xy <- sqrt(s2_x/n + s2_y/m)

aalfa <- 0.05 # intervalo del (1-aalfa)x100%
z <- qnorm(1-aalfa/2)
t <- qt(1 - aalfa/2, n + m - 2)
y_bar - x_bar + z*c(-1, 1)*s_xy
y_bar - x_bar + t*c(-1, 1)*s_xy

#
#
pima$test2 <- 1*pima$test == 0
ri <- t.test(bmi ~ test2, data = pima, conf.level=.95)
ri$conf.int


# mu_diabetes > mu_no_diabetes
t.test(y, x, data = pima, conf.level=.95)

# ¿Esto qué quiere decir?
#      <=  mu_diab - mu_no_dian <= 
# con un 95% de confianza


#############
# Ejemplo 2 #
#############
# https://archive.ics.uci.edu/ml/datasets/Covertype
library(data.table)
cvr <- data.frame(fread("/Users/carloserwin/Downloads/covtype.data")) 

head(cvr)

summary(cvr)

table(cvr[,55])


tapply(cvr[,8], cvr[, 55], mean)
tapply(cvr[,8], cvr[, 55], var)
tapply(cvr[,8], cvr[, 55], length)

id1 <- cvr[, 55] == 1
id2 <- cvr[, 55] == 2
x <- cvr[id1, 8]
y <- cvr[id2, 8]

# mu_y - mu_x
ri <- t.test(y, x, conf.level=.95)
ri$conf.int
