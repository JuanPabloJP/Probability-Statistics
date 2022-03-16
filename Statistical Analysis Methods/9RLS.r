library(alr4)
# UN11
head(UN11)

# y = esperanza de vida de las mujeres

# Creo una base de datos solo para tener todo en español
UN <- data.frame(Esp_Vida = UN11$lifeExpF, PIB_PER_CAP = UN11$ppgdp,
                 Porc_Urban = UN11$pctUrban, Fertilidad = UN11$fertility, 
                 grupo = UN11$group)
head(UN)

UN$grupo <- factor(UN$grupo, levels = c("oecd", "other", "africa"))
table(UN$grupo)



# MATRIZ DE DIAGRAMAS DE DISPERSION
my_col <- c("red", "black", "blue")[unclass(UN$grupo)]
pairs(~ Esp_Vida + PIB_PER_CAP + Porc_Urban + Fertilidad, 
      pch = 21, bg = my_col, col = my_col, data=UN)

# MATRIZ DE CORRELACIÓN DE PEARSON
round(cor(UN[,-5]), 2)

# Esperanza de vida = b0 + b1 Fertilidad
plot(UN$Esp_Vida ~ UN$Fertilidad, 
     pch = 21, bg = my_col, col = my_col)

#
summary(UN)

UN$LOG_PIB_PER_CAP <- log(UN$PIB_PER_CAP)


my_col <- c("red", "black", "blue")[unclass(UN$grupo)]
pairs(~ Esp_Vida + LOG_PIB_PER_CAP + Porc_Urban + Fertilidad, 
      pch = 21, bg = my_col, col = my_col, data=UN)

round(cor(UN[,-5]), 2)

# Esperanza de vida = b0 + b1 Fertilidad
plot(UN$Esp_Vida ~ UN$LOG_PIB_PER_CAP, 
     pch = 21, bg = my_col, col = my_col)


n <- nrow(UN)
set.seed(22)
m0 <- 20
id0 <- sample.int(n, m0)
id <- rep(FALSE, n)
id[id0] <- TRUE
TRAIN <- UN[!id,]
PRED <- UN[id,]

my_col <- rep("black", n)
my_col[id] <- "red" 
plot(UN$Esp_Vida ~ UN$Fertilidad, 
     pch = 21, bg = my_col, col = my_col)


m1 <- lm(Esp_Vida ~ Fertilidad, data = TRAIN)
summary(m1)

# Extrayendo información de la función lm 
y_hat <- m1$fitted.values # yhat_i = b0_hat + b1_hat x_i
e_hat <- m1$residuals     # e_hat_i <- y_i - yhat_i


my_col <- rep("black", n)
my_col[id] <- "red" 
plot(UN$Esp_Vida ~ UN$Fertilidad, 
     pch = 21, bg = my_col, col = my_col)
abline(m1, col = "blue", lwd = 2)



# PREDICCIÓN
my_pred <-  predict.lm(m1, newdata = PRED , 
                       interval = "prediction", level = 0.95)



plot(1:m0, my_pred[,1], type = "l", lwd = 2, ylim = c(45, 95)) 
lines(1:m0, my_pred[,2], lwd = 2, col = "blue")
lines(1:m0, my_pred[,3], lwd = 2, col = "blue")
lines(1:m0, PRED[,1], lwd = 2, col = "red") # real



# 
m2 <- lm(Esp_Vida ~ Fertilidad + LOG_PIB_PER_CAP, data = TRAIN)
summary(m2)

my_pred2 <-  predict.lm(m2, newdata = PRED , interval = "prediction", level = 0.95)


par(mfrow = c(1, 2))
plot(1:m0, my_pred2[,1], type = "l", lwd = 2, ylim = c(45, 95)) 
lines(1:m0, my_pred2[,2], lwd = 2, col = "blue")
lines(1:m0, my_pred2[,3], lwd = 2, col = "blue")
lines(1:m0, PRED[,1], lwd = 2, col = "red") # real
plot(1:m0, my_pred[,1], type = "l", lwd = 2, ylim = c(45, 95)) 
lines(1:m0, my_pred[,2], lwd = 2, col = "blue")
lines(1:m0, my_pred[,3], lwd = 2, col = "blue")
lines(1:m0, PRED[,1], lwd = 2, col = "red") # real
