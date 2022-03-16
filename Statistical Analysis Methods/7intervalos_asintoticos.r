###################
# N(mu, sigma^2)  #
###################
mu <- 10
sigma <- 5
n <- 120
x <- rnorm(n, mu, sigma)

# Intervalo del 95%
alfa <- 0.05
1- alfa
zq <- qnorm(1 - alfa/2)
tq <- qt(1 - alfa/2, df = n-1)
x_bar <- mean(x)


cte <- sd(x)/sqrt(n)

mu 
x_bar + c(-1, 1)*cte*zq
x_bar + c(-1, 1)*cte*tq


nn <- 2000
qnorm(1 - alfa/2)
qt(1 - alfa/2, df = nn-1)


###################
# Gamma(a, b)  #
###################
a <- 3
b <- 1
mu <- a/b

# Intervalo del 95%
alfa <- 0.05
1- alfa
n <- 400
zq <- qnorm(1 - alfa/2)
tq <- qt(1 - alfa/2, df = n-1)
m <- 100000
cont_as <- 0
cont_cons <- 0 
for(t in 1:m){
  x <- rgamma(n, a, b)
  x_bar <- mean(x)
  cte <- sd(x)/sqrt(n)
  Ias <- x_bar + c(-1, 1)*cte*zq
  Icons <- x_bar + c(-1, 1)*cte*tq
  if(mu >= Ias[1]  & mu <= Ias[2]){
    cont_as <- cont_as + 1 
  }
  if(mu >= Icons[1]  & mu <= Icons[2]){
    cont_cons <- cont_cons + 1 
  }
}
100*cont_as/m
100*cont_cons/m






