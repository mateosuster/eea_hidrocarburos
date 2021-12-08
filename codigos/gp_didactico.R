#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


## librerías
library(plgp)
library(mvtnorm)


##### pequeño ruido para evitar el mal condicionamiento de algunas matrices que deben ser invertidas
eps <- sqrt(.Machine$double.eps) 



####### Ejemplo sampleo GP prior
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# inputs x
n <- 200
X <- matrix(seq(0, 20, length=n), ncol=1)

v <- 1
l <- 1/sqrt(2)

D <- distance(X)
# matriz covarianzas con exponencial distancia euclídea
# el eps en la diag es para evitar malos condicionamientos
Sigma <- exp(-D/(2*l^2)) + diag(eps, n) 

# observar que la diag es todos 1's, porque es exp(0)
diag(Sigma)


# armamos la normal multivariada
Y <- rmvnorm(n=1, sigma=v^2*Sigma) #1 obs por cada x

# los Y son random de la MVN
# tiene sentido que en un 95% caigan a +/-2 de la media=0
plot(X, Y, type="p")#, ylim=c(-3,3))


# correlaciones a distancia 1 y 4
c(exp(-1^2), exp(-4^2))




#ahora 3 obs por cada x
Y <- rmvnorm(3, sigma=Sigma)
matplot(X, t(Y), type="p", ylab="Y", ylim=c(-3,3))






######## Posterior predicción sin ruido
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# toy example 1D
# prior con mu=0
n <- 8
X <- matrix(seq(0, 2*pi, length=n), ncol=1)
y <- sin(X)
D <- distance(X) 
Sigma <- exp(-D) + diag(eps, ncol(D))


# predict
XX <- matrix(seq(-0.5, 2*pi + 0.5, length=100), ncol=1)
# XX <- matrix(seq(0, 2*pi, length=n*2), ncol=1)
DXX <- distance(XX)
SXX <- exp(-DXX) + diag(eps, ncol(DXX))

DX <- distance(XX, X)
SX <- exp(-DX) 

# nuevos mu y sigma
Si <- solve(Sigma)
mup <- SX %*% Si %*% y
Sigmap <- SXX - SX %*% Si %*% t(SX)

# sample from posterior
YY <- rmvnorm(100, mup, Sigmap)

matplot(XX, t(YY), type="l", col="gray", lty=1, xlab="x", ylab="y")
lines(XX, mup, lwd=2)
# lines(XX, sin(XX), col="blue")
# lines(XX, YY, col="blue", type = "p")
points(X, y, pch=20, cex=2)

q1 <- mup + qnorm(0.05, 0, sqrt(diag(Sigmap)))
q2 <- mup + qnorm(0.95, 0, sqrt(diag(Sigmap)))
lines(XX, q1, lwd=2, lty=2, col=2)
lines(XX, q2, lwd=2, lty=2, col=2)






###### Higher dimension
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

nx <- 20
x <- seq(0, 2, length=nx)
X <- expand.grid(x, x)

D <- distance(X)
Sigma <- exp(-D) + diag(eps, nrow(X))

Y <- rmvnorm(2, sigma=Sigma)

par(mfrow=c(1,2)) 
persp(x, x, matrix(Y[1,], ncol=nx), theta=-30, phi=30, xlab="x1", 
      ylab="x2", zlab="y")
persp(x, x, matrix(Y[2,], ncol=nx), theta=-30, phi=30, xlab="x1", 
      ylab="x2", zlab="y")





library(lhs) 
X <- randomLHS(40, 2)
X[,1] <- (X[,1] - 0.5)*6 + 1
X[,2] <- (X[,2] - 0.5)*6 + 1
y <- X[,1]*exp(-X[,1]^2 - X[,2]^2)

xx <- seq(-2, 4, length=40)
XX <- expand.grid(xx, xx)

D <- distance(X)
Sigma <- exp(-D)

DXX <- distance(XX)
SXX <- exp(-DXX) + diag(eps, ncol(DXX))
DX <- distance(XX, X)
SX <- exp(-DX)

Si <- solve(Sigma)
mup <- SX %*% Si %*% y
Sigmap <- SXX - SX %*% Si %*% t(SX)

sdp <- sqrt(diag(Sigmap))

par(mfrow=c(1,2))
cols <- heat.colors(128)
image(xx, xx, matrix(mup, ncol=length(xx)), xlab="x1", ylab="x2", col=cols)
points(X[,1], X[,2])
image(xx, xx, matrix(sdp, ncol=length(xx)), xlab="x1", ylab="x2", col=cols)
points(X[,1], X[,2])

persp(xx, xx, matrix(mup, ncol=40), theta=-30, phi=30, xlab="x1", 
      ylab="x2", zlab="y")





#### Scale
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

n <- 100
X <- matrix(seq(0, 50, length=n), ncol=1)
D <- distance(X)

l <- 10
C <- exp(-D/(2*l^2)) + diag(eps, n) 
tau <- 5
Y <- rmvnorm(3, sigma=tau^2*C)

matplot(X, t(Y), type="b")






## wrong scale prior
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

n <- 8
X <- matrix(seq(0, 2*pi, length=n), ncol=1)
y <- 5*sin(X)

D <- distance(X)
Sigma <- exp(-D)
XX <- matrix(seq(-0.5, 2*pi + 0.5, length=100), ncol=1)
DXX <- distance(XX)
SXX <- exp(-DXX) + diag(eps, ncol(DXX))
DX <- distance(XX, X)
SX <- exp(-DX)
Si <- solve(Sigma); 
mup <- SX %*% Si %*% y
Sigmap <- SXX - SX %*% Si %*% t(SX)

YY <- rmvnorm(100, mup, Sigmap)
q1 <- mup + qnorm(0.05, 0, sqrt(diag(Sigmap)))
q2 <- mup + qnorm(0.95, 0, sqrt(diag(Sigmap)))
matplot(XX, t(YY), type="l", col="gray", lty=1, xlab="x", ylab="y")
points(X, y, pch=20, cex=2)
lines(XX, mup, lwd=2)
lines(XX, 5*sin(XX), col="blue")
lines(XX, q1, lwd=2, lty=2, col=2)
lines(XX, q2, lwd=2, lty=2, col=2)



##### Parameters por MLE u otros métodos numéricos.





#### Posterior predicción con Ruido
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

nlg <- function(g, D, Y) 
{
  n <- length(Y)
  K <- exp(-D) + diag(g, n)
  Ki <- solve(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  ll <- - (n/2)*log(t(Y) %*% Ki %*% Y) - (1/2)*ldetK
  counter <<- counter + 1
  return(-ll)
}

n <- 8
X <- matrix(seq(0, 2*pi, length=n), ncol=1)
# X <- rbind(X, X)
n <- nrow(X)
y <- 5*sin(X) + rnorm(n, sd=1)
D <- distance(X)

counter <- 0
g <- optimize(nlg, interval=c(eps, var(y)), D=D, Y=y)$minimum
g

K <- exp(-D) + diag(g, n)
Ki <- solve(K)
tau2hat <- drop(t(y) %*% Ki %*% y / n)
c(tau=sqrt(tau2hat), sigma=sqrt(tau2hat*g))

XX <- matrix(seq(-0.5, 2*pi + 0.5, length=100), ncol=1)
DXX <- distance(XX)
DX <- distance(XX, X)
KX <- exp(-DX)
KXX <- exp(-DXX) + diag(g, nrow(DXX))

mup <- KX %*% Ki %*% y
Sigmap <- tau2hat*(KXX - KX %*% Ki %*% t(KX))
q1 <- mup + qnorm(0.05, 0, sqrt(diag(Sigmap)))
q2 <- mup + qnorm(0.95, 0, sqrt(diag(Sigmap)))

Sigma.int <- tau2hat*(exp(-DXX) + diag(eps, nrow(DXX)) 
                      - KX %*% Ki %*% t(KX))
YY <- rmvnorm(100, mup, Sigma.int)

matplot(XX, t(YY), type="l", lty=1, col="gray", xlab="x", ylab="y")
points(X, y, pch=20, cex=2)
lines(XX, mup, lwd=2)
# lines(XX, 5*sin(XX), col="blue")
lines(XX, q1, lwd=2, lty=2, col=2)
lines(XX, q2, lwd=2, lty=2, col=2)
