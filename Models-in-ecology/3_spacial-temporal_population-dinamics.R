# Tp6: Modelos con dinamica espacial explicita.
# Modelos en ecologia 2020
# Ezequiel Perez


##############################################################################--


### Modelo espacialmente explicito.

library(MASS)
lado <- 100
n0 <- 10
lambda <- 20
sd_disp <- 10
sd_comp <- 1
S <- matrix(c(sd_disp, 0, 0, sd_disp), 2, 2) # varianza covarianza para dispersión
niter <- 30
n <- numeric(niter)
n[1] <- n0
X <- list()
X[[1]] <- matrix(runif(2 * n0, 0, lado), n0, 2)
for(i in 2:niter){
  xm <- X[[i-1]] # coordenadas de madres
  semillas <- rpois(n[i-1], lambda)
  xs <- NULL
  for(j in 1: length(semillas)){
    xs <- rbind(xs, mvrnorm(semillas[j], xm[j,], S))
    xs <- xs%%lado # wrap around...
  }
  cvec <- numeric(sum(semillas))
  for(h in 1:sum(semillas)){
    ds <- sqrt( (xs[h,1] - xs[,1])^2 + (xs[h,2] - xs[,2])^2)
    cvec[h] <- sum(exp(-ds^2/(2 * sd_comp^2))) - 1 # entiendo que la sumatoria te puede dar mas de uno, pero aca te 
  }                                                # siempre mayor a uno, sino sum(prob) - 1 = num negativo y abajo te queda prob>1
  surv <- rbinom(sum(semillas), size = 1, prob = 1 - plogis(cvec)) # estudiar mejor que pasa aca.
  X[[i]] <- xs[surv==1, ]
  n[i] <- sum(surv)
}
op <- par(mfrow=c(1,2)) #, mar=c(3,2,2,1)+0.1)
plot(X[[niter]], asp = 1, xlab = "x", ylab = "y")
plot(n, type = "l", xlab = "tiempo")



### Probando diferentes combinaciones de parametros


param = matrix(c(rep(c(.5, 1, 1.5, 2), each = 4), rep(c(10,15,20, 25), 4)), ncol = 2, byrow = F)
colnames(param) = c('sd_comp', 'sd_disp')

op <- par(mfrow=c(2,2), mai = c(.35, .35, .35, .35), oma = c(.25, .1, .1, .1), cex.lab = 0.65, mgp = c(1.3, .5, 0), cex.axis = .7, 
          cex.main = 0.8)

# parametros fijos
lado <- 100
n0 <- 10
lambda <- 20
niter <- 30

# modelos
for (k in 1:length(param[,1])){
  
  sd_comp = param[k, 1]
  sd_disp = param[k, 2]
  n <- numeric(niter)
  n[1] <- n0
  X <- list()
  X[[1]] <- matrix(runif(2 * n0, 0, lado), n0, 2)
  S <- matrix(c(sd_disp, 0, 0, sd_disp), 2, 2) # varianza covarianza para dispersión
  
  for(i in 2:niter){
    xm <- X[[i-1]] # coordenadas de madres
    semillas <- rpois(n[i-1], lambda)
    xs <- NULL
    for(j in 1: length(semillas)){
      xs <- rbind(xs, mvrnorm(semillas[j], xm[j,], S))
      xs <- xs%%lado # wrap around...
    }
    cvec <- numeric(sum(semillas))
    for(h in 1:sum(semillas)){
      ds <- sqrt( (xs[h,1] - xs[,1])^2 + (xs[h,2] - xs[,2])^2)
      cvec[h] <- sum(exp(-ds^2/(2 * sd_comp^2))) - 1
    }
    surv <- rbinom(sum(semillas), size = 1, prob = 1 - plogis(cvec))
    X[[i]] <- xs[surv==1, ]
    n[i] <- sum(surv)
  }
  
  plot(X[[niter]], asp = 1, xlab = "x", ylab = "y", main = paste('sd_disp =', sd_disp, sep = ' '))
  plot(n, type = "l", xlab = "tiempo", main = paste('sd_comp =', sd_comp, sep = ' '))
}

par(op)




### Probando con una distribucion log-normal como kernel de competencia

lado <- 100
n0 <- 10
lambda <- 20
sd_disp <- 10
sd_comp <- 0.4
S <- matrix(c(sd_disp, 0, 0, sd_disp), 2, 2) # varianza covarianza para dispersión
niter <- 30
n <- numeric(niter)
n[1] <- n0
X <- list()
X[[1]] <- matrix(runif(2 * n0, 0, lado), n0, 2)
for(i in 2:niter){
  xm <- X[[i-1]] # coordenadas de madres
  semillas <- rpois(n[i-1], lambda)
  xs <- NULL
  for(j in 1: length(semillas)){
    xs <- rbind(xs, mvrnorm(semillas[j], xm[j,], S))
    xs <- xs%%lado # wrap around...
  }
  cvec <- numeric(sum(semillas))
  for(h in 1:sum(semillas)){
    ds <- abs(sqrt((xs[h,1] - xs[,1])^2 + (xs[h,2] - xs[,2])^2)) # saco el valor absoluto por las dudas.
    cvec[h] <- sum(exp((-log(ds)^2)/(2 * sd_comp^2))) - 1 # kernel de competencia log-normal.
  }
  surv <- rbinom(sum(semillas), size = 1, prob = 1 - plogis(cvec)) 
  X[[i]] <- xs[surv==1, ]
  n[i] <- sum(surv)
}
op <- par(mfrow=c(1,2)) #, mar=c(3,2,2,1)+0.1)
plot(X[[niter]], asp = 1, xlab = "x", ylab = "y")
plot(n, type = "l", xlab = "tiempo")




### Punto desafio: Probando con heterogeneidad en el ambiente.

library("bivariate")

# simulando el ambiente heterogeneo.
f = bmbvpdf (
  mean.X1=20, mean.X2=70,
  mean.Y1=20, mean.Y2=55,
  sd.X1=15,   sd.X2=15,
  sd.Y1=10,   sd.Y2=30)


# funcion de simulacion de dinamica poblacional en el espacio heterogeneo
simuh = function(sdd, sdc, ba){
  sd_disp <- sdd
  sd_comp <- sdc
  bondad_amb = ba
  lado <- 100
  n0 <- 10
  lambda <- 20
  S <- matrix(c(sd_disp, 0, 0, sd_disp), 2, 2) # varianza covarianza para dispersión
  niter <- 30
  n <- numeric(niter)
  n[1] <- n0
  X <- list()
  X[[1]] <- matrix(runif(2 * n0, 0, lado), n0, 2)
  for(i in 2:niter){
    xm <- X[[i-1]] # coordenadas de madres
    semillas <- rpois(n[i-1], lambda)
    xs <- NULL
    for(j in 1: length(semillas)){
      xs <- rbind(xs, mvrnorm(semillas[j], xm[j,], S))
      xs <- xs%%lado # wrap around...
    }
    likely = f(xs[ , 1], xs[ , 2]) * bondad_amb # evaluando la funcion bimodal bivariada para las coordenadas de la semillas
    # puede ser que las diferencias no sean muchas entre el peor y mejor pto.
    sobr <- rbinom(sum(semillas), size = 1, prob = likely) # deteminando quienes sobreviven.
    sobrev = xs[sobr==1, ] # filtrado de las sobrevivientes
    cvec <- numeric(length(sobrev))
    for(h in 1:length(sobrev)){
      ds <- sqrt( (xs[h,1] - xs[,1])^2 + (xs[h,2] - xs[,2])^2)
      cvec[h] <- sum(exp(-ds^2/(2 * sd_comp^2))) - 1 
    }                                                
    surv <- rbinom(length(sobrev), size = 1, prob = 1 - plogis(cvec)) 
    X[[i]] <- xs[surv==1, ]
    n[i] <- sum(surv)
  }
  op <- par(mfrow=c(1,2)) #, mar=c(3,2,2,1)+0.1)
  plot (f, FALSE, theme="blue" , xlab = "x", ylab = "y", ylim = c(-5,105), xlim = c(-5, 105))
  points(X[[niter]], asp = 1)
  plot(n, type = "l", xlab = "tiempo")
}

# algunas simulaciones
simuh(15, 0.35, 1000)
simuh(10, 0.4, 2000)
simuh(25, 0.3, 2000)
simuh(50, 0.2, 500)
simuh(10, 0.35, 1500)




