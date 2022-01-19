# TP modelos jerárquicos.
# Establecimiento de alisos.


#############################  wd y paquetes ----

setwd("D:/Documents/Uncoma/Math & stadistics/Modelos en ecología/Tps/tp11-Modelos jerárquicos")

library(jagsUI)


############################# codigo del tp ----

# datos
datos <- read.table("https://sites.google.com/site/modelosydatos/alisos.txt",
                    header = TRUE)
yr <- datos$yr
ne <- datos$ne
lluvia <- datos$lluvia
cuenca <- datos$cuenca

ncuencas <- length(unique(cuenca))



# analisis jerarquico

# modelo en jags.
cat(file = "aliso.bug", "
  model{
  # modelo de datos
  for(i in 1:nobs){
  ne[i]~dpois(lambda[i]) # establecimientos por año
  lambda[i] <- exp( a[cuenca[i]] + b[cuenca[i]]*lluvia[i] )
  }
  # previas a nivel de cuencas
  for(j in 1:ncuencas){
  a[j]~dnorm(mua,taua)
  b[j]~dnorm(mub,taub)
  }
  # previas a nivel poblacional
  mua~dnorm(0,0.001) \t
  sa~dunif(0,100)\t\t
  taua<-1/(sa*sa) \t
  mub~dnorm(0,0.001)\t
  sb~dunif(0,100)
  taub<-1/(sb*sb)
  }
")


nobs = length(yr)
aliso.data <- list("ne", "lluvia", "cuenca", "nobs", "ncuencas")
inits <- function() {
  list(a = rnorm(ncuencas, 0, 0.5), b = rnorm(ncuencas, 0,
                                              0.5), mua = rnorm(1, 0, 0.5), mub = rnorm(1, 0, 0.5),
       sa = runif(1), sb = runif(1))
}
parameters <- c("a", "b", "mua", "mub", "sa", "sb")


aliso.sim <- jags(data = aliso.data, inits, parameters, model.file = "aliso.bug",
                  n.thin = 1, n.chains = 3, n.iter = 5000)



# Modelo con variable oculta
cat(file = "alisop.bug", "
model{
for(i in 1:(nobs)){
N[i] ~ dpois(lambda[i]) # variable oculta
lambda[i] <- exp( a[cuenca[i]] + b[cuenca[i]]*lluvia[i])
ne[i] ~ dbin(p[i],N[i]) # establecimientos observados
p[i] <- exp(-d * yr[i]) # probabilidad de observar un establecimiento en función del }
#Previas a nivel de cuencas\t
for(j in 1:ncuencas){
a[j]~dnorm(mua,taua)
b[j]~dnorm(mub,taub)
}
# previas para todos los parámetros\t
mua~dnorm(0,0.001)
sa~dunif(0,100)
taua<-1/(sa*sa)
mub~dnorm(0,0.001)
sb~dunif(0,100)
taub<-1/(sb*sb)
d~dunif(0,100)
}"
)

alisop.data <- list("yr", "ne", "lluvia", "cuenca", "nobs", "ncuencas")
initsp <- function() {
  list(a = rnorm(ncuencas, 6, 0.5), b = rnorm(ncuencas, 1,
                                              0.1), mua = rnorm(1, 0, 0.5), mub = rnorm(1, 0, 0.5),
       sa = runif(1), sb = runif(1), d = 1e-05)
}
parametersp <- c("a", "b", "mua", "mub", "sa", "sb", "d")
alisop.sim <- jags(data = alisop.data, initsp, parametersp, model.file = "alisop.bug",
                   n.thin = 5, n.chains = 3, n.iter = 5000)





###################################################### Ejercicio ----


# Modelo con variable oculta con saturación de establecimiento de 
cat(file = "alisop.bug", "
  model{
  for(i in 1:(nobs)){
  N[i] ~ dpois(lambda[i]) # variable oculta
  lambda[i] <- exp( a[cuenca[i]] + b[cuenca[i]]*lluvia[i]*lluvia[i])
  ne[i] ~ dbin(p[i],N[i]) # establecimientos observados
  p[i] <- exp(-d * yr[i]) # probabilidad de observar un establecimiento en función del }
  #Previas a nivel de cuencas\t
  for(j in 1:ncuencas){
  a[j]~dnorm(mua,taua)
  b[j]~dnorm(mub,taub)
  }
  # previas para todos los parámetros\t
  mua~dnorm(0,0.001)
  sa~dunif(0,100)
  taua<-1/(sa*sa)
  mub~dnorm(-4,0.001)   # previa informativa negativa para término cuadrático
  sb~dunif(0,100)
  taub<-1/(sb*sb)
  d~dunif(0,100)
  }"
)

alisop.data <- list("yr", "ne", "lluvia", "cuenca", "nobs", "ncuencas")
initsp <- function() {
  list(a = rnorm(ncuencas, 6, 0.5), b = rnorm(ncuencas, 1,
                                              0.1), mua = rnorm(1, 0, 0.5), mub = rnorm(1, 0, 0.5),
       sa = runif(1), sb = runif(1), d = 1e-05)
}
parametersp <- c("a", "b", "mua", "mub", "sa", "sb", "d")
alisop.sim <- jags(data = alisop.data, initsp, parametersp, model.file = "alisop.bug",
                   n.thin = 5, n.chains = 3, n.iter = 5000)
