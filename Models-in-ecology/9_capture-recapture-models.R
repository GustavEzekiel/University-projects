# Tp 13 modelos de captura y recaptura
# Ezequiel Perez
# Modelos en ecología 2021










################### Enviroment setting ----

setwd("D:/Documents/Uncoma/Math & stadistics/Modelos en ecología/Tps/tp13-Modelos de captura-recaptura")
library(jagsUI)

################### Codigo del tp ----

## estimación del tamaño poblacional.

# datos
trap_xy <- read.table("https://sites.google.com/site/modelosydatos/trampas.txt",
                      header = TRUE)
trap_env <- read.table("https://sites.google.com/site/modelosydatos/microambientetrampa.txt",
                       header = TRUE)
cap_h <- read.table("https://sites.google.com/site/modelosydatos/cap.txt",
                    header = TRUE)
cap_trap <- read.table("https://sites.google.com/site/modelosydatos/capturas_spat.txt",
                       header = TRUE)


# modelo

cat(file = "M0_DAUG.bug",
"
  model{
  for(i in 1: (n + nz)){
  z[i] ~ dbin(psi, 1) # Variable oculta
  mu[i] <- z[i] * p
  y[i] ~ dbin(mu[i], J) # capturas por individuo
  }
  psi ~ dunif(0, 1) # probabilidad de ser parte de la población.
  p ~ dunif(0, 1) # probabilidad de captura.
  N <- sum(z[1: (n + nz)]) # Tamaño poblacional estimado.
}"
)

n <- dim(cap_h)[1] # individuos capturados
J <- dim(cap_h)[2] # total de intentos de captura ("occasions")
nz <- 10 # Número de ceros (individuos no capturados nunca)
# matriz de historia de capturas
Y <- rbind(as.matrix(cap_h), matrix(0, nrow = nz, ncol = J))
y <- rowSums(Y) # total de capturas por individuo

m.data <- list("J","n","nz", "y")
inits <- function() list(psi = runif(1),
                         p = runif(1),
                         z = c(numeric(n) + 1, sample(0: 1, nz, replace = TRUE)))
params <- c("N", "psi", "p")
ni <- 5000
nt <- 1
nb <- 1000
nc <- 3

M0.sim <- jags(data = m.data,
               inits = inits,
               parameters.to.save = params,
               model.file = "M0_DAUG.bug",
               n.chains = nc,
               n.thin = nt,
               n.iter = ni,
               n.burnin=nb)
print(M0.sim)

op <- par(cex.lab = 1.5 , font.lab = 1, cex.axis = 1.3, las = 1, bty = "n")
hist(M0.sim$sims.list$N, 30, xlab = "N", main = "", freq = FALSE)
par(op)





################### Ejercicio ----

## Ejercicio

### . Ajustar el modelo usando un nz de 50 y uno de 200; graficar la posterior de N en cada caso y comparar.


sim <- function (nz) {
    
    nz=nz
    
    # matriz de historia de capturas
    Y <- rbind(as.matrix(cap_h), matrix(0, nrow = nz, ncol = J))
    y <- rowSums(Y) # total de capturas por individuo
    
    m.data <- list("J","n","nz", "y")
    inits <- function() list(psi = runif(1),
                             p = runif(1),
                             z = c(numeric(n) + 1, sample(0: 1, nz, replace = TRUE)))
    params <- c("N", "psi", "p")
    ni <- 5000
    nt <- 1
    nb <- 1000
    nc <- 3

    M0.simu=jags(data = m.data,
                inits = inits,
                parameters.to.save = params,
                model.file = "M0_DAUG.bug",
                n.chains = nc,
                n.thin = nt,
                n.iter = ni,
                n.burnin=nb)

    return(M0.simu)
  }

# graficado para comparación
res1 <- sim(50)
res2 <- sim(200)

# nz = 10
hist(M0.sim$sims.list$N, 30, xlab = "N", main = "", freq = FALSE)

op <- par(mfrow = c(2, 1))

# nz = 50

hist(res1$sims.list$N, 30, xlab = "N", main = "", freq = FALSE)

# nz = 200

hist(res2$sims.list$N, 30, xlab = "N", main = "", freq = FALSE)



par(op)


# La estimación de N se vuelve tiene a una distribución a medida que se aumenta la cantidad de ceros.





################### Codigo de tp ----

# Estimación de la densidad poblacional


cat(file = "Mspat.bug",
    "
model{
for (i in 1:M) { # total de bichos, incluyendo los ceros
for (j in 1:R) { # total de trampas
D2[i,j] <- pow(Sx[i]-x[j,1], 2) + pow(Sy[i]-x[j,2], 2)
# D2 es distancia entre el centro de actividad (Sx,Sy) y la trampa j
K[i,j] <- exp(-D2[i,j]/sigma2) # gaussian kernel
gamma[i,j] <- K[i,j]/E[i] # pr de captura en la trampa j dado que
# el individuo i es capturado
}
z[i] ~ dbern(psi)
E[i] <- sum(K[i, ]) # grado de exposición del individuo i
n[i] ~ dbin(Paug[i], J) # conexión con los datos
Paug[i] <- p[i]*z[i]
p[i] <- p0 * exp(-1/E[i]) # probabilidad de captura para el individuo i
Sx[i] ~ dunif(xlower, xupper)
Sy[i] ~ dunif(ylower, yupper)
}
for(h in 1:Ncap) { # total de individuos capturados
H[h,1] ~ dcat(gamma[H[h,2], ])
# H[h,1] indica la trampa donde fue capturado el individuo H[h,2]
}
psi~ dunif(0, 1)
p0 ~ dunif(0, 1)
sigma2 ~ dunif(0, 500)
N <- sum(z[1:M])
D <- N/( (xupper-xlower) * (yupper-ylower) )
}"
)

nz <- 250 # número de ceros (individuos no capturados nunca)
Y <- rbind(as.matrix(cap_h),matrix(0,nrow=nz,ncol=J))
n <- rowSums(Y) # capturas por individuo
nind <- dim(cap_trap)[1] # total de individuos capturados
J <- dim(cap_trap)[2] # total de días de campturas
R <- dim(trap_xy)[1] # total de trampas
M <- nz + nind
# capturas por trampa
cbyt <- numeric(R)
for(i in 1: R){
  cbyt[i] <- length(which(cap_trap == i))
}

H <- NULL #c(NA,NA)
for(i in 1: nind){
  tmp<-which(cap_trap[i, ] > 0)
  for(j in 1: length(tmp)){
    H <- rbind(H, c(cap_trap[i, tmp[j]], i))
  }
}

Ncap<-dim(H)[1] # total de capturas

xlower <- -1.5
xupper <- 1.5
ylower <- -1.5
yupper <- 1.5

x <- as.matrix(trap_xy/100)

datos <- list("xlower", "xupper", "ylower", "yupper", "x", "M", "R", "J", "Ncap" ,"n" ,"H")
inits <- function() list(sigma2 = runif(1, 0, 50),
                         psi = runif(1), p0 = runif(1),
                         z = c(numeric(nind) + 1, sample(0:1, nz, replace = TRUE)))
params <- c("sigma2", "N", "psi","p0","D")
Mspat.sim <- jags(data = datos,
                  inits,
                  parameters.to.save = params,
                  model.file = "Mspat.bug",
                  n.burnin = 1000,
                  n.chains = 3,
                  n.iter = 2000)

print(Mspat.sim)

op = par(mar=c(4,4,1,1)+0.1, las = 1, cex.lab = 1.5, cex.axis = 1.3)
hist(Mspat.sim$sims.list$D, freq = FALSE, xlab="Individuos por ha", main = "")
par(op)

r95 <- qweibull(0.95, shape = 2, scale = sqrt(Mspat.sim$mean$sigma2))
pi * r95^2 # home range in Ha

r95s <- qweibull(0.95,shape=2,scale=sqrt(Mspat.sim$sims.list$sigma2))
aaccs <- pi * r95s^2 # home range in Ha

op <- par(cex.lab = 1.5 , font.lab = 1, cex.axis = 1.3, las = 1, bty = "n")
plot(density(aaccs), main = "", lwd = 3, xlab = "Home range (ha)", ylab = "density")

par(op)


### Pregunta: - ¿Pusimos suficientes ceros extra?

# No se ve que haya una saturación del buffer de ceros por lo que se puede observar en los gráficos de
# densidad de probabilidad posterior de individuos y de área de acción.










x <- as.matrix(trap_xy/100)