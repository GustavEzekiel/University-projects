# TP11 modelos jerarquicos.
# Ezequiel Perez.
# Modelos 2021


#################################### libraries and wd ----

setwd("D:/Documents/Uncoma/Math & stadistics/Modelos en ecología/Tps/tp11-Modelos jerárquicos")
library("jagsUI")



#################################### Codigo del tp ----

## datos simulados

set.seed(1234)
n <- 10 # sub-poblaciones
m <- c(30, 28, 20, 30, 30, 26, 29, 5, 3, 27) # número de individuos muestraeados por sub-poblacion
a_s <- 2
b_s <- 10
theta <- rbeta(n, a_s, b_s) # generamos n tasas de mortalidad, una por cada sub-poblacion
y <- rbinom(n, size = m, prob = theta) # simulamos número de muertes por grupo
op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1,
          bty = "n")
plot(table(y), xlab = "Muertes por sub-población", ylab = "Frecuencia")


## complete pooling con los datos

# jags file
cat(file = "completepooling.bug", "
  model{
  for( i in 1 : n ) {
  y[i] ~ dbin(theta, m[i]) #Likelihood\t
  }
  pred <- y[n]
  \t theta ~ dbeta(1, 1) # previa no-informativa para la tasa de mortalidad
  }"
)

# data
data <- list("y", "m", "n")
inits <- function() list(theta = runif(1, 0, 1))
params <- c("theta", "pred")
ni <- 1000
nc <- 3
nt <- 1
nb <- 500

cp.sim <- jags(data, inits, params, model.file = "completepooling.bug",
               n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt)
print(cp.sim)


#################################### Complete pooling Ejercicios 1 y 2 ----
# Ejercicios:



# 1- ¿Cuál es el valor esperado de la posterior de la tasa de mortalidad bajo el supuesto de "complete pooling"?

# Es el valor medio arrojado por el analisis bayesiano:
mean(cp.sim$sims.list$theta)




# 2- Comparar el valor esperado de la tasa de mortalidad con el valor promedio de las tasas de mortalidad
# usadas para simular los datos.

# El valor medio de las tasas de mortalidad usadas para simular los datos es:
mean(theta)

# El valor verdadero de theta que sale de la distribucion beta:
a_s/(a_s + b_s)

# El parametro estimado por complete pooling resulta no estar tan alejado de la media de los valores usados para
# simular los datos. Sin embargo está alejado del valor verdadero que sale de la distribución beta usada
# para generar la muestra de thetas.


#################################### codigo del tp ----

cat(file = "nopooling.bug", "
model {
for( i in 1 : n ) {
y[i] ~ dbin(theta[i], m[i]) # Likelihood
theta[i] ~ dbeta(1, 1) # previas no-informativas para la tasa de mortalidad
\t\t}
}"
)
library(jagsUI)
data <- list("y", "m", "n")
inits <- function() list(theta = runif(n, 0, 1))
params <- c("theta")
ni <- 1000
nc <- 3
nt <- 1
nb <- 500
np.sim <- jags(data, inits, params, model.file = "nopooling.bug",
               n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt)

print(np.sim)

op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1,
          bty = "n")
plot(density(cp.sim$sims.list$theta), type = "l", lwd = 3, ylab = "Density",
     xlab = "Tasa de mortalidad", xlim = c(0, 0.5), ylim = c(0,
                                                             30), main = "") #Posterior de theta con complete pooling
for (i in 1:10) {
  lines(density(np.sim$sims.list$theta[, i]), type = "l", col = "gray",
        lwd = 3) #Posteriores de theta con no pooling
}

par(op)



#################################### No pooling ejercicios 1 y 2 ----

# Ejercicios

# 1- ¿Qué se puede decir de las distintas posteriores?

# Los Theta para casa subpoblacion son muy heterogeneos (hay mucha dispersion) al hacer no-pooling. Al
# hacer complete pooling se obtiene una distribución posterior intermedia
# entre todas las que salen al hacer no-pooling.

# 2- ¿Alguno de estos dos enfoques les parece más adecuado? ¿Cuáles serían los pro y contras de cada uno?

# Hacer no pooling puede ser útil si hay mucha varianza en los datos que pueda ser
# explicada por los agrupamientos en subpoblaciones, ya que permite tener en cuenta ese factor y aumentar 
# la potencia de las estimaciones. Como desventaja ocurre que si el número de sub-poblaciones es muy grande, se corre 
# el riesgo de perder muchos grados de libertad para el ajuste dada la alta cantidad de parámetros estimados.
# Otra desventaja podria ser el sobre-ajuste o "over fitting" especialmente si el N es chico. 
# Si los datos no estan muy estructurados, hacer complete pooling evitaría las desventajas mencionadas, pero 
# a costa de perder detalle y potencia en las estimaciones.



################################### codigo del tp ----

cat(file = "hier.bug", "
  model{
  #Likelihood
  for(i in 1: n ) {
  \t\t y[i] ~ dbin(theta[i], m[i])\t
  \t\t theta[i] ~ dbeta(a, b)
  }
  pred <- y[n]
  #Previas
  \t\ta ~ dnorm(0,0.01)T(0,)
  \t\tb ~ dnorm(0,0.01)T(0,)
  mean_pobl <- a/(a + b)
  }"
)

library(jagsUI)
data <- list("y", "m", "n")
inits <- function() list(a = runif(1, 1, 5), b = runif(1, 5,
                                                       20))
params <- c("a", "b", "theta", "mean_pobl")
ni <- 5000
nc <- 3
nt <- 4
nb <- 2500

hier.sim <- jags(data, inits, params, model.file = "hier.bug",
                 n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt)
print(hier.sim) 

op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1,
          bty = "n")
plot(density(hier.sim$sims.list$mean_pobl), main = "", xlab = "Tasa de mortalidad",
     lwd = 3)
abline(v = a_s/(a_s + b_s), col = 2, lwd = 3, lty = 2)



# comparación entre los enfoques
op <- par(mfrow = c(1, 2), cex.lab = 1.5, font.lab = 2, cex.axis = 1.3,
          las = 1, bty = "n")
plot(density(cp.sim$sims.list$theta), type = "l", lwd = 3, ylab = "Density",
     xlab = "Tasa de mortalidad", xlim = c(0, 1), ylim = c(0,
                                                           25), main = "") #Complete pooling
for (i in 1:10) {
  lines(density(np.sim$sims.list$theta[, i]), type = "l", col = "gray",
        lwd = 2)
} 
#No pooling
curve(dbeta(x, hier.sim$mean$a, hier.sim$mean$b), lwd = 3, col = 2,
      xlab = "Tasa de mortalidad", ylab = "", ylim = c(0, 25))

for (i in 1:10) {
  lines(density(hier.sim$sims.list$theta[, i]), col = "blue",
        lwd = 2, main = "") #Tasa de mortalidad de cada subpoblación estimadas con partial pooling
}

par(op)


################################### ultimo ejercicio ----


### ¿Cómo comparan las posteriores para las distintas sub-poblaciones con no-pooling y partial pooling?


# Comparación.
par(mfrow=c(2,3))

for (i in 1:10) {
  plot(density(np.sim$sims.list$theta[, i]), type = "l", col = "gray",
        lwd = 2, main = "", ylim = c(0,15), xlim = c(0, 0.8))
  lines(density(hier.sim$sims.list$theta[, i]), col = "blue",
        lwd = 2, main = "") #Tasa de mortalidad de cada subpoblación estimadas con partial pooling
  abline(v = theta[i], col = "black", lty=5)
  legend(0.3, 13, legend = c("no pooling", "parcial pooling"), fill = c("gray", "blue"), cex = 0.9)
} 

# No se observan muchas diferencias entre el parcial y no-pooling en la estimacion de los theta usados para
# simular cada subpoblación. Hay casos donde la estimacion por no pooling es mejor y viceversa. Sin embargo,
# a grandes rasgos la diferencia no es significativa.

