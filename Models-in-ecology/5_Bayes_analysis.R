# tp9: Bayes
# Modelos en ecología 2021.
# Ezequiel Perez.

# -----------------------------------------------------------------------------#

setwd("D:/Documents/Uncoma/Math & stadistics/Modelos en ecología/Tps/tp9-Estadistica bayesiana")






## Análisis bayesiano con datos inventados ----

set.seed(1234)
nobs <- 30  # número de observaciones (plantas)
frutos <- rep(20, nobs)  # frutos disponibles
p_rem <- 0.2  # probabilidad de remoción por fruto
removidos <- rbinom(nobs, size = frutos, prob = p_rem) # removidos: vector de 30 plantas con la cant. de removidos.

# previas
alpha <- 1
beta <- 1

# posterior
alpha_p <- alpha + sum(removidos)
beta_p <- beta + sum(frutos - removidos)

# Para obtener el valor esperado de una distribución Beta hacermos
alpha_p/(alpha_p + beta_p)

# Para tener una medida de incertidumbre alrededor de este valor, podemos ver los cuantiles de la posterior
qbeta(c(0.025, 0.975), alpha_p, beta_p)

# Graficando las previas y posteriores
op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
curve(dbeta(x, alpha + sum(removidos), beta + sum(frutos - removidos)), lwd = 2, 
      ylab = "Densidad de probabilidad", xlab = "Probabilidad de remoción")
curve(dbeta(x, 1, 1), lwd = 2, col = "gray", add = TRUE)
text(0.6, 2.5, "previa")
text(0.35, 12, "posterior")
par(op)

# Cómo sabemos si estas estimaciones tienen sentido para nuestros datos? 
# Una opción para contestar esa pregunta es hacer simulaciones a partir de la posterior y compararlas con los datos.

nreps <- 10000
vals  <- 0:20  # posibles valores de remoción
res   <- matrix(NA, nreps, length(vals) - 1)  # matriz para resultados
p_sim <- rbeta(nreps, alpha_p, beta_p)      # muestra aleatoria n=10000 de la posterior (vector)

for (i in 1:nreps) {
  tmp <- rbinom(nobs, frutos, p_sim[i])
  res[i, ] <- hist(tmp, right = FALSE, breaks = vals, plot = FALSE)$density
} 
# res es ahora una matriz cuyas filas es cada distribucion binomial generada a partir del vector p_sim. 
# Las columnas son valores de densidad que arma el histograma.

plot(table(removidos)/nobs, xlim = c(0, 10), ylim = c(0, 0.6), ylab = "frecuencia", 
     type = "p", pch = 19)
library(coda)
ci <- HPDinterval(as.mcmc(res))
lines(0:19, ci[, 2])
lines(0:19, ci[, 1])








## Analisis bayesiano con numeros reales ----

url <- "https://sites.google.com/site/modelosydatos/quintral.txt"
quintral <- read.table(url, header = TRUE)

# previas
alpha <- 1
beta <- 1

# posterior
alpha_p <- alpha + sum(quintral$Removidos)
beta_p <- beta + sum(quintral$Frutos - quintral$Removidos)

op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
curve(dbeta(x, alpha_p, beta_p), lwd = 2, ylab = "Densidad de probabilidad", xlab = "Probabilidad de remoción")
curve(dbeta(x, 1, 1), lwd = 2, col = "gray", add = TRUE)
text(0.2, 2.5, "previa")
text(0.65, 20, "posterior")
par(op)


# simulando datos para ver si el modelo es correcto
nreps <- 10000
nobs <- length(quintral$Removidos)

vals <- 0:60  # posibles valores de remoción
res <- matrix(NA, nreps, length(vals) - 1)  # matriz para resultados
p_sim <- rbeta(nreps, alpha_p, beta_p)  # muestra aleatoria de probabilidades del modelo posterior

for (i in 1:nreps) {
  tmp <- rbinom(nobs, quintral$Frutos, p_sim[i]) # uso esa muestra para generar puntos "observados" para la simulacion i.
  res[i, ] <- hist(tmp, right = FALSE, breaks = vals, plot = FALSE)$density
}

plot(table(quintral$Removidos)/nobs, xlim = c(0, 60), ylim = c(0, 0.2), ylab = "frecuencia", 
     xlab = "removidos", type = "p", pch = 19)
library(coda)
ci <- HPDinterval(as.mcmc(res))
lines(0:59, ci[, 2])
lines(0:59, ci[, 1])


### Pregunta: ¿Qué pueden decir sobre la probabilidad de observar estos datos bajo el modelo Binomial que ajustamos?
# Que es baja, dado que el modelo no ajusta bien para conteos muy altos o muy bajos observardos. Estos pts caen por fuera del 
# intervalo de credibilidad generado a partir del modelo.









## usando JAGS para resolución numérica en enfoque bayesiano ----

set.seed(123)
nobs <- 30  # número de observaciones (plantas)
frutos <- rep(20, nobs)  # frutos disponibles
p_rem <- 0.2  # probabilidad de remoción por fruto
removidos <- rbinom(nobs, size = frutos, prob = p_rem)
# previas
alpha <- 1
beta <- 1

# posterior (modelo analítico para comparar)
alpha_p <- alpha + sum(removidos)
beta_p <- beta + sum(frutos - removidos)


# defieniendo el modelo en JAGS
cat(file = "rem.bug", "
model{
    # likelihood
    for (i in 1:nobs) {    
      removidos[i] ~ dbin(theta, frutos[i])
    }
    # previa
    theta ~ dbeta(1, 1)    
  }")

# lista con datos para pasarle a JAGS
m.data <- list("nobs", "frutos", "removidos")
inits <- function() list(theta = runif(1, 0, 1))
params <- c("theta")

#Finalmente, definimos cuántas iteraciones queremos ejecutar por cada cadena, cuántos valores vamos a ir descartado en una secuencia 
# ("thin"), qué largo tiene el "burn in" y cuántas cadenas queremos correr.
ni <- 1000  # número de iteraciones
nt <- 1  # tasa de 'thining' 
nb <- 500  # cuantas iteraciones usamos de 'burn in'
nc <- 3  # y cuantas cadenas corremos

library(jagsUI)

m.sim <- jags(data = m.data, inits = inits, parameters.to.save = params, model.file = "rem.bug", 
              n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Resumen del modelo hecho con cadenas de markov
print(m.sim)

### Pregunta: ¿Qué usamos para comparar el valor estimado de la probabilidad de remoción que estima JAGS con el resultado analítico que vimos antes?
# La media de tita mostrada en la tabla. Para que sea un valor válido, Rhat debe ser menor a 1 y tener un n efectivo suficiente.

  
# En el objeto de salida de JAGS quedan guardadas varias cosas. Para ver qué cosas, podemos hacer:
str(m.sim)

# gráfico de las cadenas Markovianas del parámetro de probabilidad de remoción
jagsUI::traceplot(m.sim, parameters = c("theta"))



### Pregunta: ¿Qué les parece importante ver en este tipo de gráficos? 
# Que las cadenas converjan a un valor de media para el parámetro.


# Podemos graficar la posterior:
op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
plot(density(m.sim$sims.list$theta), lwd = 2, main = "", ylim = c(0, 35), xlab = expression(theta))
par(op)

# y comparar con el modelo analitico/teorico:
op <- par(cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
plot(density(m.sim$sims.list$theta), lwd = 2, main = "", ylim = c(0, 35), xlab = expression(theta))
curve(dbeta(x, alpha_p, beta_p), add = TRUE, lwd = 2, lty = 2)
par(op)


### Pregunta: ¿Qué pueden decir de las diferencias entre estas dos curvas?
# La curva analítica es una curva paramétrica, mientras que la curva dada por el enfoque bayesiano representa una muestra
# de una distribución posterior. Es esperable que sean parecidas, ya que a medida que la muestra posterior se agranda, su 
# curva de densidad se aproxima a aquella obtenida analíticamente, y se hacen iguales para una muestra de tamaño infinito. 
# La diferencia se da entonces, porque la muestra de la posterior es finita y no representa perfectamente la distribución
# posterior conocida por la resolución analítica.

# Si queremos por ejemplo estimar la probabilidad de que la tasa de remoción por fruto sea menor al 16% hacemos:
length(which(m.sim$sims.list$theta < 0.16))/length(m.sim$sims.list$theta)

# y la probabilidad de que la tasa de remoción por fruto sea mayor que 20%:
length(which(m.sim$sims.list$theta > 0.2))/length(m.sim$sims.list$theta)


### Pregunta: ¿Cómo harían esos calculos en base a los resultados teóricos? 
# con pbeta y usando los alpha posteriores. 

pbeta(0.16, alpha_p, beta_p, lower.tail = T)
pbeta(0.20, alpha_p, beta_p, lower.tail = F)


# También podemos calcular el intervalo de credibilidad:
library(coda)
HPDinterval(as.mcmc(m.sim$sims.list$theta))





