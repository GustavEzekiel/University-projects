# TP5 Probabilidades y Distribuciones
# Ezequiel Perez
# 19.04.21

######################################################-----

#### DISTRIBUCIONES DISCRETAS

### Ejercicio 1: 
## Â¿CuÃ¡l es la probabilidad teÃ³rica de obtener y = 0 para lambda = 1?
dpois(0, lambda = 1)

## Â¿CuÃ¡l es la probabilidad teÃ³rica de y > 2 para lambda = 1?
1-ppois(2, lambda = 1)

## Comparar la probabilidad de y = 0 para lambda = 1 de la distribuciÃ³n teÃ³rica 
##con la obtenida empÃ­ricamente con una muestra de 10, 100, 1000 y 10000 
##observaciones
y1 = rpois(10, lambda = 1)
y2 = rpois(100, lambda = 1)
y3 = rpois(1000, lambda = 1)
y4 = rpois(10000, lambda = 1)

yy1 = table(y1)/10
yy2 = table(y2)/100
yy3 = table(y3)/1000
yy4 = table(y4)/10000

y1 = yy1['0']
y2 = yy2['0']
y3 = yy3['0']
y4 = yy4['0']

tprob = dpois(0, lambda = 1)

yn = c(y1, y2, y3, y4)
ns = as.factor(c('10', '100', '1000', '10000'))
barplot(yn, col='pink', space=1/2, names.arg=c('n=10', 'n=100', 'n=1000', 'n=10000'), 
	cex.names=0.9, ylim = c(0, 0.5), ylab = 'probabilidad de y=0')
abline(h = tprob, col = 'red', lwd = 1.5)

## Ahora con las mismas muestras, calcular los intervalos de 95% teÃ³ricos y empÃ­ricos.

#intervalo teorico
qpois(c(0.00, 0.95), lambda = 1)

#intervalos empiricos
quantile(yy1, probs = c(0.00, 0.95), na.rm=T)
quantile(yy2, probs = c(0.00, 0.95), na.rm=T)
quantile(yy3, probs = c(0.00, 0.95), na.rm=T)
quantile(yy4, probs = c(0.00, 0.95), na.rm=T)



### Ejercicio 2: ¿Cómo cambia la forma de la distribución de Poisson a medida que cambia lambda?
lista =  list()
for (i in 1:9){
	lista[[i]] = rpois(1000, lambda = i)
}

op = par(mfrow=c(3,3), mai = c(.25, .25, .25, .25), oma = c(3, 3, .1, .1))
for (i in 1:9){
	hist(lista[[i]], col = 'pink', breaks = 10, main = paste('lambda = ', i, sep = ''), ylab = '', 
	xlab = '')
}
mtext('y', 1, outer = T, line = 1)
mtext('Frecuencia', 2, outer = T, line = 1)
par(op)


### Ejercicio 3: Otras distribuciones discretas. Ver otras distribuciones del bestiario. 
### ¿Qué distribución les parece más adecuada para los siguientes tipos de datos? (Justificar la respuesta!): ver la resp. en el markdown


#### DISTRIBUCIONES CONTINUAS
### preguntas parte distribucion continua:

set.seed(1234)
n <- 60
mu <- 0
sigma <- 1
y <- rnorm(n, mean = mu, sd = sigma)

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
hist(y, breaks = 20, freq = FALSE, main = "")
lines(density(y), lwd = 3, lty = 3, col = "red")
curve(dnorm(x, mean = mu, sd = sigma), lwd = 3, add = TRUE, xlim = c(-4, 4))
par(op)

## Visualmente, ¿Qué tan parecida es la distribución empírica con la teórica?
# es bastante diferente, las modas de ambas distribuciones no se superponen.

## ¿Cómo cambia esa similitud si duplico el esfuerzo de muestreo?
n <- 120
mu <- 0
sigma <- 1
y <- rnorm(n, mean = mu, sd = sigma)

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
hist(y, breaks = 20, freq = FALSE, main = "")
lines(density(y), lwd = 3, lty = 3, col = "red")
curve(dnorm(x, mean = mu, sd = sigma), lwd = 3, add = TRUE, xlim = c(-4, 4))
par(op)

# Mas muestreo aproxima la curva empirica a la teorica, en este caso se aproxima bastante al punto de que los picos coinciden.

## ¿Y si logramos tener 800 observaciones?

n <- 800
mu <- 0
sigma <- 1
y <- rnorm(n, mean = mu, sd = sigma)

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
hist(y, breaks = 20, freq = FALSE, main = "")
lines(density(y), lwd = 3, lty = 3, col = "red")
curve(dnorm(x, mean = mu, sd = sigma), lwd = 3, add = TRUE, xlim = c(-4, 4))
par(op)

# mas muestreo aproxima la curva empirica a la teorica. Es esperable, ya que la curva teorica es aquella que se obtendria con muestras
# de tamanio infinito.


### Ejercicio 1: ¿Cómo cambia la forma de la distribución cuando cambia sigma? Simular datos con distintos sigma y hacer histogramas.

n <- 1000
mu <- 0
sigma <- seq(.5, 4.5 , by = .5)

lista =  list()
for (i in 1:length(sigma)){
	lista[[i]] = rnorm(n, mean = mu, sd = sigma[i])
}

op = par(mfrow=c(3,3), mai = c(.25, .25, .25, .25), oma = c(3, 3, .1, .1))
for (i in 1:length(sigma)){
	hist(lista[[i]], col = 'pink', breaks = 20, main = paste('sigma = ', sigma[i], sep = ''), ylab = '', 
	xlab = '')
}
mtext('y', 1, outer = T, line = 1)
mtext('Frecuencia', 2, outer = T, line = 1)
par(op)

# mayor sigma implica mas dispersion de los datos en las simulaciones. Siguen una distribucion normal pero se distribuyen mas ampliamente.


### Ejercicio 2: ver pdf y markdown


#### EXTENDIENDO DISTRIBUCIONES

### Ejercicio 1: 1. Simulemos un set de datos de diferencias en el caudal de un río según las precipitaciones. 
# Asumimos que la relacion entre las precipitaciones (pp) y la diferencia en el caudal del
# rio (delta) es la siguiente:
# delta = -20 + 0.2 * pp
# Además asumimos que la variabilidad en el caudal del río es de sigma = 5mm

pp = rep(seq(0, 30, by = 3), times = 11)
n = length(pp)
delta <- rnorm(n, mean = -60 + pp * 4.2, sd = 5)

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
plot(pp, delta, pch = 21, bg = "gray", xlab = "precipitaciones", ylab = "diferencia de caudal")
par(op)

### Ejercicio 2: Simulemos ahora el número de crías de renacuajos que sobrevivieron del total de crías
# que nacieron según como fue de frío el invierno. Sabemos que la relacion entre la
# probabilidad de supervivencia de las crias (psup) y la temperatura media del invierno
#(temp) es la siguiente: delta = -20*(1-exp(0.02*temp))

temp     = rep(seq(0, 15, by = 1), times = 7)
Nmuestra = 25                                   # cantidad de crias por camada que se asume es cte.
n        = length(temp)
delta    = rbinom(n, prob = (-20 * (1 - exp(0.02*temp)))/Nmuestra, size = Nmuestra)/Nmuestra

op <- par(cex.lab = 1 , font.lab = 1, cex.axis = 1, las = 1, bty = "n", mar = c(4.5,4.5,1,1))
plot(temp, delta, pch = 21, bg = "gray", xlab = "temperatura media del invierno", ylab = "p. supervivencia",
xlim = c(min(temp), max(temp)+5))
par(op)









