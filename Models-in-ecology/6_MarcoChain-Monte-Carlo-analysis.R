# TP 10 MCMC
# Modelos en ecología 2021




################################### own setting ----

setwd("D:/Documents/Uncoma/Math & stadistics/Modelos en ecología/Tps/tp10-MCMC")
set.seed(1)






################################### primera parte ----

#### ejercicios 


### 1- Cómo cambian las posteriores cuando cambia el número de datos? Probar con n = 5, n = 100 y n = 500.

## calculo del Log-likelihood

like <- function(x, y, b0, b1){
lambda <- exp(b0 + b1 * x)
ll <- sum(dpois(y, lambda = lambda, log = TRUE))
}

## MCMC

mcmc <- function(n, sd.jump){

	# simulacion de datos

	n <- n
	b0 <- 0
	b1 <- 0.9
	x <- runif(n, -2, 2)
	y <- rpois(n, lambda = exp(b0 + b1 * x))

	# parametros de MCMC

	n.iter <- 10000      # iteraciones de la cadena
	m.b0 <- 0            # previa para b0
	m.b1 <- 0            
	sd.b0 <- 5           # Desvio de la previa para b0    
	sd.b1 <- 1
	sd.jump <- sd.jump         # desvio de la distribución de propuestas para b0 y b1     


	# objetos para guardado de valores simulados y sus LL.

	sims <- matrix(NA, n.iter, 2)  # n.iter filas, 2 columnas.
	pos <- numeric(n.iter)
	
	# valores iniciales
	
	new.b0 <- runif(1, -1, 1)
	new.b1 <- runif(1, -1, 1)
	
	pos[1] <- like(x, y, new.b0, new.b1) +
	  dnorm(new.b0, m.b0, sd.b0, log = TRUE) +
	  dnorm(new.b1, m.b1, sd.b1, log = TRUE)
	
	sims[1, 1] <- new.b0
	sims[1, 2] <- new.b1

	# MCMC

	for(i in 2:n.iter){
	# si no aceptamos nuevos valores, todo queda como antes
		pos[i] <- pos[i-1]
		sims[i,] <- sims[i-1,]
		# update b0
		new.b0 <- rnorm(1, sims[i-1, 1], sd.jump)
		n.pos <- like(x, y, new.b0, sims[i-1, 2]) +
			dnorm(new.b0, m.b0, sd.b0, log = TRUE) +
			dnorm(sims[i-1, 2], m.b1, sd.b1, log = TRUE)
		if( (n.pos - pos[i]) > log(runif(1)) ){
			pos[i] <- n.pos
			sims[i,1] <- new.b0
		}
		#update b1
		new.b1 <- rnorm(1, sims[i-1,2], sd.jump)
		n.pos <- like(x, y, sims[i, 1], new.b1) +
			dnorm(sims[i, 1], m.b0, sd.b0, log = TRUE) +
			dnorm(new.b1, m.b1, sd.b1, log = TRUE)
		if( (n.pos - pos[i]) > log(runif(1)) ){
			pos[i] <- n.pos
			sims[i, 2] <- new.b1
		}
	}
	return(sims)	
}

## burn in

burn = ceiling(n.iter/2)

## graficando las posteriores (conjuntas y marginales)

# con n = 5

sims <- mcmc(5,1)

library(ggplot2)
library(ggExtra)
library(gridExtra)
df <- data.frame(b0 = sims[burn:n.iter,1], b1 = sims[burn:n.iter,2])
p <- ggplot(df, aes(b0, b1)) + geom_point(alpha = 0.1) + theme_classic() + xlim(-1, 1) + ylim(0, 1.5) 
ggExtra::ggMarginal(p, type = "histogram")


# con n = 100

sims <- mcmc(100,1)

library(ggplot2)
library(ggExtra)
library(gridExtra)
df <- data.frame(b0 = sims[burn:n.iter,1], b1 = sims[burn:n.iter,2])
p <- ggplot(df, aes(b0, b1)) + geom_point(alpha = 0.1) + theme_classic() + xlim(-1, 1) + ylim(0, 1.5) 
ggExtra::ggMarginal(p, type = "histogram")


# con n = 500

sims <- mcmc(500,1)

library(ggplot2)
library(ggExtra)
library(gridExtra)
df <- data.frame(b0 = sims[burn:n.iter,1], b1 = sims[burn:n.iter,2])
p <- ggplot(df, aes(b0, b1)) + geom_point(alpha = 0.1) + theme_classic() + xlim(-1, 1) + ylim(0, 1.5) 
ggExtra::ggMarginal(p, type = "histogram")

### respuesta
# a medida que aumenta el n de datos, la posterior tiene menos dispersión. Cuando n -> infinito la posterior
# debería converger a un valor para cada parámetro.



### 2- Modifique el scrip anterior para contar cuántas veces se acepta mover la cadena Markpviana para cada parámetro

mcmc <- function(n, sd.jump){

	# simulacion de datos

	n <- n
	b0 <- 0
	b1 <- 0.9
	x <- runif(n, -2, 2)
	y <- rpois(n, lambda = exp(b0 + b1 * x))

	# parametros de MCMC

	n.iter <- 10000      # iteraciones de la cadena
	m.b0 <- 0            # previa para b0
	m.b1 <- 0            
	sd.b0 <- 5           # Desvio de la previa para b0    
	sd.b1 <- 1
	sd.jump <- sd.jump         # desvio de la distribución de propuestas para b0 y b1  

	# objetos para guardado de valores simulados y sus LL.

	sims <- matrix(NA, n.iter, 2)  # n.iter filas, 2 columnas.
	pos <- numeric(n.iter)
	
	# valores iniciales
	
	new.b0 <- runif(1, -1, 1)
	new.b1 <- runif(1, -1, 1)
	
	pos[1] <- like(x, y, new.b0, new.b1) +
	  dnorm(new.b0, m.b0, sd.b0, log = TRUE) +
	  dnorm(new.b1, m.b1, sd.b1, log = TRUE)
	
	sims[1, 1] <- new.b0
	sims[1, 2] <- new.b1

	# MCMC

	count = 0
	for(i in 2:n.iter){
	# si no aceptamos nuevos valores, todo queda como antes
		pos[i] <- pos[i-1]
		sims[i,] <- sims[i-1,]
		# update b0
		new.b0 <- rnorm(1, sims[i-1, 1], sd.jump)
		n.pos <- like(x, y, new.b0, sims[i-1, 2]) +
			dnorm(new.b0, m.b0, sd.b0, log = TRUE) +
			dnorm(sims[i-1, 2], m.b1, sd.b1, log = TRUE)
		if( (n.pos - pos[i]) > log(runif(1)) ){
			pos[i] <- n.pos
			sims[i,1] <- new.b0
		}
		#update b1
		new.b1 <- rnorm(1, sims[i-1,2], sd.jump)
		n.pos <- like(x, y, sims[i, 1], new.b1) +
			dnorm(sims[i, 1], m.b0, sd.b0, log = TRUE) +
			dnorm(new.b1, m.b1, sd.b1, log = TRUE)
		if( (n.pos - pos[i]) > log(runif(1)) ){
			pos[i] <- n.pos
			sims[i, 2] <- new.b1
		}
		if (pos[i] != pos[i-1]){
			count = count + 1
		}
	}
	return(count)	
}

### respuesta
veces.aceptadas <- mcmc(30, 1)
print(paste("veces aceptadas:", veces.aceptadas, "de", n.iter, sep = " "))



### 3- Qué valor de sd.jump resulta en aproximadamente 30% de aceptación en movimientos de las cadenas?

sim <- 100
r <- numeric(sim)
for (i in 1:sim){
  veces.aceptadas <- mcmc(30, 1.0)
  r[i]<-( veces.aceptadas / n.iter ) *100
}

hist(r)

### respuesta

# para un sd.jump de uno, alrrededor del 30% de las propuestas son aceptadas.










################################### segunda parte ----


#### Corriendo más de una cadena.

# simulacion de datos

n <- 30
b0 <- 0
b1 <- 0.9
x <- runif(n, -2, 2)
y <- rpois(n, lambda = exp(b0 + b1 * x))

# valores iniciales
for(i in 1:n.chain){
  new.b0 <- runif(1, -1, 1)
  new.b1 <- runif(1, -1, 1)
  pos[1,i] <- like(x, y, new.b0, new.b1) +
    dnorm(new.b0, m.b0, sd.b0) +
    dnorm(new.b1, m.b1, sd.b1)
  sims[[i]][1, 1] <- new.b0
  sims[[i]][1, 2] <- new.b1
}

# mcmc parameters
n.iter <- 10000
n.chain <- 3
m.b0 <- 0
m.b1 <- 0
sd.b0 <- 1
sd.b1 <- 1
sd.jump <- 1
pos <- matrix(NA, n.iter, n.chain)
sims <- vector("list", n.chain)

# store objects
for(i in 1: n.chain){
	sims[[i]] <- matrix(NA, n.iter, 2)
}

# chain running
for(i in 2:n.iter){
	for(j in 1: n.chain){
		# si no aceptamos nuevos valores, todo queda como antes
		pos[i,j] <- pos[i-1,j]
		sims[[j]] [i,] <- sims[[j]] [i-1,]
		# update b0
		new.b0 <- rnorm(1, sims[[j]] [i-1, 1], sd.jump)
		n.pos <- like(x, y, new.b0, sims[[j]] [i-1, 2]) +
		dnorm(new.b0, m.b0, sd.b0) +
		dnorm(sims[[j]] [i-1, 2], m.b1, sd.b1)
		if( (n.pos - pos[i,j]) > log(runif(1)) ){
		pos[i,j] <- n.pos
		sims[[j]] [i,1] <- new.b0
		}
		#update b1
		new.b1 <- rnorm(1, sims[[j]] [i-1,2], sd.jump)
		n.pos <- like(x, y, sims[[j]] [i, 1], new.b1) +
		dnorm(sims[[j]] [i, 1], m.b0, sd.b0) +
		dnorm(new.b1, m.b1, sd.b1)
		if( (n.pos - pos[i,j]) > log(runif(1)) ){
		pos[i,j] <- n.pos
		sims[[j]] [i, 2] <- new.b1
		}
	}
}

# evaluando la convergencia de las cadena con "coda"
library(coda)
burn <- ceiling(n.iter/2)
thin <- 1
sim <- vector("list", n.chain)
ps <- mcmc(pos, start = burn, end = n.iter, thin = thin)

for(i in 1:n.chain){
	sim[[i]] <- as.mcmc(cbind(mcmc(sims[[i]], start = burn, end = n.iter, thin = thin), ps[,i]))
	colnames(sim[[i]]) <- list("b0", "b1", "pos")
}

summary(as.mcmc.list(sim))
plot(as.mcmc.list(sim))





### Usando jags

# modelo en lenguaje sintetico
cat(file = "rp.bug",
"
model{
	
	# likelihood
	for (i in 1:n) {
		y[i] ~ dpois(lambda[i])
		log(lambda[i]) <- b0 + b1 * x[i]
	}

	# previas
	b0 ~ dnorm(0, 0.04)
	b1 ~ dnorm(0, 1)
	}"
)

# datos
m.data <- list("n", "y", "x")
inits <- function() list(b0 = runif(1,-1,1),
b1 = runif(1, -1,1))
params <- c("b0", "b1")

# parametros de la cadena.
ni <- 10000 # número de iteraciones
nt <- 1 # tasa de "thining"
nb <- 5000 # cuantas iteraciones usamos de "burn in"
nc <- 5 # y cuantas cadenas corremos

# invocando a jags
library(jagsUI)

jags.sim <- jags (data = m.data,
                  inits = inits,
                  parameters.to.save = params,
                  model.file = "rp.bug",
                  n.chains = nc,
                  n.thin = nt,
                  n.iter = ni,
                  n.burnin = nb)

print(jags.sim)
plot(jags.sim)



### pregunta: Qué tan similares o diferentes son las estimaciones con JAGS comparadas con nuestro MCMC?

# Como se puede apreciar en las salidas mostradas, el cálculo no difiere mucho entre los diferentes métodos. 
# Sin embargo, las distribuciones marginales obtenidas con jags son mas simetricas y mas amplias 




### desafío: JAGS reporta la devianza (-2 por el logaritmo de likelihood) del modelo. Cómo podemos hacer para obtener la devianza del ajuste de nuestro MCMC?

# parametros medios posteriores calculados sin jags
b0 = 0.03348
b1 = 0.72803

# LL para el modelo
ll <- like(x, y, b0, b1)

## devianza del modelo
dev <- -2*ll 
out <- c(dev.calculada = dev, dev.jags= 74.856)
out







################################### tercera parte ----

# estimación con Stan

cat(file = "rp.stan",
"
data {
	int<lower=1> n; // total number of observations
int<lower=0> y[n]; // response variable
vector[n] x;
}
parameters {
real b0;
real b1;
}
model{
//target += normal_lpdf(b1 | 0, 1);
//target += normal_lpdf(b0 | 0, 5);
//target += poisson_log_lpmf(y | b0 + b1 * x);
b0 ~ normal(0,5);
b1 ~ normal(0,1);
y ~ poisson(exp(b0 + b1 * x));
}
generated quantities {
real log_lik;
log_lik = poisson_log_lpmf(y | b0 + b1 * x);
}")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rem_dat <- list(n = n,
								y = y,
								x = x)

stan.sim <- stan(file = 'rp.stan',
								data = rem_dat,
								iter = 10000,
								thin = 1,
								chains = 3)P

print(stan.sim)
plot(stan.sim, plotfun = "hist", pars = "b0", include = FALSE)
plot(stan.sim, plotfun = "hist", pars = "b1", include = FALSE)

#### pregunta: Cómo comparan estas estimaciones con las de JAGS y las de nuestro MCMC?
# las estimaciones y distribuciones dieron bastante similares a los anteriores métodos.






################################### cuarta parte ----

### brms

library(brms)

datos <- data.frame(y, x)
priors <- c(set_prior("normal(0, 1)", class = "b"),
						set_prior("normal(0, 5)", class = "Intercept"))
brm.sim <- brm(y ~ x,
							data = datos,
							family = poisson(),
							prior = priors,
							iter = 10000,
							chains = 3)

print(brm.sim)
plot(brm.sim)