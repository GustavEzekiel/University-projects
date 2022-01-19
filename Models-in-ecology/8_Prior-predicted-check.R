# TP N 12
# Prior predicted check
# Ezequiel Perez


########################################################## wd and libraries ----

setwd("D:/Documents/Uncoma/Math & stadistics/Modelos en ecología/Tps/tp12-Prior predicted check")

library("jagsUI")

######################################################### codigo del tp --------


# priors
m = 35
s = 3

# data loading
datos = read.table("https://sites.google.com/site/modelosydatos/peso.txt",
                   header = TRUE)

# random data generation
a = rnorm(100, m, s)
b = rnorm(100, 0, 5)

# curves simulation and plotting.
plot( NULL , xlim = range(datos$largo) ,
      ylim = c(-30, 100) ,
      xlab = "largo",
      ylab = "peso" )
abline( h = 0, lty = 2, lwd = 2, col = rgb(0,0,0, 0.5) )
abline( h = 55, lty = 2, lwd = 2, col = rgb(0,0,0, 0.5) )

xbar <- mean(datos$largo)
for ( i in 1:100 ){
  curve( a[i] + b[i] * (x - xbar),
         from = min(datos$largo),
         to=max(datos$largo),
         add=TRUE,
         col=rgb(0,0,0,0.2) )
}


# Bayes analysis
cat(file = "ml.bug",
    "
  model {
  # Función de likelihood
    for( i in 1:n){
      y[i] ~ dnorm (mu[i], tau)
      mu[i] <- a + b * x[i]
    }
  # Previas
    a ~ dnorm(21, 0.25)
    b ~ dnorm(0, tau_b)
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0, 5)
  }"
)

x <- datos$largo - mean(datos$largo)
y <- datos$peso

params <- c("a", "b", "sigma")

inits <- function() list(a = runif(1, 16, 27),
                         b = runif(1, 0, 1),
                         sigma = runif(1, 0, 5))

dat <- list(x = x,
            y = y,
            n = length(datos$largo),
            tau_b = (1/5)^2)

m5.sim <- jags(data = dat,
               inits = inits,
               parameters.to.save = params,
               model.file = "ml.bug",
               n.chains = 3,
               n.iter = 1000)

print(m100.sim)

plot(density(m100.sim$sims.list$b), main = "",
     xlab = expression(beta), bty = "l", ylim = c(0,1))


# posterior predicted check
nsims <- 1000
n = length(y)

# matriz donde guardamos los datos simulados
y_rep <- matrix(NA, nrow = nsims, ncol = n)
nsample <- length(m5.sim$sims.list$a) # muestras de la posterior disponibles
# vectores para guardar las sumas de residuos
sum_res = numeric(nsims)
sum_res_rep = numeric(nsims)


for(i in 1:nsims){
  idx = sample.int(nsample, 1) # número de muestra que vamos a usar
  m_rep = m5.sim$sims.list$a[idx] + m5.sim$sims.list$b[idx] * x
  sd_rep = m5.sim$sims.list$sigma[idx]
  # simulamos los datos
  y_rep[i, ] = rnorm(n, m_rep, sd_rep)
  # suma de residuos
  sum_res[i] = sum(y - m_rep)
  sum_res_rep[i] = sum(y_rep[i,] - m_rep)
}

hist(rowMeans(y_rep), 100, main = "", xlab = "mean y_rep")
abline(v = mean(y), lwd = 3)

# calculamos el desvío de los datos simulados
sd_sim = numeric(nsims)
for(i in 1:nsims) sd_sim[i] <- sd(y_rep[i,])
hist(sd_sim, 100, main = "", xlab = "sd y_rep", xlim = c(4.5, 8))
abline(v = sd(y), lwd = 3)

plot(sum_res, sum_res_rep, asp = 1)
abline(0, 1)

library(coda)
tmp <- HPDinterval(as.mcmc(y_rep))
idx <- sort(datos$largo, index.return = TRUE)
op <- par(las = 1, bty = "n")
plot(datos$largo, datos$peso, pch = 16, col = gray(0.5, 0.5),
     cex = 1.5, ylim = c(10,55))
lines(datos$largo[idx$ix], tmp[idx$ix, 1], lwd = 3, col = "darkgray")
lines(datos$largo[idx$ix], tmp[idx$ix, 2], lwd = 3, col = "darkgray")





######################################################### codigo propio --------


###### Ejercicios: 




### 1) Extender el modelo de regresión para incluir además del largo, al sexo y el ancho de cola
# como predictoras. Los monitos guardan reservas en la base de la cola, por eso es de esperarse
# que el ancho de cola esté relacionado al peso. Como largo y ancho de cola tienen rangos de
# valores diferentes conviene estandarizar estas dos variables antes de incluirlas en la regresión.

# nuevo modelo

# escribirlo en rmarkdown

# estandarización de variables.

x <- (datos$largo - mean(datos$largo)) / sd(datos$largo)
width <- (datos$a_cola - mean(datos$a_cola)) / sd(datos$a_cola)





### 2) Una vez que estandarizaron largo y ancho de cola, qué previas para los coeficientes de
# regresión les parecen razonables?

# me parece usar como previa razonable un efecto positivo para ambos factores, ya que es el peso de animal 
# se correlaciona directa y positivamente con el largo y el ancho de la cola.

# ploteos exploratorios
op <- par(mfrow = c(1,2))
plot(width, y)
plot(x,y)

# para el ancho de cola

ac  = 1     # media
dac = 3     # desvío

# para el largo de la cola

lc  = 1     # media
dlc = 3     # desvio


# modelo en Jags

cat(file = "mlp1.bug",
    "
  model {
  # Función de likelihood
    for( i in 1:n){
      y[i] ~ dnorm (mu[i], tau)
      mu[i] <- a + b * x[i] + c * width[i] + d * sex[i]
    }
  # Previas
    a ~ dnorm(21, 0.25)
    b ~ dnorm(ac, tau_b)
    c ~ dnorm(lc, tau_c)
    d ~ dnorm(0, tau_d)        
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0, 5)
  }"
)

# variables faltantes
y <- datos$peso 
sex <- datos$sexo

params <- c("a", "b", "c", "d", "sigma")

inits <- function() list(a = runif(1, 16, 27),
                         b = runif(1, 0, 1),
                         c = runif(1, 0, 1),
                         d = runif(1, 0, 1),
                         sigma = runif(1, 0, 5))

dat <- list(x = x,
            y = y,
            sex = sex,              
            width = width,
            n = length(datos$largo),
            tau_b = (1/dlc)^2,
            tau_c = (1/dac)^2,
            tau_d = (1/20)^2,       # previa no informativa para el efecto del sexo
            lc = 1,
            ac = 1
)

m.sim <- jags(data = dat,
                 inits = inits,
                 parameters.to.save = params,
                 model.file = "mlp1.bug",
                 n.chains = 3,
                 n.iter = 1000)

# posteriores
print(m.sim)



### 3) Realizar al menos 3 pp-checks para el nuevo modelo de regresión


##### funcion posterior predicted check 

ppck <- function(nsims) {

  n = length(y)
  
  # matriz donde guardamos los datos simulados
  y_rep <- matrix(NA, nrow = nsims, ncol = n)
  nsample <- length(m.sim$sims.list$a)
  
  # vectores para guardar las sumas de residuos
  sum_res = numeric(nsims)
  sum_res_rep = numeric(nsims)
  
  # simulacion de observaciones posteriores
  for(i in 1:nsims){
    idx = sample.int(nsample, 1) # número de muestra que vamos a usar
    m_rep = m.sim$sims.list$a[idx] + m.sim$sims.list$b[idx] * x + m.sim$sims.list$c[idx] * width + m.sim$sims.list$d[idx] * sex
    sd_rep = m.sim$sims.list$sigma[idx]
    # simulamos los datos
    y_rep[i, ] = rnorm(n, m_rep, sd_rep)
    # suma de residuos
    sum_res[i] = sum(y - m_rep)
    sum_res_rep[i] = sum(y_rep[i,] - m_rep)
  }
  
  # checkeando la variable y
  hist(rowMeans(y_rep), 100, main = "", xlab = "mean y_rep")
  abline(v = mean(y), lwd = 3)
  
  # calculamos el desvío de los datos simulados
  sd_sim = numeric(nsims)
  for(i in 1:nsims) sd_sim[i] <- sd(y_rep[i,])
  hist(sd_sim, 100, main = "", xlab = "sd y_rep", xlim = c(4.5, 8))
  abline(v = sd(y), lwd = 3)
  
  # mirando el desvio
  plot(sum_res, sum_res_rep, asp = 1)
  abline(0, 1)
  
  # intervalos de credibilidad
  library(coda)
  tmp <- HPDinterval(as.mcmc(y_rep))
  idx <- sort(datos$largo, index.return = TRUE)
  op <- par(las = 1, bty = "n")
  plot(datos$largo, datos$peso, pch = 16, col = gray(0.5, 0.5),
       cex = 1.5, ylim = c(10,55))
  lines(datos$largo[idx$ix], tmp[idx$ix, 1], lwd = 3, col = "darkgray")
  lines(datos$largo[idx$ix], tmp[idx$ix, 2], lwd = 3, col = "darkgray")
}


##### posterior predicted check

op <- par(mfrow = c(2,2))

# Posterior predicted check 1, nsim = 1000
ppck(1000)

# Posterior predicted check 1, nsim = 1500
ppck(1500)

# Posterior predicted check 1, nsim = 10000
ppck(10000)



################
save.image("tp12data.Rdata")
