# Tp7: Maxima verosimilitud
# Modelos en ecologia 2020
# Ezequiel Perez


##############################################################################--

# Datos necesarios para el practico
url <- "https://sites.google.com/site/modelosydatos/quintral.txt"
quintral <- read.table(url, header = TRUE)

# Para encontrar el valor de probabilidad de que maximiza el log-likelihood de forma numerica:
p <- seq(0, 1, length = 100)
nLL <- array(NA, length(p))
for (i in 1: length(p)) {
  nLL[i] <- -sum(dbinom(quintral$Removidos, size = quintral$Frutos,
                        prob = p[i], log = TRUE))
}
p[which(nLL == min(nLL))]

# En el caso de la resolucion analitica
sum(quintral$Removidos)/sum(quintral$Frutos)


### Qué pasa si cambiamos la resolución en el vector de probabilidades de remoción?

vent = seq(100, 500, by = 50) # creacion de diferentes ventanas.
res = numeric(length = length(vent)) # vector de guardado.
for (v in 1:length(vent)){
  p <- seq(0, 1, length = vent[v])
  nLL <- array(NA, length(p))
  for (i in 1: length(p)) {
    nLL[i] <- -sum(dbinom(quintral$Removidos, size = quintral$Frutos,
                          prob = p[i], log = TRUE))
  }
  res[v] = p[which(nLL == min(nLL))]
}

plot(vent, res, xlab = 'Ventana', ylab = 'MLL estimado', type = 'b', col = 'orange', lwd = 2)
abline(h = sum(quintral$Removidos)/sum(quintral$Frutos), col = 'black')

# a medida que se achica la ventana la aproximacion numerica es mejor al resultado analitico.



# Calculo de MLL por medio de paquetes:
library(bbmle)

binomNLL1 <- function(p, k, N){
  -sum(dbinom(k, prob = p, size = N, log = TRUE))
}

m1 <- mle2(minuslogl = binomNLL1, start = list(p = 0.5),
           data = list(N = quintral$Frutos, k = quintral$Removidos))

k <- quintral$Removidos
hist(k, breaks = 40, freq = FALSE, ylim = c(0, 0.15), main ="")
points(0:50, dbinom(0:50, size = 35, prob = m1@coef), pch = 16, col="darkgray")



# Sin embargo, el número de frutos disponibles es variable y para una mejor comparación podemos usar un bootstrap (remuestreo)
res <- matrix(0, length(quintral$Frutos), 1000)
for (i in 1: 1000){
  n <- sample(quintral$Frutos, length(quintral$Frutos), replace = TRUE)
  res[,i] <- rbinom(length(quintral$Frutos), n, m1@coef)
}
hist(k, breaks = 40, freq = FALSE, ylim = c(0, 0.15), main = "")
lines(density(res))



### ¿Qué se puede decir sobre el ajuste del modelo a los datos?

# el modelo generado por bootstrap no se ajusta muy bien a la distribucion empirica. Sin embargo, es mejor que sobre el calculado 
# analiticamente con bbmle.



### Ejercicio 1: Estimar las probabilidades de remoción de frutos por sitio y tipo de bosque.

cc = subset(subset(quintral, sitio =='Campanario'), bosque=='c')
cf = subset(subset(quintral, sitio =='Campanario'), bosque=='f')
lc = subset(subset(quintral, sitio =='Llao-Llao'), bosque=='c')
lf = subset(subset(quintral, sitio =='Llao-Llao'), bosque=='f')
tc = subset(subset(quintral, sitio =='Tacul'), bosque=='c')
tf = subset(subset(quintral, sitio =='Tacul'), bosque=='f')
subs = list(cc, cf, lc, lf, tc, tf)

MLLs = numeric(6)
for (i in 1:6){
  x <- mle2(minuslogl = binomNLL1, start = list(p = 0.5),
                  data = list(N = subs[[i]]$Frutos, k = subs[[i]]$Removidos))
  MLLs[i] = x@coef
}
names(MLLs) = c('Campanario_bosque c', 'Campanario_bosque f', 'Llao-Llao_bosque c', 'Llao-Llao_bosque f',
                'Tacul_bosque c', 'Tacul_bosque f')
MLLs




## Deposición de polen en flores de pomelo

# datos
apis <- read.table("https://sites.google.com/site/modelosydatos/polen_Apis.csv", header = T, sep = ',')

op <- par(cex.lab = 1.2 , font.lab = 2, cex.axis = 1,
          bty = "n", las = 1)
plot(apis$visitas, apis$granos, xlab = "Número de Visitas",
     ylab = "Granos de Polen Depositados", pch = 21, bg = "grey")
title(main= expression(paste("Eficiencia de ", italic(Apis~mellifera))))
par(op)

# modelo1: lineal con distribucion de poisson. (glm?)
fnLIN<-function (p) {
  y0 <- p[1]
  a <- p[2]
  lambda <- y0 + a * visitas
  -sum(dpois(granos, lambda, log = TRUE))
}
parnames(fnLIN) <- c("y0", "a")
mLIN <- mle2(fnLIN, start = c(y0 = 10, a = 3),
             data = list(visitas = apis$visitas, granos = apis$granos))
summary(mLIN)

# plot del modelo
op <- par(cex.lab = 1.2 , font.lab = 2, cex.axis = 1,
          bty = "n", las = 1, lwd = 2)
plot(apis$visitas, apis$granos, xlab = "Número de Visitas",
     ylab = "Granos de Polen Depositados", pch = 21, bg = "grey")
title(main= expression(paste("Eficiencia de ", italic(Apis~mellifera))))
x = 0:9 # vector de valores de referencia
lines(x, mLIN@coef[1] + x * mLIN@coef[2])


# modelo 2: supone saturacion del sistema.
fnSAT <- function (p) {
  y0 <- p[1]
  a <- p[2]
  b <- p[3]
  lambda <- y0 + a * (1 - exp(-b * visitas))
  -sum(dpois(granos, lambda, log = TRUE))
}
parnames(fnSAT) <- c("y0", "a", "b")
mSAT <- mle2(fnSAT, start = c(y0 = 10, a = 41, b = 0.16),
             data = list(visitas = apis$visitas, granos = apis$granos))
summary(mSAT)

# plot del modelo
op <- par(cex.lab = 1.2 , font.lab = 2, cex.axis = 1,
          bty = "n", las = 1, lwd = 2)
plot(apis$visitas, apis$granos, xlab = "Número de Visitas",
     ylab = "Granos de Polen Depositados", pch = 21, bg = "grey")
title(main = expression(paste("Eficiencia de ", italic(Apis~mellifera))))
lines(x, mSAT@coef[1] + mSAT@coef[2] * (1 - exp(-mSAT@coef[3] * x)), col = 2)


#### ¿Cómo ajustan estos modelos a los datos? ¿Qué otros modelos podríamos probar?

# No conozco ninguna media de comparacion entre el ajuste de cada modelo para establecer cual se ajusta mejor a los datos.
# Otro modelo a probar, podria incluir una distribucion binomial negativa, u otra curva de ajuste, como una logistica.