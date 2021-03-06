# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-Dise�o experimental.
# Trabajo pr�ctico N� 3: M�xima verosimilitud.
# Ezequiel Perez ^-^


# c�lculo de m�xima verosimilitud basado en distribuci�n binomial.

k<-135
n<-207
prob<-seq(0,1, length=500)
log.likelihood<-dbinom(k , n, prob = prob , log = TRUE)
plot(prob, log.likelihood, xlab = 'probabilidad (p)', ylab = 'log-likelihood', type = "l")


# aproximaciones:
puntos<-cbind(prob, log.likelihood)  
Mll<-puntos[326,] # busqu� en la base de datos "puntos", el valor que me daba el log.likelihood m�s cercano a cero.
cotasup<-puntos[358,] # busqu� en "puntos" el valor de "prob" que m�s se aproximaba a Mll - 2 unidades.
cotainf<-puntos[293,]# busqu� en "puntos" el valor de "prob" que m�s se aproximaba a Mll - 2 unidades.

ic<-c(0.65, cotainf[1], cotasup[1])
names(ic)<-c('p', '0.125%','97,5%')
ic

# lineas en el gr�fico:
lines(x=c(0.65, 0.65), y=c(-735,0), col='red')
lines(x=c(0.71, 0.71), y=c(-735,0), col='orange', lty=5)
lines(x=c(0.58, 0.58), y=c(-735,0), col='orange', lty=5)

