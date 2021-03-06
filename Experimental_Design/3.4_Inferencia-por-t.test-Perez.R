# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-Dise�o experimental
# Trabajo pr�ctico N� 3: Inferencia y primeras pruebas estad�sticas

# Pablo Tom�s Campos B�ttges, Gustavo Ezequiel Perez

rm()
#######################################################################################
int=function(x, ns, it){     
  sam1=sample(x, ns, replace=F) 
  sam2=sample(x, ns, replace=F)
  dif=mean(sam1)-mean(sam2)   
  se=(((sd(sam1)^2+sd(sam2)^2)/2)^.5)*(2/ns)^.5 
  dif+it*se      
}    
#######################################################################################
#Parte 1

iter=200 # objeto num�rico
size=array(NA, iter) # array vacio del tama�o de iter.
alfa=array(NA, iter) # array vacio del tama�o de iter.

for(i in 1:iter){
  np=i*5   # objeto np dependiente de la iteraci�n.
  x=rnorm(np, 5.2, 1.25) # vector num�rico con sus respectivos n�meros distribuidos de manera aprox normal con media y desv�o aprox a 5.2 y 1.25 correspondientemente 
ns=5 # objeto ns 
sim=1000 # objeto sim
it=qt(c(.025, .975), df=2*ns-2) # cuantiles del 2,5% y 97.5% para una distribuci�n t con 8 grados de libertad. 
dat=data.frame(t(replicate(sim, int(x, ns, it)))) # dataframe con 1000 IC 95% para medias de muestras aleatorias n=5, basados en una distribuci�n t de difefencia de medias.
names(dat)=c("lo", "hi") # nombres para las columnas.
mu=0 # objeto mu (media esperada bajo la hipotesis nula de que las medias no difieren)
rech=ifelse(mu<dat$lo| mu>dat$hi, 1, 0)  # si mu esta fuera de los intervalos de confianza devuelve 1, en caso contrario, 0.
size[i]=np # llena el array con el valor de np
alfa[i]=100*sum(rech)/sim # llena el array con el porcentaje de veces que los IC95% no tomaron el mu = 0 
}

simu=data.frame(size, alfa) #une los arrays anteriores
attach(simu)
plot(size, alfa, ylim=c(0, 10), xlab="Population size", ylab="Type I error (%)") # plotea el % del error tipo 1 cometido respecto al tama�o poblacional.
abline(5, 0, lty=2) # agregado de linea. Intersecci�n=5, pendiente=0  

###########################################
#Parte 2
par(mfrow = c(3, 1)) # seteo de grilla de ploteos
par(mar = c(0,0,2,0), oma = c(5, 5, 2, 2)) # margenes de la grillas.


for(i in 1:3){
n=10^i # objeto n dependiente de la iteraci�n.
y=dat[c(1:n),] # subset del dataframe "dat". Extracci�n de filas de tama�o n.
x=as.numeric(rownames(y)) # transformo los nombres de la filas del subset en objetos num�ricos.
plot.new() # nuevo ploteo
plot.window(xlim=c(.5,n), ylim=c(-5,5)) # seteo de las coordenadas para la ventana gr�fica.
axis(2, at=seq(-4, 4, 2), cex.axis=1, lwd=1, las=2)  # ploteo del eje y
abline(0,0, col="red") # agregado de linea roja, intersecci�n = 0 , pendiente =0
ylo=y$lo # extrae una columna del subset "y"
yhi=y$hi # extrae una columna del subset "y"
arrows(x, ylo, x, yhi, length=0.05, angle=90, code=0) # lineas del largo de los IC95% 

mtext (paste("No. de muestras =", 10^i) , side = 3, line = -.5, adj = 0.05,
       cex = 0.6, col = "black") # texto sobre el plot

}

mtext("Long. hoja (cm)", side = 2, outer = TRUE, cex = .8, line = 3.2) # texto sobre el plot. 
