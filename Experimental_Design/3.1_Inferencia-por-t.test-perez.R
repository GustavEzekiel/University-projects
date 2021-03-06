# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-Dise�o experimental
# Trabajo pr�ctico N� 3: Inferencia y primeras pruebas estad�sticas

# Pablo Tom�s Campos B�ttges, Gustavo Ezequiel Perez



rm()

#######################################################################################
int=function(x, ns, it){
  x=sample(x, ns, replace=F)
  mean(x)+(it*sd(x)/(ns^.5))}  
#######################################################################################

# Parte 0 
np=1000  #correr varias veces la parte 0 reemplazando np con valores crecientes de 10 a 1000
sam=rnorm(np, 5.2, 1.25) # vector de n�meros de tama�o np, media=5,2 y sd=1,25
h = hist(sam, breaks=seq(-20,20,1)) # histograma de frecuencias absolutas a partir de datos del vector anterior, con los intervalos especificados.
h$density = h$counts/sum(h$counts)*100 # transforma en % las frecuencias absolutas graficadas en el histograma. (�no era mas facil h$density=h$density*100?)
plot(h,freq=FALSE, main="", xlab="Longitud (cm)", ylab="Frequency (%)", # freq= FALSE implica que el histograma no ser� de frecuencias absolutas.
     border="black",  col="green", xlim=c(0, 10), ylim=c(0, 40), las=1 ) #Histograma con las frecuencias relativas transformadas a %.
curve(100*dnorm(x, mean=mean(sam), sd=sd(sam)), col="darkblue", lwd=1, add=TRUE, yaxt="n") #curva de densidad normal, multiplicada por 100

q=quantile(sam, c(.16, .5,  .84)) # cuantiles del 16%, 50% y 84% del vector sam.   
points(q, rep(4,3), pch=c(25,24, 25),  cex=1,col=c("red", "blue", "red"),
       bg=c("red", "blue", "red")) # agrega puntos al gr�fico dados por los valores de los cuantiles calculados en la sentencia anterior.
points(mean(sam), 2, pch=3,  cex=2,col="black", bg="black") # agrega punto al gr�fico dado por la media del vector sam.
library(shape) # carga librer�a
Arrows(mean(sam)-sd(sam), 2, mean(sam)+sd(sam), 2,  code=3, lwd=1, 
       arr.lwd= .1, arr.type = "T",  col = "gray14") # agrega flechas al gr�fico dadas por las coordenadas x=media +/- sd , y=2

###########
#Parte 1

iter=200 # objeto iter 
size=array(NA, iter) #array vac�o del tama�o de iter.
alfa=array(NA, iter) #array vac�o del tama�o de iter.

for(i in 1:iter){ # iteraci�n del 1 al 200
  np=i*5  # objeto np con valor de la iteracion * 5
  sam=rnorm(np, 5.2, 1.25) # vector con valores de np distribuidos normalmente con media 5,2 y desv�o 1,25

ns=5 # objeto ns
sim=1000 # objeto sim
it=qt(c(.025, .975), df=ns-1) # vector num�rico con los valores dados por los cuantiles 2,5% y 97,5% de una distribuci�n t con 4 grados de libertad.
dat=data.frame(t(replicate(sim, int(sam, ns, it)))) #dataframe compuesto de 1000 IC 95% (repeticiones de la funci�n int). Int (con estos par�metros) calcula IC 95% estimado a partir de media y desv�o de una muestra aleatoria de 5 elementos de sam, para una distribuci�n t-student.
names(dat)=c("lo", "hi") # nombro las columnas del dataframe "dat"
mu=mean(sam) # media de sam
rech=ifelse(mu<dat$lo| mu>dat$hi, 1, 0) # si mu esta fuera del IC 95% rech=1, en caso contrario, rech=0
size[i]=np # cargo el array con el valor el tama�o de sam
alfa[i]=100*sum(rech)/sim # % de mu fuera del IC 95% de mil muestras de sam. 
}

simu=data.frame(size, alfa) # union de arrays llenados por el loop anterior en un data frame.
attach(simu)
plot(size, alfa, ylim=c(0, 10), xlab="Population size", ylab="Type I error (%)") # ploteo gr�fico de dispersi�n de alpha vs size.
abline(5, 0, lty=2) # linea sobre el gr�fico, intercepci�n con el eje y=5, pendiente=0


################
#Parte 2  (correr Parte 1 antes)

par(mfrow = c(3, 1)) # seteo de grilla de gr�ficos
par(mar = c(0,0,2,0), oma = c(5, 5, 2, 2))  # seteo de m�rgenes de la grilla  


for(i in 1:3){
  n=10^i # objeto num�rico dependiente del n�mero de iteraci�n. 
  y=dat[c(1:n),] #extracci�n de filas del data frame dat.
  x=as.numeric(rownames(y)) # conversi�n a vector num�rico los nombres de las filas del subset "y" creado en la sentencia anterior.
  plot.new() # ploteo en blanco
  plot.window(xlim=c(.5,n), ylim=c(0,10)) # seteo de sistema de coordenadas para la ventana gr�fica. 
  axis(2, at=seq(0, 10, 2), cex.axis=1, lwd=1, las=2) # ploteo de eje y solamente
  abline(5.2,0, col="red") # linea roja, intersecci�n eje y= 5,2 , pendiente = 0
  ylo=y$lo # 'extracci�n' de columna del subsey y
  yhi=y$hi # 'extracci�n' de columna del subsey y
  arrows(x, ylo, x, yhi, length=0.05, angle=90, code=0) # flechas en el gr�fico; lineas rectas a 90� del largo dado por IC 95% 
  
  mtext (paste("No. de muestras =", 10^i) , side = 3, line = .5, adj = 0.05, cex = 0.6, col = "black") # t�tulo de cada gr�fico.
  
}

mtext("Long. hoja (cm)", side = 2, outer = TRUE, cex = .8, line = 3.2) # texto fuera de los tres gr�ficos. 

