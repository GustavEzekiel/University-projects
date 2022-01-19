# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-Diseño experimental
# Trabajo práctico N° 3: Inferencia y primeras pruebas estadísticas

# Pablo Tomás Campos Böttges, Gustavo Ezequiel Perez


# PUNTO 2

  # calculo de intervalo de confianza del 95% para el estadístico p=0.65

po<-0.5
p<-0.65
n<-207
it<-c(-1.645, 1.645) # intervalo del 90%
int=function(p, n, it){     
  se=(p*(1-p)/n)^.5 
  intervalo=0.65+it*se 
  z=(p-po)/(((po*(1-po))/n)^0.5)
  resultados<-c(z, intervalo)
  names(resultados)<-c('z-score', '5%', '95%') # intervalo del 90% como si fuera a dos colas. Para expresar el intervalo de 95%
  print(resultados)                            # para una cola solo se toma el 5% restante de la cola derecha, por ende el int. 
}                                              # llegaría hasta 1.
int(p, n, it)


#----------------------------------------------------------------------------------------------------------------------------------



