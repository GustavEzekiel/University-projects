# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-Diseño experimental.
# Trabajo práctico N° 6: ANOVA
# Ezequiel Perez ^-^



####MANIPULACIÓN DE DATOS.-----


lizard<-read.csv('lizard.csv', header=T, sep = ',') # cargo base de datos
View(lizard)
str(lizard)
# convierto variables 
lizard$Sexo<-factor(lizard$Sexo)  
lizard$Especie<-factor(lizard$Especie)
lizard$Cola<-factor(lizard$Cola)
lizard$Dedo.corto<-as.numeric(lizard$Dedo.corto)

library('dplyr')
library('reshape')

# saco columnas que me interesan.
bichos<-select(lizard, 'Sexo','Especie', 'LHC', 'Cola', 'Lcola', 'id', 'speed.SCSP', 'speed.CCSP', 'speed.SCCP', 'speed.CCCP')
bichos2<-select(lizard, 'temp.SCSP', 'temp.CCSP', 'temp.SCCP', 'temp.CCCP', 'id')
temp<-melt(bichos2, id.vars = 'id')
View(bichos2)
View(bichos)

lagar<-melt(bichos, id.vars = c('Sexo','Especie', 'LHC', 'Cola', 'Lcola', 'id')) # apilo segun tipo de carrera.
colnames(lagar)<-c('Sexo','Especie', 'LHC', 'Cola', 'Lcola', 'id', 'tratamiento', 'speed') # nombro columnas.
View(lagar)

write.csv(lagar, 'lagartija3.csv', sep = ',') # guardo mi base de datos modificada.


#### ANÁLISIS ESTADÍSTICOS. ----
options(contrasts=c("contr.sum","contr.poly"))

# modelo de bloques aleatorizados
library('lmerTest')
modelo<-lmer(speed ~ tratamiento + (1|id), lagar) # Bloque como factor aleatorio y tratamiento como fijo.
anova(modelo)

#comparación de modelos
sin.bloque<-lm(speed~tratamiento, lagar) # modelo reducido
anova(sin.bloque)
anova(modelo, sin.bloque)

# medias corregidas.
library(lsmeans)
trat.lsm<-lsmeans(modelo,~ tratamiento)

#gráfico de medias corregidas
library('visreg')
ploteo<-visreg(redu, xvar = "tratamiento") 
plot(ploteo, ylab = 'velocidad (m/seg)')


#modelo considerando al bloque como fijo
bloque.fijo<-lm(speed~tratamiento+id, lagar)
anova(bloque.fijo)

#comparación de modelos:
anova(bloque.fijo, sin.bloque)

#gráficos diagnósticos:
op = par(mfrow = c(2, 2)) # realiza 4 graficos en una hoja, 2 arriba y 2 abajo
par(mar = c(5,5,2,2), oma = c(0, 0, 0, 0)) # defino los margenes

plot(bloque.fijo, add.smooth = T) 

par(op)

#### CONTRASTES. ----

#contrastes ortogonales y a priori
contrast(trat.lsm, list(c1=c(0, 0, -1, 1))) # contraste con ceniza vs sin ceniza en corridas con pendiente.
contrast(trat.lsm, list(c2=c(-1, 1, 0, 0))) # contraste con ceniza vs sin ceniza en corridas sin pendiente.







#-----------------------------------------------------------------------------------------------------------------------------