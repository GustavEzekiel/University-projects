# Universidad Nacional del Comahue - Centro Regional Universitario Bariloche - Disenio experimental.

# Tp Numero 7 -modelos mixtos- 
# Campos Tomas & Ezequiel Perez ^-^



#### Manipulacion de datos----

# datos tasa metabolica:
library('dplyr')
m.rate<-read.table('data_m_rate.txt', header=T, sep='')
View(m.rate)
str(m.rate)

m.rate<-select(m.rate, Subclass, Order, Species, BMR_.mlO2.hour., Body_mass_for_BMR_.gr.) # filtrado de columnas de interes
m.rate2<-na.omit(m.rate) # eliminando filas con NA
m.rate2<-m.rate2[c(-579,-578,-577),] # sacando las tres ultimas filas.

m.rate2$Subclass<-as.factor(m.rate2$Subclass) # transformo en factores 
m.rate2$Order<-as.factor(m.rate2$Order)
m.rate2$Species<-as.factor(m.rate2$Species)

# revisando estructura de los datos.
apply(m.rate2, 2, class)
str(m.rate2)
View(m.rate2)




#### Modelos----
options(contrasts=c("contr.sum","contr.poly")) ## needed for correct conditional SS


library(car)   # ANOVA tipo 2,3 y 4.
library(lmerTest)  # tests for general linear mixed models (anova & rand) plus p-values.

#algunos ploteos:
plot(m.rate2$BMR_.mlO2.hour.~m.rate2$Body_mass_for_BMR_.gr.)
plot(log(m.rate2$BMR_.mlO2.hour.)~m.rate2$Body_mass_for_BMR_.gr.)
plot(log(m.rate2$BMR_.mlO2.hour.)~log(m.rate2$Body_mass_for_BMR_.gr.))

library(ggplot2)
ggplot(data = m.rate2, aes(y = log(BMR_.mlO2.hour.), x = log(Body_mass_for_BMR_.gr.))) + #Grafico de la relación discriminando entre subclases 
  geom_point(aes(colour = Subclass), size = 0.6) +
  geom_smooth(aes(colour = Subclass), method = lm, se = T) +
  labs(x = "log(Masa corporal (gr))", y = "log(Tasa metabolica basal (mLO2/hr))") +
  scale_colour_discrete(name = "Subclase")

ggplot(data = m.rate2, aes(y = log(BMR_.mlO2.hour.), x = log(Body_mass_for_BMR_.gr.))) + #Grafico de la relación discriminando entre subclases y orden
  geom_point(aes(colour = Subclass), size = 0.3) +
  geom_smooth(aes(fill = Subclass), method = lm, se = T, size = 0.3, col = "black", show.legend = F) +
  labs(x = "log(Masa corporal (gr))", y = "log(Tasa metabolica basal (mLO2/hr))") +
  facet_wrap(~Order, nrow = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name = "Subclase")



#pendientes y ordenadas aleatorias variables log-log.
rate<-lmer(log(BMR_.mlO2.hour.)~log(Body_mass_for_BMR_.gr.)*Subclass/(log(Body_mass_for_BMR_.gr.)|Order), m.rate2)
plot(rate) # gr?ficos diagn?sticos.
summary(rate) # modelo lineal mixto
Anova(rate, type = 3) # Analisis de la varianza
Anova(rate, type=3, test.statistic= "F", ddf = "Kenward-Roger") # suma de cuadrados tipo 3.
anova(rate, type = 3) # anova tipo 3. (modelo desbalanceado)
rand(rate) # efectos aleatorios.


coef(rate) # medias corregidas por parcial pooling para cada nivel del factor aleatorio.
fixef(rate) # efectos fijos.
ranef(rate) # Me da las desviaciones de cada nivel respecto de esa media general.

library(arm)
se.fixef(rate) # error estandar para la media gral.
se.ranef(rate) # error estandar para cada nivel. Homocedacia (para todos igual) lo calcula usando la informaci?n total (correcci?n por parcial pooling)

# Gráfico de medias corregidas por parcial pooling, según orden
m.orden <- coef(rate)
se.orden <- se.ranef(rate)
se.orden <- data.frame(se.orden$Order)

adj.medias <- data.frame(levels(m.rate2$Order),m.orden$Order$`(Intercept)`,m.orden$Order$`(Intercept)`+se.orden$X.Intercept.,m.orden$Order$`(Intercept)`-1*se.orden$X.Intercept.)
names(adj.medias) <- c("Orden","Medias ajustadas","Desvio","Desvio_1")
str(adj.medias)

library(ggplot2)
ggplot(data = adj.medias, aes(x = `Medias ajustadas`, y = `Orden`)) +
  geom_point() +
  geom_errorbar(aes(xmin = `Desvio_1`, xmax = `Desvio`), width=.1) +
  labs(x = "Media ajustada de log(Tasa metabolica basal (mLO2/hr))", y = "Orden")


#-----------------------------------------------------------------------------------------------------------------------------