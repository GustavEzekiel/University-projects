 # Centro Regional Universitario Bariloche - Diseño experimental 
# Ezequiel Perez ^-^

# Trabajo practico numero 8 - MODELOS GENERALIZADOS-  





#---- EXPLORANDO DATOS ----

library(readxl)
beetles<-read_excel('beetles.xlsx')
View(beetles)
str(beetles)





# ---- MANIPULACION DE DATOS ----


library(dplyr)
beetles$parental_state<-as.factor(beetles$parental_state) # transformo en factores algunas columnas.
beetles$treatment<-as.factor(beetles$treatment)
beetles$mouse_size<-as.factor(beetles$mouse_size)
beetles$number_deaths<-15-beetles$number_larvae # armo una nueva columna con la cantidad de larvas muertas.
str(beetles)

beetles2<-filter(beetles, mouse_size=="5")
str(beetles2)
length(beetles2$ID)




# ---- EXPLORACIÓN DE LOS DATOS ----


library(ggplot2)
attach(beetles)

tabla<-table(treatment, parental_state, mouse_size) # modelo desbalanceado 
tabla

  # distribución de la variable respuesta.
hist(number_larvae) 
hist(number_deaths)
hist(number_larvae/(number_deaths+number_larvae)) # distribución de la proporsión larvas vivas/larvas totales.
hist(number_deaths/(number_deaths+number_larvae)) # distribución de la proporsión larvas muertas/larvas totales.

ggplot(beetles,aes(x=number_larvae))+geom_histogram(binwidth = 0.9)+facet_grid(parental_state~mouse_size+treatment) 


detach(beetles)
attach(beetles2) # base que solo contiene datos para mouse_size=5

tabla2<-table(treatment, parental_state) 
tabla2 # modelo desbalanceado 

hist(number_larvae) 
hist(number_deaths)
hist(number_larvae/(number_deaths+number_larvae)) # distribución de la proporsión larvas vivas/larvas totales.
hist(number_deaths/(number_deaths+number_larvae)) # distribución de la proporsión larvas muertas/larvas totales.

ggplot(beetles,aes(x=number_larvae))+geom_histogram(binwidth = 0.9)+facet_grid(parental_state~treatment) 

tapply(number_larvae, parental_state, mean) # medias aritméticas para los niveles del factor parental_state
tapply(number_larvae, parental_state, var)  # varianza aritmética para los niveles del factor parental_state.

tapply(number_larvae, treatment, mean) # medias aritméticas para los niveles del factor treatment
tapply(number_larvae, treatment, var)  # varianza aritmética para los niveles del factor treatment


#---- MODELOS ----

options(contrasts=c("contr.sum","contr.poly"))  

library(car)    # para anova
library(lsmeans)# para lsmeans
library(MASS)   # para glm.nb
library(lmerTest) # para modelos mixtos.


                  # analisis incluyendo todos los datos.
detach(beetles2)
attach(beetles)

  # modelo generalizado con distribucion de VR poisson
m1<-glm(number_larvae~parental_state*treatment+mouse_size+average_larval_weight, family = poisson)
1-pchisq(m1$deviance, m1$df.residual)  # ajusta muy bien a los datos.
summary(m1)
Anova(m1, type = 3)

  # modelo generalizado con distribucion de VR binomial
m2<-glm(cbind(number_larvae, number_deaths)~parental_state+treatment*mouse_size+average_larval_weight, family = 'binomial')
1-pchisq(m2$deviance, m2$df.residual)  # no ajusta muy bien a los datos.
summary(m2)
Anova(m2, type = 3)

AIC(m1,m2) # me da contradictorio a lo esperado (ajusta mejor el modelo con dist. binomial, cuando el mejor ajuste por el
            # test anterior me dice que ajusto mejor el modelo poisson)


  # medias para modelo 1.
means1<-lsmeans(m1, "parental_state") # medias ajustadas por tipo de cuidado parental.
means1
means2<-lsmeans(m1, "treatment") # medias ajustadas por tratamiento
means2
means3<-lsmeans(m1, "mouse_size") # medias ajustadas por tamanio de raton.
means3
means4<-lsmeans(m1, c("parental_state", "treatment", "mouse_size")) # medias para todas las combinaciones de niveles.
means4



              # analisis con solo ratones de tamaño 5.
detach(beetles)
attach(beetles2)

# modelo generalizado con distribucion de los VR poisson

m3<-glm(number_larvae~parental_state+treatment*average_larval_weight, family = poisson) 
1-pchisq(m3$deviance, m3$df.residual)  # ajusta muy bien a los datos.
summary(m3)
Anova(m3, type = 3)

# modelo generalizado con distribucion de VR binomial
m3.5<-glm(cbind(number_larvae, number_deaths)~parental_state*treatment*average_larval_weight, family = 'binomial') 
1-pchisq(m3.5$deviance, m3.5$df.residual)  # ajusta muy bien a los datos.
summary(m3.5)
Anova(m3.5, type = 3) # termino interacción no significativo


# modelo generalizado con distribucion de VR binomial
m4<-glm(cbind(number_larvae, number_deaths)~parental_state+treatment*average_larval_weight, family = 'binomial') # modelo solo incluyendo la interacción significativa
1-pchisq(m4$deviance, m4$df.residual)  # ajusta muy bien a los datos.
summary(m4)
Anova(m4, type = 3)

AIC(m3.5, m4) # me quedo con modelo 4



# medias ajustadas (no transformadas) para el modelo 4
means1<-lsmeans(m4, "parental_state") # medias ajustadas por tipo de cuidado parental.
means1
means2<-lsmeans(m4, "treatment") # medias ajustadas por tratamiento
means2
means4<-lsmeans(m4, c("parental_state", "treatment")) # medias para todas las combinaciones de niveles.
means4

# medias transformadas 
medias<-summary(means4)
class(medias) # checkeando que el objeto medias sea un dataframe.

medias.des<-transform(medias, 
          antilogit.mean=exp(lsmean)/(1+exp(lsmean)), 
          antilogit.SE=exp(SE)/(1+exp(SE)),
          antilogit.LCL=exp(asymp.LCL)/(1+exp(asymp.LCL)),
          antilogit.UCL=exp(asymp.UCL)/(1+exp(asymp.UCL)))
medias.des


#---- GRAFICOS ----

library(visreg)

  # medias ajustadas +/- 1 SE
visreg(m4, 'parental_state', by='treatment', scale="response")# Por alguna razon no funciona.

stripchart(number_larvae/(number_larvae+number_deaths)~parental_state + treatment, vertical = TRUE, 
           method = "jitter", pch=1, col = c('firebrick', 'firebrick1', 'darkorchid', 'darkorchid1'),
           ylab = 'proporsión de larvas vivas', main='medias ajustadas +/- 1 SE'
           )
m<-medias.des$antilogit.mean  # extraigo medias destransfomadas
up<-medias$lsmean+medias$SE   # media mas un SE 
down<-medias$lsmean-medias$SE # media menos un SE

up<-exp(up)/(1+exp(up))  # destransformo media mas un SE 
down<-exp(down)/(1+exp(down)) # destransformo media menos un SE 

#agrego lineas al stripchart
segments(1:4 - 0.25, m, 1:4 + 0.25, m, lwd = 2) # medias ajustadas
segments(1:4 - 0.2, up, 1:4 + 0.2, up, lty = 2) # media menos 1 SE
segments(1:4 - 0.2, down, 1:4 + 0.2, down, lty = 2) # media mas 1 SE




detach(beetles2)

