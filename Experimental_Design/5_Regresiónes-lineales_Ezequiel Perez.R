# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-Diseño experimental.
# Trabajo práctico N° 5: Regresión lineal.
# Ezequiel Perez ^-^





#### ingreso y manipulación de datos---------------


#datos de la cátedra
datos<-read.csv('datos/corona_28-4-2020.csv', header = T)
nombres<-c('pais','casosTot', 'casosnuevos', 'muertosTot', 'nuevosMuertos', 'recuTot', 'activos','serios', 'casos1M', 'muestos1M', 'totTest', 'Test1M')
datos<-datos[c(-1), 1:12]
names(datos)<-nombres
datos<-datos[1:212, 1:12]
datos<-datos[order(datos[,1]), 1:12]
for(i in 2:12){
  datos[,i]<-as.numeric(gsub(',','', datos[,i]))
}
write.table(datos, "E:/wd/goodData_28-4-2020.csv", sep = ',', na = "NA", dec = '.', col.names = T, row.names = T)


#Otros datos
datos2<-read.csv('Datos/others/owid-covid-data.csv', header = T, sep = ",")
str(datos2)
View(datos2)
joya<-read.csv('Datos/others/OxCGRT_latest.csv', header = TRUE, sep = ',')
str(joya)
View(joya)

#subset de datos:
library('dplyr')

tests<-subset(joya, Date==20200415) # fecha 2020-04-15
paises_tests<-subset(tests, H2_Testing.policy==3 | H2_Testing.policy==2) # políticas de testeo mas altas.
paises_tests<-select(paises_tests, CountryName, H2_Testing.policy) # filtro columnas
names(paises_tests)<-c('location', 'testing_policy') # renombro columnas


data1<-filter(datos2, date=='2020-04-15'| date=='2020-04-16'| date=='2020-04-17'| date=='2020-04-18' | date=='2020-04-19')
data2<-filter(datos2, date=='2020-05-05'| date=='2020-05-06'| date=='2020-05-07'| date=='2020-05-08' | date=='2020-05-09')

data1<-slice(data1, 1:1030)
data2<-slice(data2, 1:1045)

View(data1)
View(data2)

data1.2<-select(data1, iso_code, location, date, new_tests_per_thousand, new_tests_smoothed_per_thousand, total_cases, stringency_index)
data2.2<-select(data1, iso_code, location, date, total_cases, total_cases_per_million, new_cases, new_cases_per_million, stringency_index)

data1.3<-aggregate(cbind(data1.2$new_tests_per_thousand, data1.2$new_tests_smoothed_per_thousand, data1.2$total_cases, data1.2$stringency_index)~data1.2$location, data1.2, mean, na.rm = TRUE)
names(data1.3)<-c('location', 'new_tests_per_thousand_1', 'new_tests_smoothed_per_thousand_1', 'total_cases_1', 'stringency_index_1')
data2.3<-aggregate(cbind(data2.2$total_cases, data2.2$total_cases_per_million, data2.2$new_cases, data2.2$new_cases_per_million, data2.2$stringency_index)~data2.2$location, data2.2, mean, na.rm=T)
names(data2.3)<-c('location', 'total_cases_2', 'total_cases_per_million_2', 'new_cases_2', 'new_cases_per_million_2', 'stringency_index_2')

full_datos<-left_join(data1.3, data2.3, by="location")
full_data<-left_join(paises_tests, full_datos, by="location")
full_data<-na.exclude(full_data)


#### Modelo lineal y ploteos---------------


# variables de interes:

NC_normalizados<-(full_data$stringency_index_1[c(-3, -13)]*full_data$new_cases_per_million_2[c(-3, -13)])/full_data$total_cases_1[c(-3, -13)]
daily_tests<-full_data$new_tests_per_thousand_1[c(-3, -13)]

    # modelo y analisis estadisticos:

modelo<-lm(NC_normalizados~daily_tests)
summary(modelo)
anova<-aov(NC_normalizados~daily_tests)
summary(anova)
confint(modelo, level=0.95)



    # ploteo modelo:
plot(daily_tests, NC_normalizados, pch = 18, col = 19, ylab = 'Media de nuevos casos entre 05-05 y 09-05', xlab = 'Media de tests diarios entre 15-04 y 19-04', main = 'Efecto de los tests sobre nuevos casos', ylim = c(-1, 3))
curve(modelo$coefficients[1] + modelo$coefficients[2]*x, from = min(daily_tests), to = max(daily_tests), add = T, col = 'black', lwd=2)


x<-seq(min(daily_tests), max(daily_tests), by = 0.03)
new.daily_tests<-data.frame(daily_tests=x)

#Intervalo de prediccion de la media
conf.95<-predict(modelo, newdata = new.daily_tests, interval = 'confidence', level = 0.95) 

lines(x, conf.95[,2], lty = 2, col = 'orange', lwd=2)
lines(x, conf.95[,3], lty = 2, col = 'orange', lwd=2) 


#Intervalo de prediccion de datos
pred.95 <- predict(modelo, newdata = new.daily_tests, interval="predict", level=0.95)

lines(x, pred.95[,2], lty=3, col = 'red', lwd=2)
lines(x, pred.95[,3], lty=3, col = 'red', lwd=2)


# graficos diagnosticos
conf<-par(mfrow=c(2,2))
plot(modelo)
par(conf)
par(mfrow=c(1,1))

# ----------------------------------------------------------------------------------------------------