# Tp N° 2 Diseño experimental 2020, Universidad Nacional del Comahue-Centro Regional Universitario Bariloche.
# NOMBRE: Ezequiel Perez ^_^


# _______________________________________

    # PARTE UNO


library(reshape)


# carga de datos


files<-list.files("datos/")# lista de archivos.csv del directorio /datos.
corona<-vector(mode='list', length = length(files)) # lista vacía
for(i in 1:length(files)){
  corona[[i]]<-read.table(paste('datos/',files[i], sep=''), header = T, sep = ',')
}
names(corona)=c('13/4','14/3','19/3','24/3','29/3','3/4','8/4') 
corona #lista con todos los csv.




# subsets de datos


paises<-vector(mode='list', length = length(corona)) # lista vacía
for (i in 1:length(corona)){
  paises[[i]]<-subset(corona[[i]], Country.=='China'| Country.=='S. Korea' | Country.=='Iran' | Country.=='Turkey')
}
names(paises)=c('13/4','14/3','19/3','24/3','29/3','3/4','8/4') 
paises #subset de paises elegidos.



cont_paises<-vector(mode='list', length = length(corona)) # lista vacía
for (i in 1:length(paises)){
  x<-paises[[i]]
  cont_paises[[i]]<-x[,c(1,2,4)]
}
names(cont_paises)=c('13/4','14/3','19/3','24/3','29/3','3/4','8/4') 
cont_paises # conteo de casos totales y muertos por país




 # reordenamiento de datos


for (i in 1:length(cont_paises)){
  cont_paises[[i]]<-cont_paises[[i]][order(cont_paises[[i]][,1]), 1:3] # ordenamiento del subset por orden alfabético
}
cont_paises 

fechas<-as.Date(c('13/4','14/3','19/3','24/3','29/3','3/4','8/4'), format = '%d/%m')

countries<-vector(mode='list', length = 4) # lista vacía
names(countries)<-c('china','iran', 's.korea', 'turquia')
countries


for (i in 1:length(countries)){
  countries[[i]]<-vector(mode = 'list', length = 7) # anido otra lista a "countries"
  pais<-countries[[i]]
  for (j in 1:length(pais)){          # filtro por pais todos los datos de las diferentes fechas
    x<-cont_paises[[j]]
    pais[[j]]<-x[i,]
  }
  pais<-do.call('rbind', pais)    # junto todos los datos por fechas en un mismo data frame "pais"
  colnames(pais)<-c('país', 'infectados', 'muertos')
  pais<-data.frame(pais,fechas)
  pais<-pais[order(pais[,4]), 1:4] # ordeno el data frame por fechas
  countries[[i]]<-pais           # lo incorporo a la lista 
}
countries
str(countries)



  # transformación de datos

countries2<-melt(countries) # uno todos los data sets de la lista en uno solo.
infectados<-as.numeric(gsub(",", "", countries2$infectados)) # cambio factores a variable numérica
muertos<-as.numeric(gsub(",", "", countries2$muertos))
pais<-as.character(countries2$país) # factores en caracteres.
countries2<-data.frame(pais, countries2, muertos, infectados)  # uno nuevas variables
countries2<-countries2[,c(1,6,8,9)] # filtro las columnas
colnames(countries2)<-c('pais', 'fecha', 'muertos', 'infectados') # renombro
countries<-split(countries2, countries2$pais)  # vuelvo a separar en dataframes por pais.


 

  # GRÁFICOS


colores<-c('gold3','darkgoldenrod4','bisque4', 'brown2')
fechas<-c('14-marzo','19-marzo','24-marzo','29-marzo','3-abril','8-abril', '13-abril') 
muertos<-c('0','1.000','2.000','3.000','4.000','5.000')
infec<-c('0', '10.000', '20.000', '30.000', '40.000', '50.000', '60.000', '70.000', '80.000', '90.000')
pts<-seq(0,90000, by=10000)

par(mar=c(5,5,3,2), mfrow=c(1,1), col='blue', family='serif', bg='cornsilk') 


#fallecidos
plot(x=countries2$fecha, y=countries2$muertos, axes = F, type ='n', ylab = '', xlab = '',
     cex.lab=1 )

axis(2, at=c(0,1000,2000,3000,4000,5000), labels=muertos[1:6], las=1, cex.axis=1, lwd = 2)
axis(1, at=countries2$fecha[1:7], labels = fechas[1:7], las=1, cex.axis=0.9, lwd=2)
title(main='Muertos COVID-19', sub='figura 1: fallecidos acumulados en China, Irán, Korea del Sur y Turquía 
      desde el 14 marzo hasta el 13 de abril.', font.main=1, font.sub=3, cex.sub=1)


for (i in 1:4){
  pais<-countries[[i]]
  lines(x=pais$fecha, y=pais$muertos, col=colores[i], type = 'b', pch=i, lwd=1.5, lty= i)
}
legend(countries2$fecha[1], 4700, legend = c('China','Iran', 'Corea del Sur', 'Turquia'), col=colores[1:4],
       lty = c(1:4), pch = c(1:4), cex = 0.8, bg='cornsilk', text.col = 'black', text.font = 2,
       title = 'paises', box.lty = 0)



#infectados
plot(x=countries2$fecha, y=countries2$infectados, axes = F, type ='n', ylab = '', xlab = '',
     cex.lab=1 )

axis(2, at=pts[1:10], labels=infec[1:10], las=1, cex.axis=1, lwd = 2)
axis(1, at=countries2$fecha[1:7], labels = fechas[1:7], las=1, cex.axis=0.9, lwd=2)
title(main='Infectados COVID-19', sub='figura 2: infectados acumulados en China, Irán, Korea del Sur y Turquía 
      desde el 14 marzo hasta el 13 de abril.', font.main=1, font.sub=3, cex.sub=1)


for (i in 1:4){
  pais<-countries[[i]]
  lines(x=pais$fecha, y=pais$infectados, col=colores[i], type = 'b', pch=i, lwd=1.5, lty= i)
}
legend(countries2$fecha[1], 75000, legend = c('China','Iran', 'Corea del Sur', 'Turquia'), col=colores[1:4],
       lty = c(1:4), pch = c(1:4), cex = 0.8, bg='cornsilk', text.col = 'black', text.font = 2,
       title = 'paises', box.lty = 0)







# _______________________________________

# PARTE DOS

  # carga de datos adicionales

library(readxl)
gdp<-read_excel("datos2/gdp_per_capita.xls", sheet = 1) # gdp per cápita de los países
str(gdp)


  # subset de datos escogidos

covid<-corona$`29/3`
elegidos<-subset(covid, X1st=='Feb-25'|X1st=='Feb-26'|X1st=='Feb-27'|X1st=='Feb-28') # paises cuyo primer infectado ocurrió dentro de una ventana de 4 días.
p_elegidos<-as.character(sort(elegidos[,1])) # vector nombre de países elegidos ordenado alfabéticamente

covid2<-corona$`13/4` # data.frame con la fecha más reciente
covid2<-subset(covid2, Country. %in% p_elegidos) # subset de paises elegidos de la base de datos más reciente

gdp_elegidos<-subset(gdp, CountryName %in% p_elegidos) # subset de paises elegídos de la base datos de gdp
str(gdp_elegidos)


  # transformación de datos

country<-as.character(covid2$Country.) # transformo este factor en vector carácter
covid2<-cbind(country, covid2[,2:12]) # cambio variable factor por caracter 
covid2<-covid2[order(covid2[,1]), 1:12] # ordeno base de datos por orden alfabético
covid2<-data.frame(p_elegidos, covid2) # uno con vector 'paises elegidos' para corroborar que estén todos los paises y en el orden correcto. 
covid2

deaths<-as.numeric(gsub(",", "", covid2$Total.1)) # variables numéricas a partir de datos de covid2
infects<-as.numeric(gsub(",", "", covid2$Total))
prop<-deaths/infects # cálculo de proporción muertos/infectados
covid2<-data.frame(covid2, deaths, infects, prop) # uno todo a covid2
covid2

gdp_paises<-as.character(gdp_elegidos$CountryName) # transformo variable CountryName en characters
gdp_elegidos<-cbind(gdp_paises, gdp_elegidos)    # la uno a la base de datos de GDPs
gdp_elegidos<-gdp_elegidos[order(gdp_elegidos[,1]), 1:4] # ordeno la base de datos por orden alfabetico de paises
gdp_elegidos<-cbind(p_elegidos, gdp_elegidos) # uno a vector con los nombres de paises elegidos para asegurarme que quedaron en el mismo orden
gdp_elegidos # se que algunos pasos parecen innecesarios pero lo tuve que hacer para lograr ordenar alfabéticamente la base -.-


gdp_per_capita<-gdp_elegidos$gdp_per_capita # extraigo gdp per cápita
covid_19<-cbind(covid2[,c(2,14,15,16)] , gdp_per_capita) # uno todo a la base con los muertos y infectados
covid_19




  # GRÁFICOS

proporcion<-c('0','0,02','0,04','0,06','0,08','0,1','0,12')
posicion<-seq(0, 0.12, by=0.02)
gdpPais<-c('0','US$50.000', 'US$100.000', 'US$150.000', 'US$200.000')
par(mar=c(6,5,3,2), mfrow=c(1,1), col='blue', family='serif', bg='cornsilk') 


plot(x=covid_19$gdp_per_capita, y=covid_19$prop, axes = F, ylab = 'proporción (Muertos / Infectados)', xlab = '', pch=16, col='red',
     cex.lab=1 )
axis(2, at=posicion[1:7], labels=proporcion[1:7], las=1, cex.axis=1, lwd = 2)
axis(1, at=c(0, 50000, 100000, 150000, 200000), labels = gdpPais[1:5], las=1, cex.axis=0.9, lwd=2)
title(main='Muertos según GDP per cápita', sub='figura 3: proporción de muertos del total de infectados a la fecha 13/4
      según índice GDP en países con primer infectado entre el 25/02 y el 28/02',
      font.main=1, font.sub=3, cex.sub=1)




# datos de GDP per cápita obtenidos de https://data.worldbank.org/indicator/NY.GDP.PCAP.CD

#---------------------------------------------------------------------------------------------------------------------------------