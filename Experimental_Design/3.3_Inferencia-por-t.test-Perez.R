# Universidad nacional de Comahue-Centro Regional Universitario Bariloche-DiseÒo experimental
# Trabajo pr·ctico N∞ 3: Inferencia y primeras pruebas estadÌsticas

# Pablo Tom·s Campos Bˆttges, Gustavo Ezequiel Perez 



# PUNTO 3


datosunif <- read.csv("/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Datos unificados.csv")

# Armo la base de datos "datosunif"
d1 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_14-3-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d2 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_19-3-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d3 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_24-3-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d4 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_29-3-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d5 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_3-4-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d6 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_8-4-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d7 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_13-4-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d8 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_18-4-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d9 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_23-4-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")
d10 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/Bases de datos de coronavirus-20200508/corona_28-4-2020.csv", sep = ",", skip = 1, header = T, na.strings = "")


d1 <- d1[!is.na(d1$Other),]
d2 <- d2[!is.na(d2$Other),]
d3 <- d3[!is.na(d3$Other),]
d4 <- d4[!is.na(d4$Other),]
d5 <- d5[!is.na(d5$Other),]
d6 <- d6[!is.na(d6$Other),]
d7 <- d7[!is.na(d7$Other),]
d8 <- d8[!is.na(d8$Other),]
d9 <- d9[!is.na(d9$Other),]
d10 <- d10[!is.na(d10$Other),]


d1$`Cases.1` <- as.numeric(gsub(",","",d1$`Cases.1`))
d2$`Cases.1` <- as.numeric(gsub(",","",d2$`Cases.1`))
d3$`Cases.1` <- as.numeric(gsub(",","",d3$`Cases.1`))
d4$`Cases.1` <- as.numeric(gsub(",","",d4$`Cases.1`))
d5$`Cases.1` <- as.numeric(gsub(",","",d5$`Cases.1`))
d6$`Cases.1` <- as.numeric(gsub(",","",d6$`Cases.1`))
d7$`Cases.1` <- as.numeric(gsub(",","",d7$`Cases.1`))
d8$`Cases.1` <- as.numeric(gsub(",","",d8$`Cases.1`))
d9$`Cases.1` <- as.numeric(gsub(",","",d9$`Cases.1`))
d10$`Cases.1` <- as.numeric(gsub(",","",d10$`Cases.1`))

d1$`Cases.2` <- as.numeric(gsub(",","",d1$`Cases.2`))
d2$`Cases.2` <- as.numeric(gsub(",","",d2$`Cases.2`))
d3$`Cases.2` <- as.numeric(gsub(",","",d3$`Cases.2`))
d4$`Cases.2` <- as.numeric(gsub(",","",d4$`Cases.2`))
d5$`Cases.2` <- as.numeric(gsub(",","",d5$`Cases.2`))
d6$`Cases.2` <- as.numeric(gsub(",","",d6$`Cases.2`))
d7$`Cases.2` <- as.numeric(gsub(",","",d7$`Cases.2`))
d8$`Cases.2` <- as.numeric(gsub(",","",d8$`Cases.2`))
d9$`Cases.2` <- as.numeric(gsub(",","",d9$`Cases.2`))
d10$`Cases.2` <- as.numeric(gsub(",","",d10$`Cases.2`))

var <- c(names(d1))

Fecha10 = "2020/04/28"
Fecha9 = "2020/04/23"
Fecha8 = "2020/04/18"
Fecha7 = "2020/04/13"
Fecha6 = "2020/04/8"
Fecha5 = "2020/04/3"
Fecha4 = "2020/03/29"
Fecha3 = "2020/03/24"
Fecha2 = "2020/03/19"
Fecha1 = "2020/03/14"

library(dplyr)
dd1 <- d1 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha1)) %>% 
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd2 <- d2 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha2)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd3 <- d3 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha3)) %>% 
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd4 <- d4 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha4)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd5 <- d5 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha5)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd6 <- d6 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha6)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd7 <- d7 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha7)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd8 <- d8 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha8)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd9 <- d9 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha9)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

dd10 <- d10 %>%
  select(all_of(var)) %>% 
  mutate(Date = as.Date(Fecha10)) %>%
  mutate('Proporci√≥n' = `Cases.1`/`Cases.2`)

datarse <- rbind(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10)
names(datarse) <- c("Country","Total Cases","New Cases","Total Deaths","New Deaths","Total Recovered","Active Cases","Serious Cases", "Total Cases / 1 M pop", "Date", "Proporci√≥n")

index3 <- read.csv(file = "/Users/tomascbtt/Google Drive/CRUB - Unco/Materias Optativas/DisenÃÉo experimental/covid-stringency-index.csv") #Base de datos de √?ndice de rigurosidad  
names(index3) <- c("Country","Code","Date","Stringency index")

library(dplyr)

v <- c("Stringency index","Country","Date")

index4 <- index3 %>% 
  select(all_of(v)) %>%
  filter(Date == "Mar 14, 2020"|
           Date == "Mar 19, 2020"|
           Date == "Mar 24, 2020"|
           Date == "Mar 29, 2020"|
           Date == "Apr 3, 2020"|
           Date == "Apr 8, 2020"|
           Date == "Apr 13, 2020"|
           Date == "Apr 18, 2020"|
           Date == "Apr 23, 2020"|
           Date == "Apr 28, 2020") 

library(lubridate)
index4$Date <- parse_date_time(index4$Date, "b d Y", locale = "English")

datosunif <- merge(index4,datarse, by = c("Country","Date"), all.x = TRUE)
datosunif$`Total Cases` <- as.numeric(gsub(",","",datosunif$`Total Cases`))
datosunif$`Total Deaths` <- as.numeric(gsub(",","",datosunif$`Total Deaths`))
datosunif$`Total Recovered` <- as.numeric(gsub(",","",datosunif$`Total Recovered`))
datosunif$`Serious Cases` <- as.numeric(gsub(",","",datosunif$`Serious Cases`))
datosunif$`Total Cases / 1 M pop` <- as.numeric(gsub(",","",datosunif$`Total Cases / 1 M pop`))

str(datosunif)

# Grafico de cajas
library(ggplot2)
library(dplyr)

uru2 <- datosunif %>% 
  filter(Country == "Uruguay") %>% 
  select(`New.Cases`)
uru22 <- datosunif %>% 
  filter(Country == "Uruguay") %>% 
  select(`Country`)
para2 <- datosunif %>% 
  filter(Country == "Paraguay") %>% 
  select(`New.Cases`)
para22 <- datosunif %>% 
  filter(Country == "Paraguay") %>% 
  select(`Country`)

uru2[is.na(uru2),] <- 0
para2[is.na(para2),] <- 0

urupara <- rbind(data.frame(uru2,uru22),data.frame(para2,para22))
urupara %>% 
  group_by(Country) %>% 
  summarise(mean(New.Cases), n = n())

ggplot(urupara, aes(x = `Country`, y = `New.Cases`)) +
  geom_boxplot(aes(group = `Country`, colour = `Country`), alpha = 0.5) +
  scale_color_discrete(name = "Pa√?s") +
  scale_y_continuous(name = "Nuevos casos") +
  scale_x_discrete(name = "Pa√?s")

# Indice de rigurosidad en funci√≥n del pa√?s

datosunif %>%
  filter(Country == "Uruguay"|
           Country == "Paraguay") %>% 
  ggplot(aes(x = `Date`, y = `Stringency.index`)) +
  geom_line(aes(group = Country, colour = Country)) + 
  scale_y_continuous(limits = c(0,100), name = "√çndice de rigurosidad") +
  scale_x_discrete(name = "Fecha") +
  geom_point(size=2, shape=21, fill="white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name = "Pa√?s")


#cargo datos

datos<-read.csv("Datos unificados.csv", header = T)
str(datos)

#subsets
paises<-subset(datos, Country=='Paraguay'| Country=='Uruguay')
paises2<-split(paises, paises$Country)



# Test !!!!!!!!!!!!!!!

totcasos_uru<-paises2$Uruguay$New.Cases
totcasos_uru[is.na(totcasos_uru)] <- 0
media_uru<-mean(totcasos_uru)

totcasos_para<-paises2$Paraguay$New.Cases
totcasos_para[is.na(totcasos_para)] <- 0
media_para<-mean(totcasos_para)

meanDif<-media_uru-media_para # diferencia de medias.

Ho<-0
n1<-10
n2<-10
it<-c(-1.734, 1.734) # intervalo del 90% para distribuciÛn t con n1+n2-2 grados de libertad. (extraÌdo de valores de tabla)
se1<-sd(totcasos_uru)/(n1^.5) # error est·ndar para media de nuevos casos de Uruguay.
se2<-sd(totcasos_para)/(n2^.5) # error est·ndar para media de nuevos casos de Paraguay.

int=function(Ho, n1, n2, it){     
  se=(((sum((totcasos_uru-mean(totcasos_uru))^2)+sum((totcasos_para-mean(totcasos_para))^2))/(n1+n2-2))/(n1+n2))^.5 # error estandar de la diferencia.
  intervalo=meanDif+it*se # IC 95%
  t=(meanDif-Ho)/se      # estadÌstico t de student
  resultados<-c(meanDif, t, intervalo, media_uru, se1, media_para, se2)
  names(resultados)<-c('Dif media','t', '5%', '95%', 'media Uruguay', 'Error est·ndar', 'media Paraguay', 'Error est·ndar') 
  print(resultados)                             
}                                              
int(Ho, n1, n2, it)




# * Datos de variable "Ìndice de restricciÛn" extraidos de https://ourworldindata.org/grapher/covid-stringency-index?year=2020-05-07

#-----------------------------------------------------------------------------------------------------------------------------------
