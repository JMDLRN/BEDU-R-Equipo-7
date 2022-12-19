# Un centro de salud nutricional está interesado en analizar estadísticamente y 
# probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
# en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene 
# recursos financieros extras al ingreso y en si presenta o no inseguridad alimentaria. 
# Además, está interesado en un modelo que le permita identificar los determinantes 
# socioeconómicos de la inseguridad alimentaria.
# 
# La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) 
# levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las 
# personas afirman que los hogares con menor nivel socioeconómico tienden a gastar 
# más en productos no saludables que las personas con mayores niveles socioeconómicos 
# y que esto, entre otros determinantes, lleva a que un hogar presente cierta 
# inseguridad alimentaria.
# 
# La base de datos contiene las siguientes variables:

# nse5f (Nivel socioeconómico del hogar): 
#   1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"

# area (Zona geográfica): 
#   0 "Zona urbana", 1 "Zona rural"

# numpeho (Número de persona en el hogar)

# refin (Recursos financieros distintos al ingreso laboral): 
#   0 "no", 1 "sí"

# edadjef (Edad del jefe/a de familia)

# sexoje (Sexo del jefe/a de familia): 
#   0 "Hombre", 1 "Mujer"

# añosedu (Años de educación del jefe de familia)

# ln_als (Logaritmo natural del gasto en alimentos saludables)

# ln_alns (Logaritmo natural del gasto en alimentos no saludables)

# IA (Inseguridad alimentaria en el hogar): 
#   0 "No presenta IA", 1 "Presenta IA"

"1 Plantea el problema del caso"
#Cargamos librerías
library(dplyr)
library(ggplot2)


#Cargamos dataset csv
df.ini<-read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

#Visualización de los datos, tipos, clases del dataframe
str(df.ini)
head(df.ini)
summary(df.ini) #Observamos NA's en edadjef, sexojef, ln_als y ln_alns
sum(complete.cases(df.ini)) #Confirmamos que solo hay 20280 filas completas

#Limpieza de datos
#Dado que el planteamiento no considera la edad y sexo del jefe de familia dentro del análisis se excluyen del dataframe
df <- select(df.ini, nse5f, area, numpeho, refin, añosedu, IA, ln_als, ln_alns) 
summary(df) #Observamos que hay 787 NA en alimentos saludables y 17504 NA en alimentos no saludables

#Dado que las variables area, refin y IA son categóricas, las cambiamos al tipo indicado
df$area <- factor(df$area, labels = c("Urbana", "Rural"))
df$refin <- factor(df$refin, labels = c("No", "Si"))
df$IA <- factor(df$IA, labels = c("No", "Si"))
df$numpeho<-as.numeric(df$numpeho)
df$nse5f <- as.numeric(df$nse5f)

# #Excluimos los NA de alimentos saludables y no saludables
df.clean <- df[complete.cases(df),]

"2 Realiza un análisis descriptivo de la información"
#Análisis descriptivo del dataframe limpio
dim(df.clean) #El dataframe tiene 23201 obs. en 8 variables
#nse5f, numpeho, añosedu, ln_als y ln_alns son numerics
#area, refin, IA son Factor

str(df.clean) 
head(df.clean)
summary(df.clean) 
boxplot(df.clean)

#Visualización de los datos Globales
par(mfrow = c(2,4))
hist(df$nse5f, 
     breaks = c(0:5), 
     col = "red", 
     main = "Nivel Socioeconómico", 
     labels = TRUE)
hist(as.integer(df$area), 
     breaks = 2, 
     col = "orange", 
     main = "Tipo de Área", 
     labels = TRUE)
hist(df$numpeho, 
     col = "yellow", 
     main = "Núm. Personas en Hogar", 
     labels = TRUE)
hist(as.integer(df$refin), 
     breaks = 2, 
     col = "green", 
     main = "Rec. Financieros distintos", 
     labels = TRUE)
hist(df$añosedu, 
     col = "cyan", 
     main = "Años de educación", 
     labels = TRUE)
hist(as.integer(df$IA), 
     breaks = 2, 
     col = "blue", 
     main = "Inseguridad Alimentaria", 
     labels = TRUE)
hist(df$ln_als, 
     col = "violet", 
     main = "LN Gasto en Alim. Saludable", 
     labels = TRUE)
hist(df$ln_alns, 
     col = "purple", 
     main = "LN Gasto en Alim. No Saludable", 
     labels = TRUE)

dev.off()

#Del dataframe, podemos extraer también información visual de la Inseguridad
#Alimentaria como función de todas las variables, como se muestra a continuación:

#Inseguridad alimentaria (IA) separada por Nivel socioeconómico del hogar
ggplot(data=df.clean, aes(x=as.factor(nse5f), color=IA)) +
  geom_bar(stat="count",fill="white")+ 
  labs(title = "IA por Nivel socioeconómico del hogar (NSE5F)", 
       x = "Nivel socioeconómico",
       y = "Total") + 
  theme_classic()

#Aquí se aprecia que la Inseguriad alimenticia en efecto es función del nivel socioeconómico.

#Inseguridad alimentaria separada por Zona Geográfica
ggplot(data=df.clean, aes(x=area, color=IA)) +
  geom_bar(stat="count",fill="white")+ 
  labs(title = "IA por Zona Geográfica", 
       x = "Zona Geográfica",
       y = "Total") + 
  theme_classic()

#Inseguridad alimentaria separada por recursos financieros distintos al ingreso laboral
ggplot(data=df.clean, aes(x=refin, color=IA)) +
  geom_bar(stat="count",fill="white")+ 
  labs(title = "IA por Recursos financieros distintos al ingreso laboral", 
       x = "Recursos financieros",
       y = "Total") + 
  theme_classic()

#Boxplot Número de personas en el hogar por IA

ggplot(df.clean, aes(x=numpeho, y=IA, color=as.factor(nse5f))) +
  geom_boxplot()+ 
  labs(title = "Numero de personas en el hogar por IA y NSE5F",
       x = "Numero de personas",
       y = "IA",
       color="Nivel Socioeconómico")   + 
  theme_classic()

#Histograma Logaritmo Alimentos Saludables y No Saludables por IA
#Criterio para Número de Bins
ceiling(1+3.3*log10(length(df.clean$ln_alns)))


ggplot(df.clean, aes(x=ln_als, color=IA)) +
  geom_histogram(bins = 16, fill="White") + 
  labs(title = "Histograma LNALS por IA", 
       x = "Logaritmo natural alimentos saludables (LNALS)",
       y = "Frequencia") + 
  theme_classic()

ggplot(df.clean, aes(x=ln_alns, color=IA)) +
  geom_histogram(bins = 16, fill="White") + 
  labs(title = "Histograma LNALNS por IA", 
       x = "Logaritmo natural alimentos no saludables (LNALNS)",
       y = "Frequencia") + 
  theme_classic()

#Scatterplot LNALS vs LNALNS por IA y NSE
ggplot(df.clean, aes(x = ln_als, y=ln_alns, color =IA, shape=as.factor(nse5f))) + 
  geom_point( alpha=0.5) +
  labs(title = "Encuesta nacional de salud y nutrición 2012", 
       x = "LNALS",
       y = "LNALNS")

#gasto saludable-inseguridad alimenticia


Ia.Ga<-select(df, IA, ln_als, ln_alns) #seleccionamos solo las variables interesadas
Ia.Ga.clean<-na.omit(Ia.Ga  )#limpiamos los datos- quedan 23201 obs

Ia.No.Ga<- filter(Ia.Ga.clean, IA=="No")#gasto - Seguridad alimenticia
Ia.Si.Ga<- filter(Ia.Ga.clean, IA=="Si")#gasto - Inseguridad alimenticia

par(mfrow = c(2, 2)) #El siguiente bloque de histogramas se pueden colocar en el orden especificado

Seguridad.ga.sal <-hist(Ia.No.Ga$ln_als,  breaks="Sturges", main="Gasto saludable con seguridad")

Seguridad.gas.nsal<-hist(Ia.No.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable con seguridad")

NSeguridad.ga.sal <-hist(Ia.Si.Ga$ln_als,  breaks="Sturges", main="Gasto saludable con inseguridad")

NSeguridad.gas.nsal<-hist(Ia.Si.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable con inseguridad")

par(mfrow = c(1, 1))

#Histogramas con media indicada aun tiene errores pero los estoy revisando

ggplot(Ia.No.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(ln_als)),color="blue",linetype="dashed" ,size=1)


ggplot(Ia.No.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(ln_alns)),color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(ln_als)), color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(ln_alns)), color="blue",linetype="dashed" ,size=1)

#Histogramas de gasto apareados seguridad e inseguridad alimenticia 

par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ia.No.Ga$ln_als, main="" , xlim=c(0,8), ylab="Seguridad", xlab="", ylim=c(0,2500) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ia.Si.Ga$ln_als, main="" , xlim=c(0,8), ylab="Inseguridad", xlab="Gasto en comida saludable", ylim=c(5500,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ia.No.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Seguridad", xlab="", ylim=c(0,1300) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ia.Si.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Inseguridad", xlab="Gasto en comida no saludable", ylim=c(3000,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


#Histogramas suavizados con media indicada y normal aproximada, aun tiene errores pero los estoy revisando

ggplot(Ia.No.Ga ,aes(x=ln_als))+
  geom_histogram(aes( y=after_stat(density)),  colour="black", fill="white")+  
  geom_density(alpha=.2, fill="#FF6666")+ 
  geom_vline(aes(xintercept=mean(ln_als)),color="blue",linetype="dashed" ,size=1)


ggplot(Ia.No.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white")+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_alns)),color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white")+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_als)), color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white")+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_alns)), color="blue",linetype="dashed" ,size=1)



## Momentos de las distribuciones 

library(DescTools)
library(moments)

#LNALS para poblacion con IA
#Medidas de tendencia central
mean(df.clean[df.clean$IA=="Si", "ln_als"])
mean(df.clean[df.clean$IA=="Si", "ln_als"], trim = 0.20)
median(df.clean[df.clean$IA=="Si", "ln_als"])
Mode(df.clean[df.clean$IA=="Si", "ln_als"])
#Desviación estándar
sd(df.clean[df.clean$IA=="Si", "ln_als"])
#Sesgo
skewness(df.clean[df.clean$IA=="Si", "ln_als"])
#Esta distribución posee sesgo a la izquierda
#Curtosis
kurtosis(df.clean[df.clean$IA=="Si", "ln_als"])
#Esta distribución es leptocúrtica.

#El sesgo se puede eliminar truncando las colas.
#Para esto es útil el paquete dvmisc, que puede istalarse conel siguiente comando:
#install.packages("dvmisc")
library(dvmisc)
IASiLNALS<-trim(df.clean[df.clean$IA=="Si", "ln_als"],p=0.2,tails="both")
mean(IASiLNALS)
skewness(IASiLNALS)

#LNALS para poblacion sin IA
#Medidas de tendencia central
mean(df.clean[df.clean$IA=="No", "ln_als"])
mean(df.clean[df.clean$IA=="No", "ln_als"], trim = 0.20)
median(df.clean[df.clean$IA=="No", "ln_als"])
Mode(df.clean[df.clean$IA=="No", "ln_als"])
#Desviación estándar
sd(df.clean[df.clean$IA=="No", "ln_als"])
#Sesgo
skewness(df.clean[df.clean$IA=="No", "ln_als"])
#Esta distribución posee sesgo a la izquierda
#Curtosis
kurtosis(df.clean[df.clean$IA=="No", "ln_als"])
#Esta distribución es muy leptocúrtica.
#Tambien en este caso podemos eliminar el sesgo truncando las colas
IANoLNALS<-trim(df.clean[df.clean$IA=="No", "ln_als"],p=0.2,tails="both")
mean(IANoLNALS)
skewness(IANoLNALS)



#LNALNS para poblacion con IA
#Medidas de tendencia central
mean(df.clean[df.clean$IA=="Si", "ln_alns"])
mean(df.clean[df.clean$IA=="Si", "ln_alns"], trim = 0.20)
median(df.clean[df.clean$IA=="Si", "ln_alns"])
Mode(df.clean[df.clean$IA=="Si", "ln_alns"])
#Desviación estándar
sd(df.clean[df.clean$IA=="Si", "ln_alns"])
#Sesgo
skewness(df.clean[df.clean$IA=="Si", "ln_alns"])
#Esta distribución posee un ligero sesgo a la derecha
#Curtosis
kurtosis(df.clean[df.clean$IA=="Si", "ln_alns"])
#Esta distribución es ligeramente platicurtica.



#LNALNS para poblacion sin IA
#Medidas de tendencia central
mean(df.clean[df.clean$IA=="No", "ln_alns"])
mean(df.clean[df.clean$IA=="No", "ln_alns"], trim = 0.20)
median(df.clean[df.clean$IA=="No", "ln_alns"])
Mode(df.clean[df.clean$IA=="No", "ln_alns"])
#Desviación estándar
sd(df.clean[df.clean$IA=="No", "ln_alns"])
#Sesgo
skewness(df.clean[df.clean$IA=="No", "ln_alns"])
#Esta distribución posee un ligero sesgo a la derecha
#Curtosis
kurtosis(df.clean[df.clean$IA=="No", "ln_alns"])
#Esta distribución es ligeramente platicúrtica. Puede aproximarse con una distribución normal


"3 Calcula probabilidades que nos permitan entender el problema en México"

##Probabilidad IA como función de Nivel socioeconómico del hogar


#Probabilidad de tener IA en el Nivel Bajo
length(df.clean[df.clean$IA=="Si" & df.clean$nse5f==1, "nse5f"])/length(df.clean[df.clean$nse5f==1, "nse5f"])
#Probabilidad de tener IA en el Nivel Medio Bajo
length(df.clean[df.clean$IA=="Si" & df.clean$nse5f==2, "nse5f"])/length(df.clean[df.clean$nse5f==2, "nse5f"])
#Probabilidad de tener IA en el Nivel Medio
length(df.clean[df.clean$IA=="Si" & df.clean$nse5f==3, "nse5f"])/length(df.clean[df.clean$nse5f==3, "nse5f"])
#Probabilidad de tener IA en el Nivel Medio Alto
length(df.clean[df.clean$IA=="Si" & df.clean$nse5f==4, "nse5f"])/length(df.clean[df.clean$nse5f==4, "nse5f"])
#Probabilidad de tener IA en el Nivel Alto
length(df.clean[df.clean$IA=="Si" & df.clean$nse5f==5, "nse5f"])/length(df.clean[df.clean$nse5f==5, "nse5f"])

##Probabilidad IA como función de la Zona Geográfica

#Probabilidad de tener IA en el ámbito urbano
length(df.clean[df.clean$IA=="Si" & df.clean$area=="Urbana", "area"])/length(df.clean[df.clean$area=="Urbana", "area"])
#Probabilidad de tener IA en el ámbito rural
length(df.clean[df.clean$IA=="Si" & df.clean$area=="Rural", "area"])/length(df.clean[df.clean$area=="Rural", "area"])


##Probabilidad IA como función de los recursos financieros

#Probabilidad de tener IA con recursos financieros adicionales
length(df.clean[df.clean$IA=="Si" & df.clean$refin=="Si", "refin"])/length(df.clean[df.clean$refin=="Si", "refin"])
#Probabilidad de tener IA sin recursos financieros adicionales
length(df.clean[df.clean$IA=="Si" & df.clean$refin=="No", "refin"])/length(df.clean[df.clean$refin=="No", "refin"])




"4 Plantea hipótesis estadísticas y concluye sobre ellas para entender el
problema en México"

#Los resultados anteriores parecen indicar que la población sin IA gasta más en 
#promedio que la que presenta IA en Alimentos Saludables.

#Planteamiento de hipótesis:
#Ho: prom_IANo_als <= prom_IASi_als 
#Ha: prom_IANo_als > prom_IASi_als"

#En primer lugar hacermos la comparación de varianzas de ambas distribuciones,
#Donde en ambos casos hemos truncado las colas para aleviar el sesgo
var.test(trim(df.clean[df.clean$IA=="No", "ln_als"],p=0.2,tails="both"), 
         trim(df.clean[df.clean$IA=="Si", "ln_als"],p=0.2,tails="both"),
         ratio = 1, alternative = 'two.sided')
#El resultado indica que a 99% de confianza las varianzas son diferentes

t.test(x = trim(df.clean[df.clean$IA=="No", "ln_als"],p=0.2,tails="both"), 
       y =trim(df.clean[df.clean$IA=="Si", "ln_als"],p=0.2,tails="both"),
       alternative = "greater",
       mu = 0, var.equal = FALSE)
#El resultado indica que a 99% de confianza se desecha la hipótesis nula, de modo que
#efectivamente la población sin IA gasta más en 
#promedio en Alimentos Saludables que la población con IA 




"5 Estima un modelo de regresión, lineal o logístico, para identificar los
determinantes de la inseguridad alimentaria en México"

#Modelo de Regresión logística para identificar las determinantes de la Inseguridad
#alimentaria en México.

attach(df.clean)
logistic.1 <- glm(IA ~ nse5f + area + numpeho + refin  + ln_als + ln_alns 
                  + nse5f:area + nse5f:numpeho + nse5f:refin 
                  + area:numpeho + area:refin + numpeho:refin, family = binomial)

summary(logistic.1)


#Del análisis anterior, se puede concluir que a un nivel de confianza de 99%, los
#Coeficientes de las variables area, refin y de los términos de interacción
#nse5f:numpeho, nse5f:refin, area:numpeho, area:refin y numpeho:refin
#son consistentes con cero, de modo que podemos remover del modelo dichos términos

logistic.2 <- update(logistic.1, ~. - area - refin - nse5f:numpeho 
                     - nse5f:refin - area:numpeho - area:refin - numpeho:refin)
summary(logistic.2)

#En este modelo simplificado, también a nivel de confianza de 99% el coeficiente 
#de la interacción remanente es consistente con cero, de modo que podemos simplificar 
#una segunda ocasión para obtener el modelo definitivo

logistic.3 <- update(logistic.2, ~. - nse5f:area)
summary(logistic.3)

pseudo_r2.3 <- (logistic.3$null.deviance - logistic.3$deviance)/logistic.3$null.deviance
pseudo_r2.3

