# Un centro de salud nutricional está interesado en analizar estadísticamente y 
# probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
# en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene 
# recursos financieros extras al ingreso y en si presenta o no inseguridad alimentaria. 
# Además, está interesado en un modelo que le permita identificar los determinantes 
# socioeconómicos de la inseguridad alimentaria.
# 
# La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) 
# levantada por el Instituto Nacional de Salud Pública en México. 


#La mayoría de las 
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
df <- select(df.ini, nse5f, area, numpeho, refin, añosedu, IA, ln_als, ln_alns) 

df$area <- factor(df$area, labels = c("Urbana", "Rural"))
df$refin <- factor(df$refin, labels = c("No", "Si"))
df$IA <- factor(df$IA, labels = c("No", "Si"))
df$numpeho<-as.numeric(df$numpeho)
df$nse5f <- as.numeric(df$nse5f)




#inicio- Estudio de gasto en comida no saludable en funcion de diferentes factores


#gasto-inseguridad alimenticia

Ia.Ga<-select(df, IA, ln_als, ln_alns) #seleccionamos solo las variables interesadas
Ia.Ga.clean<-na.omit(Ia.Ga  )#limpiamos los datos- quedan 23201 obs

Ia.No.Ga<- filter(Ia.Ga.clean, IA=="No")#gasto - Seguridad alimenticia 6815
summary( Ia.No.Ga  )
Ia.Si.Ga<- filter(Ia.Ga.clean, IA=="Si")#gasto - Inseguridad alimenticia 16386
summary( Ia.Si.Ga  )


#Histogramas de gasto apareados seguridad e inseguridad alimenticia 

par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ia.No.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Seguridad", xlab="", ylim=c(0,1300) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ia.Si.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Inseguridad", xlab="Gasto en comida no saludable", ylim=c(3000,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


"Planteamiento de hipótesis:

mu_1: El promedio del gasto en comida no saludable con seguridad alimentaria
mu_2: El promedio del gasto en comida no saludable con inseguridad alimentaria


Ho: mu_1 >=  mu_2 
Ha:  mu_1 <  mu_2"

var.test( x = Ia.No.Ga$ln_alns, 
          y = Ia.Si.Ga$ln_alns, 
          ratio = 1, alternative = "two.sided")


t.test(x = Ia.No.Ga$ln_alns, 
       y = Ia.Si.Ga$ln_alns, 
       alternative = "greater",
       mu = 0, var.equal = FALSE)

"Existe evidencia estadistica para rechazar H0 en los niveles de confianza estandar
es decir: "

"El promedio del gasto en comida no saludable con seguridad alimentaria
es menor al promedio del gasto en comida no saludable con inseguridad alimentaria
como se esperaba"


#Gasto-Recursos extras

Ex.Ga<-select(df, refin, ln_als, ln_alns) #seleccionamos solo las variables interesadas
Ex.Ga.clean<-na.omit(Ex.Ga  )#limpiamos los datos- quedan 23201 obs

Ex.No.Ga<- filter(Ex.Ga.clean, refin=="No")#gasto - No extras 18787
Ex.Si.Ga<- filter(Ex.Ga.clean, refin=="Si")#gasto - Extras 4414


#Histogramas de gasto apareados recursos extras e inseguridad alimenticia 


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ex.No.Ga$ln_alns, main="" , xlim=c(0,8), ylab="No extras", xlab="", ylim=c(0,3500) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ex.Si.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Extras", xlab="Gasto en comida no saludable", ylim=c(800,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


"Planteamiento de hipótesis:

mu_1: El promedio del gasto en comida no saludable sin recursos extras
mu_2: El promedio del gasto en comida no saludable con recursos extras


Ho: mu_1 >= mu_2 
Ha:  mu_1  < mu_2"

var.test( x = Ex.No.Ga$ln_alns, 
          y = Ex.Si.Ga$ln_alns, 
          ratio = 1, alternative = "two.sided")

#NIVEL DE CONFIANZA MENOR DEL 95%

t.test(x = Ex.No.Ga$ln_alns, 
       y = Ex.Si.Ga$ln_alns, 
       alternative = "less",
       mu = 0, var.equal = TRUE)

"Existe evidencia estadistica para no rechazar H0 es decir: "

" El promedio del gasto en comida no saludable sin recursos extras es mayor 
o igual al promedio del gasto en comida no saludable con recursos extras"


#Gasto-Nivel socioeconomico 


N.Ga<-select(df, nse5f, ln_als, ln_alns) #seleccionamos solo las variables interesadas
N.Ga.clean<-na.omit(N.Ga  )#limpiamos los datos- quedan 23201 obs

N1.Ga<- filter(N.Ga.clean, nse5f==1)#gasto - N1 3935
N2.Ga<- filter(N.Ga.clean, nse5f==2)#gasto - N2 4419
N3.Ga<- filter(N.Ga.clean, nse5f==3)#gasto - N3 4748
N4.Ga<- filter(N.Ga.clean, nse5f==4)#gasto - N4 5007
N5.Ga<- filter(N.Ga.clean, nse5f==5)#gasto - N5 5092

#Histogramas de gasto apareados nivel socioeconomico 1 y 2 e inseguridad alimenticia 


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(N1.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 1", xlab="", ylim=c(0,900) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(N2.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 2", xlab="Gasto en comida no saludable", ylim=c(900,0) , las=1 , col="tomato3"  ,  breaks="Sturges")

"Planteamiento de hipótesis 1 y 2:

mu_1: El promedio del gasto en comida no saludable nivel 1
mu_2: El promedio del gasto en comida no saludable nivel 2


Ho: mu_1 <= mu_2 
Ha:  mu_1  > mu_2"

var.test( x = N1.Ga$ln_alns, 
          y = N2.Ga$ln_alns, 
          ratio = 1, alternative = "two.sided")


t.test(x = N1.Ga$ln_alns, 
       y = N2.Ga$ln_alns, 
       alternative = "greater",
       mu = 0, var.equal = TRUE)

"No rechazo H0"


#Histogramas de gasto apareados nivel socioeconomico 1 y 3 e inseguridad alimenticia 


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(N1.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 1", xlab="", ylim=c(0,900) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(N3.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 3", xlab="Gasto en comida no saludable", ylim=c(900,0) , las=1 , col="tomato3"  ,  breaks="Sturges")

"Planteamiento de hipótesis 1 y 3:

mu_1: El promedio del gasto en comida no saludable nivel 1
mu_2: El promedio del gasto en comida no saludable nivel 3


Ho: mu_1 <= mu_2 
Ha:  mu_1  > mu_2"

var.test( x = N1.Ga$ln_alns, 
          y = N3.Ga$ln_alns, 
          ratio = 1, alternative = "two.sided")


t.test(x = N1.Ga$ln_alns, 
       y = N3.Ga$ln_alns, 
       alternative = "greater",
       mu = 0, var.equal = FALSE)

"No rechazo H0"


#Histogramas de gasto apareados nivel socioeconomico 1 y 4 e inseguridad alimenticia 


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(N1.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 1", xlab="", ylim=c(0,900) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(N4.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 4", xlab="Gasto en comida no saludable", ylim=c(900,0) , las=1 , col="tomato3"  ,  breaks="Sturges")

"Planteamiento de hipótesis 1 y 4:

mu_1: El promedio del gasto en comida no saludable nivel 1
mu_2: El promedio del gasto en comida no saludable nivel 4


Ho: mu_1 <= mu_2 
Ha:  mu_1  > mu_2"

var.test( x = N1.Ga$ln_alns, 
          y = N4.Ga$ln_alns, 
          ratio = 1, alternative = "two.sided")


t.test(x = N1.Ga$ln_alns, 
       y = N4.Ga$ln_alns, 
       alternative = "greater",
       mu = 0, var.equal = FALSE)

"No rechazo H0"


#Histogramas de gasto apareados nivel socioeconomico 1 y 5 e inseguridad alimenticia 


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(N1.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 1", xlab="", ylim=c(0,900) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(N5.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Nivel 5", xlab="Gasto en comida no saludable", ylim=c(900,0) , las=1 , col="tomato3"  ,  breaks="Sturges")

"Planteamiento de hipótesis 1 y 5:

mu_1: El promedio del gasto en comida no saludable nivel 1
mu_2: El promedio del gasto en comida no saludable nivel 5


Ho: mu_1 <= mu_2 
Ha:  mu_1  > mu_2"

var.test( x = N1.Ga$ln_alns, 
          y = N5.Ga$ln_alns, 
          ratio = 1, alternative = "two.sided")


t.test(x = N1.Ga$ln_alns, 
       y = N5.Ga$ln_alns, 
       alternative = "greater",
       mu = 0, var.equal = FALSE)

"No rechazo H0"



# Gasto por niveles

boxplot(  N.Ga.clean$ln_alns ~ N.Ga.clean$nse5f  )

"Planteamiento de hipótesis:
Ho: Todos los promedios son iguales
Ha: Al menos uno es diferente."

anova <- aov(ln_alns ~ nse5f ,
             data= N.Ga.clean)

summary(anova)


"Reachazo H0"

#final- Estudio de gasto en comida no saludable en funcion de diferentes factores

"Determinar como influye la inseguridad alimentaria (alimenticia) 
, los recursos extras y el nivel socioeconomico al gasto en comida no saludable " 


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

summary(df)
str(df)


#gasto-inseguridad alimenticia


Ia.Ga<-select(df, IA, ln_als, ln_alns) #seleccionamos solo las variables interesadas
Ia.Ga.clean<-na.omit(Ia.Ga  )#limpiamos los datos- quedan 23201 obs

Ia.No.Ga<- filter(Ia.Ga.clean, IA=="No")#gasto - Seguridad alimenticia 6815
Ia.Si.Ga<- filter(Ia.Ga.clean, IA=="Si")#gasto - Inseguridad alimenticia 16386

par(mfrow = c(2, 2)) #El siguiente bloque de histogramas se pueden colocar en el orden especificado

Seguridad.ga.sal <-hist(Ia.No.Ga$ln_als,  breaks="Sturges", main="Gasto saludable con seguridad")

Seguridad.gas.nsal<-hist(Ia.No.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable con seguridad")

NSeguridad.ga.sal <-hist(Ia.Si.Ga$ln_als,  breaks="Sturges", main="Gasto saludable con inseguridad")

NSeguridad.gas.nsal<-hist(Ia.Si.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable con inseguridad")

par(mfrow = c(1, 1))

#Histogramas con media indicada aun tiene errores pero los estoy revisando

ggplot(Ia.No.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_als)),color="blue",linetype="dashed" ,size=1)


ggplot(Ia.No.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_alns)),color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_als)), color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_alns)), color="blue",linetype="dashed" ,size=1)

#Histogramas de gasto apareados seguridad e inseguridad alimenticia 

par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ia.No.Ga$ln_als, main="" , xlim=c(0,8), ylab="Seguridad", xlab="", ylim=c(0,2500) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ia.Si.Ga$ln_als, main="" , xlim=c(0,8), ylab="Inseguridad", xlab="Gasto en comida saludable", ylim=c(5500,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


"Planteamiento de hipótesis:

Ho: prom_total_intl_calls_churn1 == prom_total_intl_calls_churn2 
Ha: prom_total_intl_calls_churn1 =! prom_total_intl_calls_churn2"

var.test( x = Ia.No.Ga$ln_alns, 
         y = Ia.Si.Ga$ln_alns, 
         ratio = 1, alternative = "two.sided")

t.test(x = Ia.No.Ga$ln_alns, 
       y = Ia.Si.Ga$ln_alns, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE)



par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ia.No.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Seguridad", xlab="", ylim=c(0,1300) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ia.Si.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Inseguridad", xlab="Gasto en comida no saludable", ylim=c(3000,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


#Histogramas suavizados con media indicada y normal aproximada, aun tiene errores pero los estoy revisando

ggplot(Ia.No.Ga ,aes(x=ln_als))+
geom_histogram(aes( y=after_stat(density)),  colour="black", fill="white",bins=30)+  
geom_density(alpha=.2, fill="#FF6666")+ 
geom_vline(aes(xintercept=mean(ln_als)),color="blue",linetype="dashed" ,size=1)


ggplot(Ia.No.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white",bins=30)+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_alns)),color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white",bins=30)+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_als)), color="blue",linetype="dashed" ,size=1)

ggplot(Ia.Si.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white",bins=30)+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_alns)), color="blue",linetype="dashed" ,size=1)


#Gasto-Recursos extras

summary(Ex.Ga.clean)
str(Ex.Ga.clean)

Ex.Ga<-select(df, refin, ln_als, ln_alns) #seleccionamos solo las variables interesadas
Ex.Ga.clean<-na.omit(Ex.Ga  )#limpiamos los datos- quedan 23201 obs

Ex.No.Ga<- filter(Ex.Ga.clean, refin=="No")#gasto - No extras 18787
Ex.Si.Ga<- filter(Ex.Ga.clean, refin=="Si")#gasto - Extras 4414

par(mfrow = c(2, 2)) #El siguiente bloque de histogramas se pueden colocar en el orden especificado

NExtras.ga.sal <-hist(Ex.No.Ga$ln_als,  breaks="Sturges", main="Gasto saludable sin extras")

NExtras.gas.nsal<-hist(Ex.No.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable sin extras")

Extras.ga.sal <-hist(Ex.Si.Ga$ln_als,  breaks="Sturges", main="Gasto saludable con extras")

Extras.gas.nsal<-hist(Ex.Si.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable con extras")

par(mfrow = c(1, 1))

#Histogramas con media indicada aun tiene errores pero los estoy revisando

ggplot(Ex.No.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_als)),color="blue",linetype="dashed" ,size=1)


ggplot(Ex.No.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_alns)),color="blue",linetype="dashed" ,size=1)

ggplot(Ex.Si.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_als)), color="blue",linetype="dashed" ,size=1)

ggplot(Ex.Si.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(count)), colour="black", fill="white",bins=30)+ 
  geom_vline(aes(xintercept=mean(ln_alns)), color="blue",linetype="dashed" ,size=1)

#Histogramas de gasto apareados seguridad e inseguridad alimenticia 

par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ex.No.Ga$ln_als, main="" , xlim=c(0,9), ylab="No extras", xlab="", ylim=c(0,6500) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ex.Si.Ga$ln_als, main="" , xlim=c(0,9), ylab="Extras", xlab="Gasto en comida saludable", ylim=c(1500,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


par(mfrow=c(2,1))
par(mar=c(0,5,3,3))
hist(Ex.No.Ga$ln_alns, main="" , xlim=c(0,8), ylab="No extras", xlab="", ylim=c(0,3500) , xaxt="n", las=1 , col="slateblue1",  breaks="Sturges")
par(mar=c(5,5,0,3))
hist(Ex.Si.Ga$ln_alns, main="" , xlim=c(0,8), ylab="Extras", xlab="Gasto en comida no saludable", ylim=c(800,0) , las=1 , col="tomato3"  ,  breaks="Sturges")


#Histogramas suavizados con media indicada y normal aproximada, aun tiene errores pero los estoy revisando

ggplot(Ex.No.Ga ,aes(x=ln_als))+
  geom_histogram(aes( y=after_stat(density)),  colour="black", fill="white",bins=30)+  
  geom_density(alpha=.2, fill="#FF6666")+ 
  geom_vline(aes(xintercept=mean(ln_als)),color="blue",linetype="dashed" ,size=1)


ggplot(Ex.No.Ga ,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white",bins=30)+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_alns)),color="blue",linetype="dashed" ,size=1)

ggplot(Ex.Si.Ga ,aes(x=ln_als))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white",bins=30)+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_als)), color="blue",linetype="dashed" ,size=1)

ggplot(Ex.Si.Ga,aes(x=ln_alns))+
  geom_histogram( aes( y=after_stat(density)), colour="black", fill="white",bins=30)+ 
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(ln_alns)), color="blue",linetype="dashed" ,size=1)


#Gasto-Nivel socioeconomico 

summary(N.Ga.clean)

str(N5.Ga)

N.Ga<-select(df, nse5f, ln_als, ln_alns) #seleccionamos solo las variables interesadas
N.Ga.clean<-na.omit(N.Ga  )#limpiamos los datos- quedan 23201 obs

N1.Ga<- filter(N.Ga.clean, nse5f==1)#gasto - N1 3935
N2.Ga<- filter(N.Ga.clean, nse5f==2)#gasto - N2 4419
N3.Ga<- filter(N.Ga.clean, nse5f==3)#gasto - N3 4748
N4.Ga<- filter(N.Ga.clean, nse5f==4)#gasto - N4 5007
N5.Ga<- filter(N.Ga.clean, nse5f==5)#gasto - N5 5092


par(mfrow = c(1, 2)) 

N1.Ga.s <-hist(N1.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N1")

N1.Ga.ns <-hist(N1.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N1")

par(mfrow = c(1, 1))


"Planteamiento de hipótesis:

Ho: prom_total_intl_calls_churn1 == prom_total_intl_calls_churn2 
Ha: prom_total_intl_calls_churn1 =! prom_total_intl_calls_churn2"

var.test(df[df$churn == 1, "total_intl_calls"], 
         df[df$churn == 0, "total_intl_calls"], 
         ratio = 1, alternative = "two.sided")

t.test(x = df[df$churn == 1, "customer_service_calls"], 
       y = df[df$churn == 0, "customer_service_calls"],
       alternative = "two.sided",
       mu = 0, var.equal = TRUE)



par(mfrow = c(1, 2)) 

N2.Ga.s <-hist(N2.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N2")

N2.Ga.ns <-hist(N2.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N2")

par(mfrow = c(1, 1))




par(mfrow = c(1, 2)) 

N3.Ga.s <-hist(N3.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N3")

N3.Ga.ns <-hist(N3.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N3")

par(mfrow = c(1, 1))




par(mfrow = c(1, 2)) 

N4.Ga.s <-hist(N4.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N4")

N4.Ga.ns <-hist(N4.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N4")

par(mfrow = c(1, 1))


par(mfrow = c(1, 2)) 

N5.Ga.s <-hist(N5.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N5")

N5.Ga.ns <-hist(N5.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N5")

par(mfrow = c(1, 1))




par(mfrow = c(2, 3)) 
hist(N1.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N1")
hist(N2.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N2")
hist(N3.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N3")
hist(N4.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N4")
hist(N5.Ga$ln_als,  breaks="Sturges", main="Gasto saludable N5")
par(mfrow = c(1, 1))



par(mfrow = c(2, 3)) 
hist(N1.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N1")
hist(N2.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N2")
hist(N3.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N3")
hist(N4.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N4")
hist(N5.Ga$ln_alns,  breaks="Sturges", main="Gasto no saludable N5")
par(mfrow = c(1, 1))



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
  labs(title = "Histograma Mediciones por Categoría", 
       x = "Logaritmo natural alimentos saludables (LNALS)",
       y = "Frequencia") + 
  theme_classic()

ggplot(df.clean, aes(x=ln_alns, color=IA)) +
  geom_histogram(bins = 16, fill="White") + 
  labs(title = "Histograma Mediciones por Categoría", 
       x = "Logaritmo natural alimentos no saludables (LNALNS)",
       y = "Frequencia") + 
  theme_classic()

#Scatterplot LNALS vs LNALNS por IA y NSE
ggplot(df.clean, aes(x = ln_als, y=ln_alns, color =IA, shape=as.factor(nse5f))) + 
  geom_point( alpha=0.5) +
  labs(title = "Encuesta nacional de salud y nutrición 2012", 
       x = "LNALS",
       y = "LNALNS")


"3 Calcula probabilidades que nos permitan entender el problema en México"

##Probabilidad IA como función de Nivel socioeconómico del hogar


IASi1 <- df.clean%>%filter(IA == "Si", nse5f==1)
IANo1 <- df.clean%>%filter(IA == "No", nse5f==1)
IASi2 <- df.clean%>%filter(IA == "Si", nse5f==2)
IANo2 <- df.clean%>%filter(IA == "No", nse5f==2)
IASi3 <- df.clean%>%filter(IA == "Si", nse5f==3)
IANo3 <- df.clean%>%filter(IA == "No", nse5f==3)
IASi4 <- df.clean%>%filter(IA == "Si", nse5f==4)
IANo4 <- df.clean%>%filter(IA == "No", nse5f==4)
IASi5 <- df.clean%>%filter(IA == "Si", nse5f==5)
IANo5 <- df.clean%>%filter(IA == "No", nse5f==5)

#Probabilidad de tener IA en el Nivel Bajo
length(IASi1$IA)/(length(IASi1$IA)+length(IANo1$IA))
#Probabilidad de tener IA en el Nivel Medio Bajo
length(IASi2$IA)/(length(IASi2$IA)+length(IANo2$IA))
#Probabilidad de tener IA en el Nivel Medio
length(IASi3$IA)/(length(IASi3$IA)+length(IANo3$IA))
#Probabilidad de tener IA en el Nivel Medio Alto
length(IASi4$IA)/(length(IASi4$IA)+length(IANo4$IA))
#Probabilidad de tener IA en el Nivel Alto
length(IASi5$IA)/(length(IASi5$IA)+length(IASi5$IA))

##Probabilidad IA como función de la Zona Geográfica

IASiU <- df.clean%>%filter(IA == "Si", area=="Urbana")
IANoU <- df.clean%>%filter(IA == "No", area=="Urbana")
IASiR <- df.clean%>%filter(IA == "Si", area=="Rural")
IANoR <- df.clean%>%filter(IA == "No", area=="Rural")

#Probabilidad de tener IA en el ámbito urbano
length(IASiU$IA)/(length(IASiU$IA)+length(IANoU$IA))
#Probabilidad de tener IA en el ámbito rural
length(IASiR$IA)/(length(IASiR$IA)+length(IANoR$IA))

##Probabilidad IA como función de los recursos financieros

IASiSi <- df.clean%>%filter(IA == "Si", refin=="Si")
IANoSi <- df.clean%>%filter(IA == "No", refin=="Si")
IASiNo <- df.clean%>%filter(IA == "Si", refin=="No")
IANoNo <- df.clean%>%filter(IA == "No", refin=="No")

#Probabilidad de tener IA con recursos financieros adicionales
length(IASiSi$IA)/(length(IASiSi$IA)+length(IANoSi$IA))
#Probabilidad de tener IA sin recursos financieros adicionales
length(IASiNo$IA)/(length(IASiNo$IA)+length(IANoNo$IA))


"4 Plantea hipótesis estadísticas y concluye sobre ellas para entender el
problema en México"




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



"6 Escribe tu análisis en un archivo README.MD y tu código en un script de R y
publica ambos en un repositorio de Github."
