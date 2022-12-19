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
library(rstudioapi)

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

#Dado que las variables area, refin y IA son booleano lo cambiamos al tipo indicado
df$area <- as.logical(df$area)
df$IA <- as.logical(df$IA)
df$refin <- as.logical(df$refin)

#Para fines prácticos consideramos que quienes no respondieron gastos en alimentos no saludables (NA) se considera valor 0
#Aquí también podríamos reemplazar por la media para no reducir drásticamente el tamaño de la muestra
# df$ln_alns[is.na(df$ln_alns)] = 0
# df$ln_als[is.na(df$ln_als)] = 0

# #Excluimos los NA de alimentos saludables y no saludables
df.clean <- df[complete.cases(df),]

"2 Realiza un análisis descriptivo de la información"
#Análisis descriptivo del dataframe limpio
dim(df.clean) #El dataframe tiene 23291 obs. en 8 variables
#nse5f, numpeho, añosedu son integer
#area, refin, IA es logical
#ln_als y ln_alns son numerics
str(df.clean) 
head(df.clean)
summary(df.clean) 
boxplot(df.clean)

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

"3 Calcula probabilidades que nos permitan entender el problema en México"


"4 Plantea hipótesis estadísticas y concluye sobre ellas para entender el
problema en México"


"5 Estima un modelo de regresión, lineal o logístico, para identificar los
determinantes de la inseguridad alimentaria en México"


"6 Escribe tu análisis en un archivo README.MD y tu código en un script de R y
publica ambos en un repositorio de Github."
myTerm <- terminalCreate()
terminalSend(myTerm, "echo Hello")
