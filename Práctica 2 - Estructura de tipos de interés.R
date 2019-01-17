########## PRÁCTICA 2 - Análisis de los Componentes Principales: análisis factorial ##########

#Objetivo 1: predecir el valor del bono a 10 años.
#Objetivo2: averiguar si tiene sentido realizar un análisis de los componentes principales.
#Objetivo 3: justificar con cuántos componentes como mínimo se podría explicar la estructura subyacente.
#Objetivo 4: explicar si tiene sentido llevar a cabo una rotación de las variables subyacentes.


#En primer lugar y para poder conseguir todos los objetivos, es necesario cargar los datos con los que vamos a trabajar y las librerías que vamos a utilizar.
library(FactoMineR)
library(factoextra)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

datos_bonos <- read.csv2("ACPTIUSD.csv")
datos_bonos

#Como se puede apreciar, a partir de la observación 784, no hay valores para la rentabilidad de los depósitos a 1 mes, por lo que gracias a "https://fred.stlouisfed.org/series/USD1MTD156N" obtenemos los datos oficiales.
datos_bonosOF <- read.csv2("ACPTIUSDOK.csv", sep=";", dec=".", header=TRUE)
datos_bonosOF

#Para poder trabajar correctamente, eliminaremos las columnas 1 (fechas) y la 11 (valor del bono a 10 años, pues es la que queremos predecir).
#Utilizaremos las primeras 949 observaciones para conseguir todos los objetivos, menos para predecir el comportamiento del bono a 10 años, que utilizaremos las suplementarias.
datos_bonosOK <- datos_bonosOF[1:949, c(-1, -11)]
datos_bonosOK
datos_bonosSUPLEM <- datos_bonosOF[950:978, -1] #Eliminamos la primera columna pero no la última, para poder comparar la predicción.
datos_bonosSUPLEM


##Ya tenemos todos los datos preparados para trabajar. Ahora realizaremos distintos análisis para ver si podemos levar a cabo un Análisis de los Componentes Principales.
#Cargamos la siguiente librería para poder trabajar correctamente con los análisis estadísticos.

library(psych)

#En primer lugar vamos a obtener las Matrices de Correlaciones de las primeras 949 obs. y de las siguientes 28.
cor_datosbonosOK <- cor(datos_bonosOK)
cor_datosbonosOK
cor_datosbonosSUPLEM <- cor(datos_bonosSUPLEM)

#Ahora se calcula el determinante de las anteriores matrices de correlación. A más pequeño, más colinealidad hay entre las diferentes variables.
det(cor_datosbonosOK)
det(cor_datosbonosSUPLEM)

#Lo siguiente a analizar es el Test de Cortest Barlet para obtener el P Value (coeficiente de significación). Sirve para conocer "cuánto se parece nuestra matriz a la matriz de identidad".
cortest.bartlett(cor_datosbonosOK, n = length(datos_bonosOK))
cortest.bartlett(cor_datosbonosSUPLEM, n = length(datos_bonosSUPLEM))

#El último test a realizar es el Test KMO. Como observamos, para cada variable obtenemos un valor >0.8.
KMO(cor_datosbonosOK)
KMO(cor_datosbonosSUPLEM)


####Conclusión: debido a que las correlaciones parciales son muy altas, el determinante es un número muy bajo. Además, el Test de Cortest Barlet nos da un coeficiente de significación muy bajo y el KMO nos da valores por encima de 0.8
####Esto significa que las variables tienen mucha relación entre sí, por tanto, a la hora de realizar un Análisis de los Componentes Principales no se perdería mucha información reduciendo variables. Pasamos a realizar el ACP:

#Como se observa, en el gráfico del ACP de las primeras 949 observaciones, la primera dimensión explica un 81% de la varianza acumulada y la segunda un 18%

acpOK <- PCA(datos_bonosOK, graph=T)
acpOK

acpSUPLEM <- PCA(datos_bonosSUPLEM, graph=T)
acpSUPLEM

#Ahora se justificará el uso de estas dos dimensiones analizando el ACP con los autovectores y los cos^2.
acpOK$eig #Según la regla de Kaiser, nos quedaremos con aquellas dimensiones cuyos autovalores son >1. Solo hay dos componentes (DIM 1 y DIM2) y entre estos explican un 98.27% de la varianza acumulada.
acpOK$var$cos2 #En este estudio se comprueba numéricamente qué porcentaje de cada variable está explicada en cada dimensión. Se 

fviz_eig(acpOK, choice="variance", axes= 1:2)
fviz_cos2(acpOK, choice="var", axes= 1:2)

#Para continuar, vamos a realizar un análisis factorial para explicar si tiene sentido llevar a cabo una rotación de las variables subyacentes

factanal(datos_bonosOK, rotation = "varimax", factors = 2)

#Por último, se va a realizar una predicción del valor del bono a 10 años.
datos_bonosOF_sin10<-datos_bonosOF[950:978, c(-1,-11)]

variables_res <- lm(IRS.10Y ~ DEPO.1M + DEPO.3M + DEPO.6M + DEPO.12M + IRS.2Y + IRS.3Y + IRS.4Y + IRS.5Y +IRS.7Y, data=datos_bonosOF)

Prediccion<-predict(variables_res, datos_bonosOF_sin10)
IRS.10Y <- datos_bonosSUPLEM$IRS.10Y

compara_prediccion <- as.data.frame(cbind(IRS.10Y, Prediccion))
View(compara_prediccion)

#Se observa en la tabla "compara_prediccion" que, tras realizar una prediccion del interés del bono a 10 años con los datos suplementarios y sin tener en cuenta los resultados que ya se conocían previamente,
#la comparación final es muy acertada. 





