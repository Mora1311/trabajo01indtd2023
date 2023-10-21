rm(list=ls())
library(kableExtra)
library(tidyverse)
source("teoriadecision_funciones_incertidumbre.R")

#Aplicar los criterios de decisión bajo incertidumbre a una matriz de datos 
#generados aleatoriamente, considerandolos primero beneficios y luego costos:

set.seed(778)
datos=runif(8*5)
datos
tabla1=crea.tablaX(c(datos), 8, 5)
tabla1

#aplico los criterios considerando los datos beneficios

#criterio de Wald o pesimista

m.wald=criterio.Wald(tabla1, T)
m.wald
#por el criterio de wald la que mas beneficios nos generaria es la alternativa 4

#criterio optimista

m.opt=criterio.Optimista(tabla1, T) 
names(m.opt$AlternativaOptima)
#en este caso la alternativa 1 genera mas beneficios

#criterio Hurwicz

m.hur=criterio.Hurwicz(tabla1, alfa=0.4, T)
m.hur
#con Hurwicz la alternativa mas favorable es la 4

dibuja.criterio.Hurwicz(tabla1, T)
dibuja.criterio.Hurwicz_Intervalos(tabla1, T, T)

#criterio savage

m.sav=criterio.Savage(tabla1)
m.sav
#con savage escogeriamos la quinta alternativa

#criterio Laplace

m.lap=criterio.Laplace(tabla1)
m.lap
#al igual que con el criterio de savage la quinta alternativa genera mas beneficios

#criterio punto ideal

m.pid=criterio.PuntoIdeal(tabla1)
m.pid
#como los criterios anteriores la alternativa 5 es la optima


