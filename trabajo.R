rm(list=ls())
library(kableExtra)
library(tidyverse)
source("teoriadecision_funciones_incertidumbre.R")

#Primer problema
#Aplicar los criterios de decisión bajo incertidumbre a una matriz de datos 
#generados aleatoriamente, considerandolos primero beneficios y luego costos:

set.seed(778)
datos=runif(8*5)
datos=round(datos,2)
datos
tabla1=crea.tablaX(c(datos), 8, 5)
tabla1

#aplico los criterios considerando los datos beneficios

#criterio de Wald o pesimista

m.wald=criterio.Wald(tabla1, T)
m.wald
#por el criterio de wald la que más beneficios nos generaría es la alternativa 4,
#siendo el valor optimo 0.4

#criterio optimista

m.opt=criterio.Optimista(tabla1, T) 
m.opt
#en este caso la alternativa 1 genera más beneficios, con 0.98 de valor optimo

#criterio Hurwicz

m.hur=criterio.Hurwicz(tabla1, alfa=0.4, T)
m.hur
#con Hurwicz la alternativa más favorable es la 4 y el valor optimo es 0.612

dibuja.criterio.Hurwicz(tabla1, T)
dibuja.criterio.Hurwicz_Intervalos(tabla1, T, T)

#criterio savage

m.sav=criterio.Savage(tabla1, T)
m.sav
#con savage escogeríamos la quinta alternativa, siendo el valor optimo de 0.41

#criterio Laplace

m.lap=criterio.Laplace(tabla1, T)
m.lap
#al igual que con el criterio de savage la quinta alternativa genera más beneficios,
#con 0.718 de valor optimo

#criterio punto ideal

m.pid=criterio.PuntoIdeal(tabla1, T)
m.pid
#como los criterios anteriores la alternativa 5 es la optima y su valor correspondiente es 0.532

#Con los beneficios, las alternativas más favorables  son la 5, la 4 y la 1. Siendo la 
#alternativa que más veces es favorable entre los distintos criterios la quinta.

#aplico los criterios considerando los datos costos

#criterio de Wald o pesimista

m.wald2=criterio.Wald(tabla1, F)
m.wald2
#por el criterio de wald la que más costos nos generaría es la alternativa 8,
#con un valor optimo de 0.7

#criterio optimista

m.opt2=criterio.Optimista(tabla1, F) 
m.opt2
#en este caso la alternativa 2 genera más costos y el valor optimo es 0.02

#criterio Hurwicz

m.hur2=criterio.Hurwicz(tabla1, alfa=0.4, F)
m.hur2
#con Hurwicz la alternativa más favorable es la 6, siendo su valor optimo 0.446

dibuja.criterio.Hurwicz(tabla1, F)
dibuja.criterio.Hurwicz_Intervalos(tabla1, F, T)

#criterio savage

m.sav2=criterio.Savage(tabla1, F)
m.sav2
#con savage escogeríamos la sexta alternativa con 0.6 de valor optimo

#criterio Laplace

m.lap2=criterio.Laplace(tabla1, F)
m.lap2
#al igual que con el criterio de savage la sexta alternativa genera más costos
#y su valor optimo es 0.358

#criterio punto ideal

m.pid2=criterio.PuntoIdeal(tabla1, F)
m.pid2
#como los criterios anteriores la alternativa 6 es la optima y el valor que le corresponde es 0.662

#En el caso de considerar los datos costos, las alternativas más favorables  son la 6, la 8 y la 2. 
#Siendo la alternativa que más veces es favorable entre los distintos criterios la sexta.

#Segundo problema
#Un fabricante de cojines estudia lanzar un nuevo modelo al mercado, debatiendose entre
#cuatro estampados distintos. Los estampados candidatos son: lisos, con plantas, 
#con frases y con figuras geometricas. Los beneficios esperados (expresados 
#en millones de euros) en el año siguiente al lanzamiento en función del tipo de 
#interés al consumo son:
#             E1    E2    E3    E4
#Lisos        30    26    13    19
#Plantas      25    17    10    28
#Frases       22    25    21    16
#Fig. Geom.   24    18    23    27
#¿Cuál sería la opción que recomendaría a la empresa?

tabla2=crea.tablaX(c(30, 26, 13, 19,
                     25, 17, 10, 28,
                     22, 25, 21, 16,
                     24, 18, 23, 27), 4, 4)
rownames(tabla2)=c("Lisos", "Plantas", "Frases", "Figuras Geométricas")
tabla2
criterio.Todos(tabla2, alfa=0.4, favorable = T)
#Si aplicasemos el criterio optimista la opción recomendable sería lanzar cojines lisos, sin embargo
#aplicando el resto de los criterios la empresa debería lanzar cojines con figuras geométricas
#en su estampado.
#Por lo tanto, la recomendación es vender cojines con estampados de figuras geométricas, pues
#estos generarían más beneficios