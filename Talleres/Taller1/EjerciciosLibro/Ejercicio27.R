# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

##Código para grafica polar
#-0.2803467164  y  1.63852842
#install.packages("pracma")
#install.packages("spuRs")
library(spuRs)
library(pracma)
options(digits=10)
f = function(x) (3*(sin(x))^3-1)-(4*sin(x)*cos(x))
respuestaBiseccion = bisection(f, 0, 2, 10^-5)
respuestaNewton = newtonRaphson(f, 1.5,NULL, 500 ,10^-5)

graficarPuntoPolar1 <- function(angulo, f, color){
  r <- f(angulo)
  y <- r*sin(angulo)
  x <- r*cos(angulo)
  points(x,y,pch=23,cex=1, col=color)
}
graficarPuntoPolar2 <- function(angulo, f, color){
  r <- f(angulo)
  y <- r*sin(angulo)
  x <- r*cos(angulo)
  points(x,y,pch=24,cex=1, col=color)
}


dim <- seq(0, pi/2, by=pi/3000) 
x =(3*(sin(dim))^3-1)
y = (4*sin(dim)*cos(dim))
polar(dim, x, "l", "blue")
polar(dim, y, "l", "green", add=TRUE)
graficarPuntoPolar1(respuestaBiseccion, f, "red")
graficarPuntoPolar2(respuestaNewton$root, f, "yellow")