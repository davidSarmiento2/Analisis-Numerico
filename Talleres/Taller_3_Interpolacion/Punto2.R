##Analisis numerico 

library(Matrix)
library(PolynomF)

##Puntos 

rm(list=ls())

x=c( 100, 200, 300, 400, 500, 600)                                                                                                       
y=c(-160, -35, -4.2, 9.0, 16.9, 21.3 )     


Datosx= x[1:5];  Datosy=y[1:5]
inter = poly.calc(Datosx,Datosy)
inter
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Interpolación")
points(Datosx,Datosy, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(inter,add=T,col = "blue",lty=1, lwd=1)

evaluarPolinomio = function (x)
{
  
  fx = function(x) -520.1 + 5.406917*x - 0.02174708*x^2 + 3.955833e-05*x^3 - 2.679167e-08*x^4 

  resultado = fx(x)

}

res = evaluarPolinomio(450)
print(res)


Datosx= x[2:6];  Datosy=y[2:6]
barylag(Datosx, Datosy, 450)
