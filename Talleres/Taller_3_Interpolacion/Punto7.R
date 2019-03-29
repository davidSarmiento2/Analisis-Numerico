require(PolynomF)
require(pracma)
rm(list=ls())

ft <- function(x)
{
  y0 = c()
  it = 1
  while (it <= length(x))
  {
    y0[it] = tan(x[it])
    it = it + 1
  }
  return(y0)
}

x = seq(-pi/2, pi/2,length = 20 )
y = ft(x)

polinomio = poly.calc(x,y)
polinomio


x_tan = seq(-pi/2,pi/2,length = 20)
y_tan = tan(x_tan)

plot(x_tan,y_tan,type="l",xlim=c(-2,2),ylim=c(-10,10))
par(new=TRUE)

plot(x,y, pch = 19, cex=1.5, col= "blue",xlim=c(-2,2),ylim=c(-10,10))
par(new=TRUE)
