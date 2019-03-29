rm(list=ls())
library(Matrix)
library(PolynomF)



evaluar = function ()
{
  i= 0
  
  fx = function(x) exp(x)  
  
  while(i < 1)
  {
    res = fx(i)
    
    cat("I:", i ,"   Resultado:", res, "\n" )
    points(i,res, pch=19,  col = "blue",xlab="X", ylab="Y")
    i = i +0.1
    
  }
  
  
}

evaluar()


x = c(0,0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9,1)
y = exp(x)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Interpolación")
inter=poly.calc(x,y)
inter
curve(inter,add=T,from =0,to =1)

barylag(x,y,0.5)
