rm(list=ls())
xa = seq(-4,2,0.1);
fx <- function(x) ((3*sin(x)*cos(x))-(4*sin(x))+cos(x))
gx <- function(x) ((-sin(x))-(4*cos(x))+(3*cos(2*x)))


newton <- function(a,b,error) {
  
  puntoMedio = (a+b)/2;
  iteraciones = 0
  y = 1
  
  repeat{
    cc = fx(puntoMedio)/gx(puntoMedio)
    x = puntoMedio - cc
    dx = abs(cc)
    puntoMedio = x
    iteraciones = iteraciones+1
    y[[iteraciones]] =  x
    if(dx <= error ) break;
    
  }
  
  horizontal = c(0,1)
  vertical = c(-1,1)
  plot(xa,fx(xa),type="l",horizontal,vertical,col="red",xlab="t",ylab="f(t)")
  par(new=TRUE)
  plot(y,fx(y),type="p",horizontal,vertical,col="springgreen",xlab="t",ylab="f(t)")
  par(new=TRUE)
  plot(xa,fx(xa),type="l",horizontal,vertical, col="red",xlab="t",ylab="f(t)")
  par(new=TRUE)
  plot(y,fx(y),type="p",horizontal, vertical, col="springgreen",xlab="t",ylab="f(t)")
  abline(h=0,v=0,col="black")
  cat("Iteraciones = ", iteraciones, " t = ", x, "\n")
}

newton(0,1,10e-8)

