rm(list=ls())
xa = seq(-4,0.5,0.1);
Primera <-function(t) (2+cos(3*t))
Segunda <-function(t) (2-(exp(1)^t))

fx <- function(x) (cos(3*x)+(exp(1)^x))
gx <- function(x) ((-3*sin(3*x))+(exp(1)^x))


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
  
  horizontal = c(-1,0)
  vertical = c(-1,3)
  plot(y,Primera(y),type="p",horizontal,vertical,col="black",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(y,Segunda(y),type="p",horizontal,vertical,col="springgreen",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(xa,Primera(xa),type="l",horizontal,vertical, col="red",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(xa,Segunda(xa),type="l",horizontal, vertical, col="darkblue",xlab="t",ylab="r(t)")
  title(main="Gráfica Intersecciones ")
  
  cat("Iteraciones = ", iteraciones, " t = ", x, "\n")
}

newton(-1,1,10e-8)

