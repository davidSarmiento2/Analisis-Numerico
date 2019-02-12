rm(list=ls())
xa = seq(-5,5,0.1);
numero = readline("Ingrese el numero al cual desea conocer la raiz: ")
num = as.double(numero)



valorInicial = readline("Ingrese el Valor de n : ")
val= as.double (valorInicial)

fx <- function(x) (x^val-num)
gx <- function(x) (val*x^(val-1))

newton <- function(a,b,error) {
  
  puntoMedio = (a+b)/2;
  iteraciones = 0
  
  repeat{
    cc = fx(puntoMedio)/gx(puntoMedio)
    x = puntoMedio - cc
    dx = abs(cc)
    puntoMedio = x
    iteraciones = iteraciones+1
    if(dx <= error ) break;
    cat("x: ", x, " con: ", iteraciones ," Iteracion\n")
  }
  cat("Iteraciones = ", iteraciones, "\n")
  cat("Resultado = ", x, " es la raiz ", val, "de el numero", num, "\n")
}

matplot(xa,cbind(gx(xa),fx(xa)),type="l",col=c("blue","red"))
newton(0,1,10e-8)

