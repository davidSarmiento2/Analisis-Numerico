rm(list=ls())

Primera <-function(t) (2+cos(3*t))
Segunda <-function(t) (2-(exp(1)^t))


fx = function(x) (cos(3*x)+(exp(1)^x))


biseccion = function(a,b,error) {
  # Grafico
  x = seq(-4,3,0.1)
  plot(x,fx(x),type="l",col="blue")
  abline(h=0,v=0,col="black")
  
  
  puntoMedio = 0
  PM2 = 0
  iterRealizadas = 0
  resultado = fx(b)
  y = 1 
  
  while(error < abs(resultado)){
    iterRealizadas = iterRealizadas+ 1
    
    if((fx(a)*fx(b))<0){
      puntoMedio = (b+a)/2
      PM2 = fx(puntoMedio)
      
      if(abs(PM2) < error){
        break
      }else{
        if((fx(a)*PM2)>0){
          a = puntoMedio
        }else{
          b = puntoMedio
        }
      }
      y[[iterRealizadas]] <- puntoMedio
      
    }else{
      cat("El intervalo [", a, ",",b,"] no es valido\n")
      break
    }
  }
  horizontal = c(-1,3)
  vertical = c(-1,4)
  plot(y,Primera(y),type="p",horizontal,vertical,col="black",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(y,Segunda(y),type="p",horizontal,vertical,col="springgreen",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Primera(x),type="l",horizontal,vertical, col="red",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Segunda(x),type="l",horizontal,vertical, col="darkblue",xlab="t",ylab="r(t)")
  title(main="Gráfica Intersecciones Encontradas")
  
  
  cat("Iteraciones =",iterRealizadas,"Resultado = ",puntoMedio,"\n")
}

biseccion(-1,3,10e-8)


