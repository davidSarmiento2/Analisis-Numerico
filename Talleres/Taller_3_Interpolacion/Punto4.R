rm(list=ls())
x = c(30,40,50,60,70,80)
x1 = c(35, 45,55,65,75) #Puntos Medios
y = c(35,48, 70,40,22)
z= c(35,83,153,193,215) #Frecuencia Acumulativa
inter = poly.calc(x1,y)
inter

evaluar = function (z)
{
  
  fx =  function (x) 2292.5 - 182.45*x + 5.213333*x^2 - 0.062*x^3 + 0.0002666667*x^4 
  
  resultado = fx(z)
  
}

res = evaluar (54)
print(res)


barylag (x1,z,54)
