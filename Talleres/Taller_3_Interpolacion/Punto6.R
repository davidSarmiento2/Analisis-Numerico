require(pracma)
ex <- function(x)
{
  y0 = 0
  it = 1
  while (it <= length(x))
  {
    y0[it] = exp(1)^x[it]
    it = it + 1
  }
  return(y0)
}


funcion <- function(x)
{
  y0 = 0
  it = 1
  while (it <= length(x))
  {
    y0[it] = 1/x[it]
    it = it + 1
  }
  return(y0)
}

tay_2 <- function(x)
{
  y0 = 1 + x+ (x^2)/2
  return(y0)
}

##Con la funcion de euler dada
x = c(-3,-2,-1,1,2,3)
y = ex(x)

##Polinomio resultante
inter = poly.calc(x,y)
inter

barylag (x,y,0)
tay_2(0)

##COn la segunda funcion
a = c(-1/2,-1/4,-1/8,1/16,1/8,1/4,1/2)
b = funcion(a)

##Polinomio Resultante
segundo = poly.calc(a,b)
segundo

barylag (a,b,0)
