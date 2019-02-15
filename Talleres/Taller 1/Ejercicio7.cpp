#include <iostream>
#include <cmath>
#include <cstdlib>
#include <string>

double calcularRaiz( double Epsylon, double a, double b );

int main( int argc, char* argv[] )
{
  double Epsylon, a, b;
  return 0;
}

double calcularRaiz(double Epsylon, double a, double b)
{
  double d = ( b+a )/10, evaluar = a;
  double valor = funcionX( evaluar ), valorPrev;

  while( d < Epsylon )
  {
    evaluar = evaluar + d;
    valorPrev = valor;
    valor = funcionX(evaluar);
    if( ( valor * valorPrev )  < 0 )
    {
      evaluar = evaluar - d;
      d = d/10;
    }
  }
  return evaluar;
}
