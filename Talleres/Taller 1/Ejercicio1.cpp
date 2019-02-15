#include <iostream>
#include <cmath>

using namespace std;

double horner(double [], int n, double x);
double eval(double p[], int n, double x);

int main()
{
    cout<<"Valor Indicado de Polinomios" << endl;
    double p1[5] = {2,0,-3,3,-4};
    double p2[6] = {7,6,-6,0,3,-4};
    double p3[7] = {-5,0,3,0,2,-4,0};
    int n1=5;
    int n2=6;
    int n3=7;
    double x1=-2;
    double x2=3;
    double x3=-1;
    cout << "Resultado primer Polinomio: " <<horner(p1,n1,x1) << endl;
    cout << "Resultado segundo Polinomio: " <<horner(p2,n2,x2) << endl;
    cout << "Resultado tercero Polinomio: " <<horner(p3,n3,x3) << endl;
    return 0;
}

double horner(double p[], int n, double x)
{
    double y = p[0];
    for(int i=1 ; i<n ; ++i)
    {
        y = x*y + p[i];
        cout<< i << endl;
    }
    return y;

}
