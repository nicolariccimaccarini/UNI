/* programma che calcola l'approssimazione della radice quadrata del numero
inserito in input utilizzando il metodo babilonese e douuble*/

#include <stdio.h>
#include <math.h>

int main()
{
    double a;
    double x=1.0;

    printf ("Inserisci un numero positivo:\n");
    scanf ("%lf", &a);

    while (fabs(x*x-a)/a > 1e-5)
    {
        x=(x+ a/x)/2.0;
    }

    printf ("La radice quadrata di %lf è %lf", a, x);
}