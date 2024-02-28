/* programma che calcola un'approssimazione della radice quadrata di
un numero reale a con il metodo babilonese*/

#include <stdio.h>
#include <math.h>

int main()
{
    float a, x=1.0;
    
    printf ("inserisci un numero:\n");
    scanf ("%f", &a); 

    while (fabsf(x * x - a) / a > 1e-5)
    {
        x=(x + a/x)/2.0;
    }

    printf ("La radice quadrata di %f è %f", a, x);
}