#include <stdio.h>
#include <math.h>

float media(float a, float x)
{
     x=(x+a/x)/2.0;
     
     return x;
}

float radq(float a, float x)
{
     while (fabs(x*x-a)/a > 1e-5)
     {
          x=media(a, x);
     }

     return x;
}

int main(void)
{
     float a, x=1.0;

     printf ("Inserisci un numero intero\n");
     scanf ("%f", &a);

     if (a<=0)
     {
          printf ("La radice quadrata non esiste!\n");
          return 0;
     }

     else
     {
          printf ("La radice quadrata di %f è %f\n", a, radq(a, x));
     }

     return 0;
}