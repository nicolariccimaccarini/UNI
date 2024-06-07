#include <stdio.h>
#include <math.h>

float perimetro(float a, float b, float c)
{
     float p;

    return p=a+b+c;
}

float area(float a, float b, float c)
{
     float ar;
     float sp;

     sp=(perimetro(a, b, c))/2;

     return ar=sp*(sp-a)*(sp-b)*(sp-c);
}

int main()
{
     float a, b, c;

     printf ("Inserisci tre numeri:\n");
     scanf ("%f%f%f", &a, &b, &c);

     printf ("Il perimetro del triangolo è: %f\n", perimetro(a, b, c));

     printf ("L'area del triangolo è: %f\n", area(a, b, c));

     return 0;
}