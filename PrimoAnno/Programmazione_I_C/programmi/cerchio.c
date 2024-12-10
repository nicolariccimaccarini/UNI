#include <stdio.h>
#include <math.h>

int power(int base, int esponente)
{
     int cont, prod=1;

    for (cont=0; cont<esponente; cont++)
    {
        prod*=base;
    }

    return prod;
}

int diametroCerchio(int raggio)
{
     int diametro;

     return diametro=raggio*2;
}

int perimetroCerchio(int raggio)
{
     int perimetro;

     return perimetro=(diametroCerchio(raggio))*3.14;
}

int areaCerchio(int raggio)
{
     int area;

     return area=(power(raggio, 2))*3.14;
}

int main(void)
{
     int raggio;

     printf ("Inserisci il raggio del cerchio:\n");
     scanf ("%d", &raggio);
    
     printf ("\n");

     printf ("Il diametro del cerchio è: %d\n", diametroCerchio(raggio)); 
     printf ("Il perimetro del cerchio è: %d\n", perimetroCerchio(raggio));
     printf ("L'area del cerchio è: %d\n", areaCerchio(raggio));

     return 0;
}