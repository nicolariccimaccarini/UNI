#include <stdio.h>

float potenza(float base, int esp)
{
    int cont;
    float prod;

    if (esp>=0)
    {
          prod=1.0;

          for (cont=0; cont<esp; cont++)
          {
               prod*=base; //è l'equivalente di prod=prod*base
          }
          return prod;
     }

     else 
     {
          esp=-esp;
          prod=1.0;

          for (cont=0; cont<esp; cont++)
          {
               prod*=base;
          }
          return 1.0/prod;
     }
}

int main()
{
    float b; //b=base
    int e; //e=esp

    printf ("Inserisci la base(reale) e l'esponente(intero con segno):\n");
    scanf ("%f%d", &b, &e);

    printf ("%f^%d=%f\n", b, e, potenza(b, e));

    return 0;
}
