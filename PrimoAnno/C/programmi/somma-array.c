#include <stdio.h>
#define DIM 5

int main()
{
     int a[DIM];
     int i;
     int somma=0;

     printf ("Inserisci %d nuemri positivi:\n", DIM);
     
     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &a[i]);

          if (a[i]<0)
          {
               printf ("Reinserire il numero:\n");
               scanf ("%d", &a[i]);
          }

          somma+=a[i];
     }

     printf ("%d\n", somma);
}