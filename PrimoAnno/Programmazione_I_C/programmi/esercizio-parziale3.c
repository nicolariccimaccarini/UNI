#include <stdio.h>
#define DIM 100

int cubo(int n)
{
     int s=0, i, j;

     for (i=1; i<=n; i++)
     {
          for (j=0; j<n; j++)
          {
               s+=n;
          }
     }

     return s;
}

int main()
{
     int a[DIM];
     int M;
     int i;

     printf ("Inserisci un numero <=100\n");
     scanf ("%d", &M);

     for (i=1; i<=M; i++)
     {
          a[i]=cubo(i);

          printf (" %d ", a[i]);
     }

     printf ("\n");
     return 0;
}