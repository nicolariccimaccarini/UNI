#include <stdio.h>

int fattoriale (int n)
{
     int i;
     int fatt=1;

     for (i=1; i<=n; i++)
     {
         fatt*=i;
     }

     return fatt;
}

int main()
{
     int n;

     printf ("Inserisci un numero intero:\n");
     scanf ("%d", &n);

     printf ("%d! = %d\n", n, fattoriale(n));

     return 0;
}