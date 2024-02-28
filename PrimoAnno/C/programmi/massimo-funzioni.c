#include <stdio.h>

int max(int a, int b)
{
     if (a>b)
     {
          return a;
     }

     else 
     {
          return b;
     }
}

int main ()
{
     int a, b;

     printf ("Inserisci due nuemeri interi:\n");
     scanf ("%d%d", &a, &b);

     printf ("Il massimo è: %d\n", max(a, b));

     return 0;
}