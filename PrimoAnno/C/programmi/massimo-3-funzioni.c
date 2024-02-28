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

int max3(int a, int b, int c)
{
     return max(max(a, b), c);
}

int main ()
{
     int a, b, c;

     printf ("Inserisci due nuemeri interi:\n");
     scanf ("%d%d%d", &a, &b, &c);

     printf ("Il massimo è: %d\n", max3(a, b, c));

     return 0;
}