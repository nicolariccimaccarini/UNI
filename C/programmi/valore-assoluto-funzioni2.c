#include <stdio.h>

int assoluto(int n)
{
     if (n<0)
     {
          n=-n;
     }

     return n;
}

int main()
{
     int n;
     
     printf ("Inserisci un numero intero:\n");
     scanf("%d", &n);
     
     printf ("|%d| = %d\n", n, assoluto(n));
}
     