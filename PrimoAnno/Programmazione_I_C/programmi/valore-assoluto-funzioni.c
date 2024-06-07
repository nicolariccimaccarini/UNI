#include <stdio.h>

int assoluto(int n)
{
     int risultato;
     
     if (n>0)
     {
         risultato=n;
     }

     else
     {
          risultato=-n;
     }

     return risultato;
}

int main()
{
     int n;
     
     printf ("Inserisci un numero intero:\n");
     scanf("%d", &n);
     
     printf ("|%d| = %d\n", n, assoluto(n));
}
     

     
