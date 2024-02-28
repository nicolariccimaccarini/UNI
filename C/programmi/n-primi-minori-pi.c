#include <stdio.h>

int primo(int n)
{
     int numero_divisori=0;
     int i;

     for (i=0; i<=n; i++)
     {
          if (n%i==0)
          {
               numero_divisori++;
          }
     }

     if (numero_divisori==2)
     {
          return 1;
     }

     else 
     {
          return 0;
     }
}

int PI(int n) //restituisce il numero di numeri primi minori uguali a m
{
     int m;
     int numero_primi=0; //inizializzo il contatore per i nuemri primi
     
     for (m=1; m<=n; m++)
     {
          if (primo(m))
          {
               numero_primi++;
          }
     }

     printf ("%d\n", numero_primi);

     return m;
}

int main(void)
{
     int n;

     printf ("Inserisci un numero intero:\n");
     scanf ("%d", &n);

     if (n>=1)
     {
          printf ("I numeri primi minori di %d sono: ", n);
          PI(n);

          return 0;
     }
     
     else
     {
          printf ("Inserire un numero >=1\n");
          return 0;
     }
}