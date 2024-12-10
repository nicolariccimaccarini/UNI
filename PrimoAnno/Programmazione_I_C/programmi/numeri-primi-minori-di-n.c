#include <stdio.h>

int main()
{
   int numero, n, i, numero_divisori_di_n;

   printf ("Inserisci un numero intero:\n");
   scanf ("%d", &numero);

   for (n = 2; n <= numero; n++)
   {
        numero_divisori_di_n = 0;

        for(i = 2; i < n; i++) 
        {
            if (n%i==0)
            {
                numero_divisori_di_n++;
            }
            
            
            
        }    
        
        if (numero_divisori_di_n == 0)
            {
                printf ("%d\n", n);
            }
   }
}