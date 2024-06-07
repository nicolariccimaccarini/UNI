#include <stdio.h>

int sommaDivisori()
{
     int i;
     int n;
     int somma_divisori=0;

     for (i=1; i<=100; i++)
     {
          for (n=1; n<=i/2; n++)
          {
               if (i%n==0)
               {
                   return somma_divisori+=n;
               }
          }

          if (somma_divisori==i)
          {
               return printf ("%d\n", i);
          }
     }

     return ;
}

int main(void)
{
     printf ("I numeri perfetti compresi tra 1 e 100 sono:\n");

     printf ("%d\n", sommaDivisori());

     return 0;
}
