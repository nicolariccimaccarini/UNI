#include <stdio.h>
#define dim 10

int precede(int a, int b)
{
     return a*a<b*b;
}

int main ()
{
     int a[dim];
     int DL;
     int dlo;
     int i, n;

     for (DL=0; DL<dim; DL++)
     {
          scanf ("%d", &n);

          if (n!=0)
          {
               a[DL]=n;
          }

          else
          {
               break;
          }
     }

     for (dlo=0; dlo<DL; dlo++)
     {
          int j=dlo, m=a[dlo];

          while (j>0 && precede(m, a[j-1]))
          {
               a[j]=a[j-1];
          }

          a[j]=m;
     }

     for (i00; i<DL; i++)
     {
          printf ("%d ", a[i]);
     }
     printf ("\n");

     return 0;
}