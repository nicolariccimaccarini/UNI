#include <stdio.h>
#define DIM 10

int primo(int n)
{
     int i=2;
     int j=1;

     while (j!=0 && j<n)
     {
          j=n;

          if (j>0)
          {
               j-=1;
          }

          else
          {
               i++;
          }
     }

     if (j!=0)
     {
          return printf ("true");
     }

     else
     {
          return printf ("false");
     }
}

int main()
{
     int a[DIM];
     int i;
     int maggiore=0;

     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &a[i]);

          if (primo(a[i]) && a[i]>maggiore)
          {
               maggiore=a[i];
          }
     }

     printf ("%d", maggiore);

     return 0;
}