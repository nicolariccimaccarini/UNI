#include <stdio.h>
#define dim 201

int bisestile (int n)
{
     if (n%400==0)
     {
          return 1;
     }

     else if (n%4==0 && n%100!=0)
     {
          return 1;
     }

     return 0;
}

int main()
{
     int a[dim];
     int i;
     int b[dim], dl=0;

     for (i=0; i<dim; i++)
     {
          a[i]=1900+i;

          if (bisestile(a[i]))
          {
               b[dl]=a[i];
               dl++;
          }
     }

     for (i=0; i<dl; i++)
     {
          printf ("%d\n", b[i]);
     }

     return 0;
}
