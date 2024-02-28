#include <stdio.h>
#define DIM 10

int main()
{
     int a[DIM], i, m=0;

     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &a[i]);
     }
     
     for (i=1; i<DIM; i++)
     {
          if (a[i]>a[m])
          {
               m=i;
          }
     }

     printf ("%d", m);

     return 0;
}