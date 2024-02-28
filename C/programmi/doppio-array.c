#include <stdio.h>
#define DIM 5

int main(void)
{
     int i;
     int a[DIM];
     int b[DIM];

     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &a[i]);
     }

     for (i=0; i<DIM; i++)
     {
          b[i]=2*a[i];
     }

     for (i=0; i<DIM; i++)
     {
          printf ("%d\n", b[i]);
     }

     return 0;
}