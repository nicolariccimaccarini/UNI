#include <stdio.h>
#define DIM 10

int main()
{
     int a[DIM];
     int i;
     int n;

     scanf ("%d", &n);

     for (i=0; i<DIM; i++)
     {
          n++;
          a[i]=n;
     }
     
     for (i=0; i<DIM; i++)
     {
          printf ("%d\n", a[i]);
     }

     return 0;
}