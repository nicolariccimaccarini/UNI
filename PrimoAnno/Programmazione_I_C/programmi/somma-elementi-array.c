#include <stdio.h>
#define DIM 5

int main()
{
     int a[DIM], i, s=0;

     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &a[i]);
          s+=a[i];
     }

     printf ("%d\n", s);

     return 0;
}