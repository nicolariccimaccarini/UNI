#include <stdio.h>
#define DIM 5 //definisco la macro DIM

int main(void)
{
     int a[]={2, 4, 6, 1, 2};
     int i;

     for (i=0; i<DIM; i++)
     {
          printf ("%d\n", a[i]);
     }

     return 0;
}