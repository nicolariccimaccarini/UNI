#include <stdio.h>
#define DIM 5

int main()
{
     int a[DIM], i;
     int b[DIM];
     int dl=0;

     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &a[i]);
     }

     for (i=0; i<DIM; i++)
     {
          if (a[i]>0)
          {
               b[dl]=a[i];
               dl++;
          }
     }
}