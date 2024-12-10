#include <stdio.h>
#define DIM 10

float radice2(int y)
{
     float a, b;
     float m, d;

     if (y>1.0)
     {
          a=1.0;
          b=y;
     }

     else 
     {
          a=y;
          b=1.0;
     }

     do
     {
          m=(a+b)/2;

          if (m*m<y)
          {
               a=m;
          }

          else 
          {
               b=m;
          }

          d=m*m-y;

          if (d<0)
          {
               d=-d;
          }
     }
     while (d>1e-5);

     return m; 
}

int main(void)
{
     float b[DIM];
     int dl=0;
     float x;
     int i;

     for (i=0; i<DIM; i++)
     {
          scanf ("%d", &b[i]);
          if (x>0.0)
          {
               b[dl]=radice2(x);
               dl++;
          }
     }

     for (i=0; i<dl; i++)
     {
          printf ("%d\n", b[i]);
     }

     return 0;
}