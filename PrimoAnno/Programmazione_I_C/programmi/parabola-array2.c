#include <stdio.h>
#define DIM 21

int main(void)
{
     float a, b, c;
     float ascisse[DIM];
     float ordinate[DIM];
     int i;

     printf ("Inserisci i coefficienti:\n");
     scanf ("%f%f%f", &a, &b, &c);

     for (i=0; i<DIM; i++)
     {
          ascisse[i]=-1.0+0.1*i;
          ordinate[i]= a*ascisse[i]*ascisse[i] + b*ascisse[i] + c;
          printf ("(%f, %f)\n", ascisse[i], ordinate[i]);
     }

     return 0;
}