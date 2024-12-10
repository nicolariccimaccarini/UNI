#include <stdio.h>

int main()
{
     int a[5];
     int p=1, i;

     for (i=0; i<5; i++)
     {
          scanf ("%d", &a[i]);
          p*=a[i];
     }

     printf ("%d x %d x %d x %d x %d = %d\n", a[0], a[1], a[2], a[3], a[4], p);

     return 0; 
}