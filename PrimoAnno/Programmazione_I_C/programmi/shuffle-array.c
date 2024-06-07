#include <stdio.h>
#define MAX 100

int main()
{
     int a[MAX];
     int numeroElementi;
     int i;
     int mid;
     int temp;

     printf ("Inserisci il nuemro di elementi:\n");
     scanf ("%d", &numeroElementi);

     if (numeroElementi%2!=0)
     {
          printf ("Il numero di elementi deve essere pari:\n");
          return -1;
     }

     for (i=0; i<numeroElementi; i++)
     {
          printf ("Inserire elemento %d: ", i);
          scanf ("%d", &a[i]);
     }

     //stampo array
     printf ("Prima:\n");
     for (i=0; i<numeroElementi; i++)
     {
          printf ("%d", a[i]);
     }

     //shuffle
     mid=numeroElementi/2;

     for (i=0; i<mid; i++)
     {
          temp=a[i];
          a[i]=a[mid+i];
          a[mid+i]=temp;
     }

     //stampo array
     printf ("Dopo:\n");

     for (i=0; i<numeroElementi; i++)
     {
          printf ("%d", a[i]);
     }
     printf ("\n");

     return 0;
}