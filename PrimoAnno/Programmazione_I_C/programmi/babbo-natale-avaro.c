#include <stdio.h>
#define DIM 50

int main()
{
    int n; //numero dei doni    
    int k; //numero dei bambini
    int i;
    int n_doni;
    int na[DIM]; //array dove ci andranno i numeri da n a 1
    int ka[DIM];

    printf ("Inserisci n e k:\n");
    scanf ("%d%d", &n, &k);

    
    i=n;
    while (n!=0)
    {
        na[i]=n;
        n--;
        i--;
    }

    return 0;
}

/*
n_doni=n-1;
while (n_doni==0)
{
    

    n_doni--;
}
*/
    