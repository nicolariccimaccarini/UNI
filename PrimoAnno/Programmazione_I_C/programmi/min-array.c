#include <stdio.h>
#define DIM 5

int valore_assoluto(int n)
{
    return n<0? -n : n;
}

int main() {

    int a[DIM];
    int i, n;
    int min; //accumularore del minimo
    
    printf ("Inserisci 5 numeri interi:\n");
    for (i=0; i<DIM; i++)
    {
        scanf ("%d", &a[i]);
    }

    n=0;
    for (i=1; i<DIM; i++)
    {
        n = valore_assoluto(a[i]) < valore_assoluto (a[n]) ? i : n;
    }

    printf ("%d\n", a[n]);
    return 0;
}