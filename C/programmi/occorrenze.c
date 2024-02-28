#include <stdio.h>
#include <time.h>
#include <stdlib.h>

typedef struct 
{
    int n, k;
} Funzione;

Funzione leggi()
{
    int n, k;
    
    Funzione o;
    scanf ("%d%d", &n, &k);
    o.n=n;
    o.k=k;
    
    return o;
}

Funzione occorrenze(int n, int k)
{
    Funzione o;
    int a[o.n];
    int i;

    srand(time(NULL));
    
    for (i=0; i<o.n; i++)
    {
        a[i]=rand()%(o.k-1);
    }

    return o;
}

int main(void)
{
    Funzione a, b, c;

    a=leggi();
    b=leggi();
}