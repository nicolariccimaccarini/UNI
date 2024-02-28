#include <stdio.h>

int main()
{
    int m, n, i, prodotto = 0;

    printf ("Inserisci due numeri interi:\n");
    scanf ("%d%d", &m, &n);

    for (i = 1; i <= n; i++)
    {
        prodotto = prodotto + m;
    }
    
    printf ("Il prodotto è: %d\n", prodotto);
}