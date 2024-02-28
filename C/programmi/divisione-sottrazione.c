#include <stdio.h>

int main()
{
    int m, n, q = 0;

    printf ("Inserisci due numeri interi:\n");
    scanf ("%d%d", &m, &n);

    while ((m - n) >= 0)
    {
        m = m - n;
        q++;
    }
    
    printf ("Quoziente: %d, Resto: %d\n", q, m);

}