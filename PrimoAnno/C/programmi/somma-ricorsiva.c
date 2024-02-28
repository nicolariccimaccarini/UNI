#include <stdio.h>

int somma(int n)
{
    if (n==1)
    {
        return 1;
    }

    else return n+somma(n-1);
}

int main()
{
    int n;

    printf ("Inserisci un numero:\n");
    scanf ("%d", &n);

    printf ("%d\n", somma(n));

    return 0;
}