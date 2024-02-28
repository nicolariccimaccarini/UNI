#include <stdio.h>

int potenza(int base, int esp)
{
    int cont, prod=1;

    for (cont=0; cont<esp; cont++)
    {
        prod*=base; //è l'equivalente di prod=prod*base
    }

    return prod;
}

int main()
{
    int a, b;

    printf ("Inserisci la base e l'esponente:\n");
    scanf ("%d%d", &a, &b);

    printf ("%d^%d=%d\n", a, b, potenza(a, b));

    return 0;
}