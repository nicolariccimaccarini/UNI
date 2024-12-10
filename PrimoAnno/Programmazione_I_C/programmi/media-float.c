#include <stdio.h>

int main()
{
    float n;
    float m=0;
    int contatore=0;

    while (n!=0)
    {
        printf ("Inserisci un numero:\n");
        scanf ("%f", &n);
        m=m+n;
        contatore++;
    }

    printf ("La media dei %d numeri inseriti è: %f\n", contatore, m);
}