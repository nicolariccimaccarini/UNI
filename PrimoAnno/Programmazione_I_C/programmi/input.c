#include <stdio.h>

int main()
{
    int n;
    int i;
    int somma=0;
    int max, min;

    printf ("Inserisci un numero intero compreso fra 1 e 100:\n");
    scanf ("%d", &max);

    printf ("Inserisci un numero intero compreso fra 1 e 100:\n");
    scanf ("%d", &min);

    if (max<min)
        {
            min=max;
        }

    for (i=0; i<100; i++)
    {
        printf ("Inserisci un numero intero compreso fra 1 e 100:\n");
        scanf ("%d", &n);

        somma=somma+n;

        if (n>max)
        {
            max=n;
        }

        if (n<min && n!=0)
        {
            min=n;
        }
        
        if (n==0)
        {
            printf ("La somma è: %d\n", somma);
            printf ("Il numero massimo è: %d\n", max);
            printf ("Il numero minimo è: %d\n", min);

            return 0;
        }
    }
}