#include <stdio.h>

int main()
{
    int righe, colonne;
    int i, j;

    printf ("Inserisci il numero di righe desiderate:\n");
    scanf ("%d", &righe);
    printf ("Inserisci il numero di colonne desiderate:\n");
    scanf ("%d", &colonne);
    printf ("\n");

    for (i=1; i<=righe; i++)
    {
        for (j=1; j<=colonne; j++)
        {
            printf ("%4d", i*j);
        }

        printf ("\n");
    }

    return 0;
}