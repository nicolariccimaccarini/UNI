#include <stdio.h>

int main(void)
{
    int altezza;
    int riga, i;

    printf ("Inserisci il numero di righe: ");
    scanf ("%d", &altezza);

    if (1<=altezza<=40)
    {
        for (riga=1; riga<=altezza; riga++)
        {
            for (i=1; i<=altezza-riga; i++)
            {
                printf (" ");
            }

            for (i=1; i<=riga*2 - 1; i++)
            {
                printf ("*");
            }

            printf ("\n");
        }

        return 0;
    }
    
    else {return 0;}
}