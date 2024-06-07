#include <stdio.h>

int main()
{
    int denominatore;

    printf ("Inserisci un numero intero:\n");
    scanf ("%d", &denominatore);

    if (denominatore != 0 && 10 / denominatore > 2)
    {
        printf ("Verificato\n");
    }
    
    else 
    {
        printf ("Non verificato\n");
    }
}