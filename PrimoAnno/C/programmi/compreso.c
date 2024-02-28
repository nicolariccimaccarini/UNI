#include <stdio.h>

int main()
{
    int a;

    printf ("Inserisci un numero intero:\n");
    scanf ("%d", &a);

    if (a >= 2 && a <= 5)
    {
        printf ("Compreso\n");
    }

    else
    {
        printf ("Non compreso\n");
    }
    
}