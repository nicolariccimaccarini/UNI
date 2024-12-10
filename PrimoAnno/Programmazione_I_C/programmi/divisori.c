#include <stdio.h>

int main()
{
    int a, i;

    printf ("Inserisci un nuemero intero:\n");
    scanf ("%d", &a);

    for (i = 0; i <= a; i++)
    {
        if (a%i==0)
        {
            printf ("Il divisore di %d " "è: %d\n", a, i);
        }
        
    }
}